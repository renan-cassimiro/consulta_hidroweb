# =============================================================================
# ANÁLISE HIDROLÓGICA DE TENDÊNCIAS — BACIA AMAZÔNICA
# =============================================================================
# 
# Autor:   Renan Cassimiro Brito
# Data:    2026-05-13
# 
# Descrição:
#   Pipeline principal de análise hidrológica. Obtém dados de vazão de estações
#   fluviométricas via API HidroWeb (ANA), aplica testes estatísticos de
#   tendência (Mann-Kendall, Sen, Pettitt) e gera relatório PDF com visualizações.
#
# Fluxo:
#   1. Leitura da área de estudo e estações pré-selecionadas
#   2. Download do inventário e dados via {hydrobr}
#   3. Seleção de estações por critérios de qualidade
#   4. Análise estatística por estação (analisar_estacao)
#   5. Geração de tabela resumo e gráficos (gerar_graficos)
#   6. Renderização do relatório PDF via RMarkdown
#
# Inputs:
#   resources/hybas_lake_sa_lev03_v1c_bacia_amazonica.gpkg  — polígono da bacia
#   resources/dourada_geoft_estacao_hidrometeorologica_filtradas.gpkg — estações
#   resources/ne_10m_rivers_lake_centerlines_amazonia.gpkg  — hidrografia
#   resources/AMACRO/AMACRO.shp                             — região AMACRO
#
# Outputs:
#   output/refactor_test/data/          — objetos RDS intermediários
#   output/refactor_test/images/        — gráficos PNG (via ggsave, comentados)
#   output/refactor_test/report/        — relatório PDF final
#   output/refactor_test/resumo_tendencias_mensal.csv
#
# Dependências:
#   CRAN: fs, here, Kendall, lubridate, openxlsx, patchwork, scales,
#         sf, tidyverse, trend, zyp, zoo
#   GitHub: hydroversebr/hydrobr
#     # install_github("hydroversebr/hydrobr", build_vignettes = FALSE)
#
# TODO: Melhorar gráfico de apresentação inicial (incluir camada de rios e estados)
# TODO: Cruzar códigos das estações com camada de rios/bacias para filtragem
#       geográfica antes de chamar as funções do hydrobr
# TODO: Aplicar filtro antes do download (atualmente feito após, para teste offline)
# TODO: Aplicar análises temporais disponíveis no pacote hydrobr
# TODO: Interface Shiny para seleção interativa de estações e bacias
# =============================================================================

# if (!require(devtools)) install.packages("devtools")
# library(devtools)
# install_github("hydroversebr/hydrobr", build_vignettes = FALSE)

library(fs)
library(here)
library(hydrobr)
library(Kendall)
library(lubridate)
library(openxlsx)
library(patchwork)
library(scales)
library(sf)
library(tidyverse)
library(trend)
library(zyp)
library(zoo)

source(here("functions/functions.R"))

# -----------------------------------------------------------------------------
# Diretórios do projeto (todos relativos à raiz via {here})
# -----------------------------------------------------------------------------
RESOURCES_DIR <- here("resources")
OUTPUT_DIR    <- here("output", "refactor_test")
REPORT_DIR    <- path(OUTPUT_DIR, "report")
IMAGE_DIR     <- path(OUTPUT_DIR, "images")
DATA_DIR      <- path(OUTPUT_DIR, "data")

# -----------------------------------------------------------------------------
# 1. ÁREA DE ESTUDO E ESTAÇÕES
# -----------------------------------------------------------------------------

area_estudo <- st_read(path(RESOURCES_DIR,
                            "hybas_lake_sa_lev03_v1c_bacia_amazonica.gpkg"))

# Defina estacoes_filtradas <- NULL para usar todas as estações do inventário
estacoes_filtradas <- st_read(path(RESOURCES_DIR,
                                   "dourada_geoft_estacao_hidrometeorologica_filtradas.gpkg"))

# -----------------------------------------------------------------------------
# 2. INVENTÁRIO DE ESTAÇÕES FLUVIOMÉTRICAS
# -----------------------------------------------------------------------------

# Consulta o inventário da ANA para estações do tipo fluviométrico (flu)
# dentro do polígono da área de estudo
inventario <- inventory(stationType = "flu", as_sf = TRUE, aoi = area_estudo)

# Aplica filtro por estações pré-selecionadas (quando fornecidas)
if (!is.null(estacoes_filtradas)) {
  inventario <- inventario |>
    filter(station_code %in% estacoes_filtradas$CodigoEstacao)
}

saveRDS(inventario, file = path(DATA_DIR, "inventario"))

# Mapa rápido de conferência do inventário
ggplot() +
  geom_sf(data = inventario) +
  geom_sf(data = area_estudo, fill = NA, color = "red") +
  theme_classic()

# -----------------------------------------------------------------------------
# 3. DOWNLOAD E ORGANIZAÇÃO DOS DADOS
# -----------------------------------------------------------------------------

# Baixa as séries históricas de todas as estações do inventário
dados_inventario <- stationsData(inventoryResult = inventario)
saveRDS(dados_inventario, file = path(DATA_DIR, "dados_inventario"))

# Organiza os dados no formato padrão do {hydrobr}
dados_inventario_organizado <- organize(dados_inventario)
saveRDS(dados_inventario_organizado, file = path(DATA_DIR, "dados_inventario_organizado"))

# -----------------------------------------------------------------------------
# 4. SELEÇÃO DE ESTAÇÕES POR CRITÉRIOS DE QUALIDADE
# -----------------------------------------------------------------------------
# Critérios aplicados:
#   mode        = "yearly"  — agrega por ano hidrológico
#   maxMissing  = 5         — máximo de 5% de dados faltantes por ano
#   minYears    = 10        — mínimo de 10 anos com dados válidos
#   month       = 1         — mês de início do ano hidrológico (janeiro)
#   iniYear/finYear         — janela temporal de interesse
#   consistedOnly = FALSE   — inclui dados não consistidos
# -----------------------------------------------------------------------------

dadosestacoes_selecionadas <- selectStations(
  organizeResult = dados_inventario_organizado,
  mode           = "yearly",
  maxMissing     = 5,
  minYears       = 10,
  month          = 1,
  iniYear        = 1900,
  finYear        = 2025,
  consistedOnly  = FALSE
)

saveRDS(dadosestacoes_selecionadas, file = path(DATA_DIR, "dadosestacoes_selecionadas"))

# -----------------------------------------------------------------------------
# 5. ANÁLISE ESTATÍSTICA POR ESTAÇÃO
# -----------------------------------------------------------------------------
# analisar_estacao() é definida em functions/functions.R
# Retorna lista com: Mann-Kendall, Yue-Pilon, slope de Sen, Pettitt,
# decomposição STL, anomalias e extremos para cada estação.

resultados <- map2(
  dadosestacoes_selecionadas$series,
  names(dadosestacoes_selecionadas$series),
  analisar_estacao
)

# -----------------------------------------------------------------------------
# 6. TABELA RESUMO
# -----------------------------------------------------------------------------

tabela_resumo <- map_dfr(resultados, function(x) {
  tibble(
    station_code    = x$station_code,
    tau_mk          = round(x$mann_kendall$tau, 2),
    p_valor_mk      = round(x$mann_kendall$sl, 2),
    slope_sen       = round(as.numeric(x$sens_slope$estimates), 2),
    p_valor_sen     = round(x$sens_slope$p.value, 2),
    ponto_mudanca   = round(x$pettitt$estimate, 2),
    p_valor_pettitt = x$pettitt$p.value,
    n_anomalias     = x$n_anomalias,
    n_extremos      = x$n_extremos,
    # Classificação: significativo (p < 0.05) e direção do slope de Sen
    tendencia       = case_when(
      x$mann_kendall$sl < 0.05 & x$sens_slope$estimates < 0 ~ "Negativa",
      x$mann_kendall$sl < 0.05 & x$sens_slope$estimates > 0 ~ "Positiva",
      .default = "Não significativa"
    )
  )
})

print(tabela_resumo)
write.csv(tabela_resumo,
          path(OUTPUT_DIR, "resumo_tendencias_mensal.csv"),
          row.names = FALSE)

# -----------------------------------------------------------------------------
# 7. DADOS EMPILHADOS PARA GRÁFICOS DE SÉRIE TEMPORAL
# -----------------------------------------------------------------------------

# Combina as séries de todas as estações em um único data.frame longo,
# mantendo a coluna station_code para identificação nos facets
dados_empilhados <- map_dfr(resultados, function(x) {
  x$dados |> mutate(station_code = x$station_code)
})

# -----------------------------------------------------------------------------
# 8. GERAÇÃO DE GRÁFICOS
# -----------------------------------------------------------------------------
# gerar_graficos() retorna lista com os objetos ggplot:
#   $tendencias_estacoes          — barplot de slope por estação
#   $tendencias_classificadas     — facets com série + tendência STL
#   $mapa_tendencias_classificadas — mapa simples por categoria
#   $contagens_tendencia          — barplot de frequência de tendência
#   $mapa_tendencia_hidrologica   — mapa com |τ| e camadas geográficas

graficos <- gerar_graficos(tabela_resumo, dados_empilhados, inventario)

# Adiciona o gráfico de disponibilidade gerado pelo selectStations()
graficos$disponibilidade <- dadosestacoes_selecionadas$plot

# Exportar gráficos individualmente (descomentar conforme necessário)
# ggsave(path(IMAGE_DIR, "disponibilidade_estacoes.png"),        graficos$disponibilidade,              width = 10, height = 8,  dpi = 300)
# ggsave(path(IMAGE_DIR, "tendencias_estacoes.png"),             graficos$tendencias_estacoes,          width = 10, height = 8,  dpi = 300)
# ggsave(path(IMAGE_DIR, "mapa_tendencias_classificadas.png"),   graficos$mapa_tendencias_classificadas, width = 16, height = 12, dpi = 300)
# ggsave(path(IMAGE_DIR, "contagens_tendencia.png"),             graficos$contagens_tendencia,          width = 12, height = 5,  dpi = 150)
# ggsave(path(IMAGE_DIR, "mapas_tematicos.png"),                 graficos$mapa_tendencia_hidrologica,   width = 8,  height = 16, dpi = 150)

# -----------------------------------------------------------------------------
# 9. RENDERIZAÇÃO DO RELATÓRIO PDF
# -----------------------------------------------------------------------------
# Os objetos gráficos são passados via `params` para o RMarkdown,
# evitando dependência de variáveis globais no ambiente de renderização.

rmarkdown::render(
  input         = path("relatorio.Rmd"),
  output_format = "pdf_document",
  output_dir    = REPORT_DIR,
  params        = list(graficos = graficos),
  envir         = new.env(parent = globalenv())
)
