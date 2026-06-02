# =============================================================================
# ANÁLISE DE DISPONIBILIDADE E TENDÊNCIAS HIDROLÓGICAS — PIPELINE PRINCIPAL
# =============================================================================
#
# Autor:   Renan Cassimiro Brito
# Data:    2026-05-13
#
# Descrição:
#   Orquestrador do pipeline de análise hidrológica da ANA.
#   Executa as três variáveis (vazão, cota, precipitação) em sequência usando
#   funções genéricas e consolida os resultados num único artefato espacial.
#
# Sequência de execução:
#   [x] 1. Setup de diretórios e configurações
#   [x] 2. Leitura da área de estudo e camadas geográficas
#   [x] 3. Para cada variável (discharge / water_level / precipitation):
#          a. Inventário de estações via API HidroWeb
#          b. Download + organização das séries
#          c. Seleção por critérios de qualidade
#          d. Análise estatística (STL, Mann-Kendall, Sen, Pettitt)
#          e. Tabela resumo + gráficos de tendência
#          f. Exportação dos gráficos de disponibilidade
#   [x] 4. Consolidação: long + wide + spatial (camada analítica final)
#   [ ] 5. Conectividade hidrológica (DEM → rede → igraph) — etapa futura
#   [ ] 6. Moran global + LISA                             — etapa futura
#   [ ] 7. Regionalização (PCA + clustering)               — etapa futura
#   [ ] 8. Sazonalidade hidrológica                        — etapa futura
#   [ ] 9. CHIRPS (tendência + anomalias precipitação)     — etapa futura
#   [ ] 10. Chuva × vazão (elasticidade + residual)        — etapa futura
#   [ ] 11. Pressão territorial (desmatamento + fogo)      — etapa futura
#
# Inputs:
#   input/{RUN_NAME}/
#   ├── {RUN_NAME}_area_estudo.gpkg
#   ├── ne_10m_rivers_lake_centerlines_bacias_amacro.gpkg  (opcional)
#   └── AMACRO/AMACRO.shp                                  (opcional)
#
# Outputs:
#   output/{RUN_NAME}/
#   ├── data/
#   │   ├── {RUN_NAME}_inventario_{variable}.parquet
#   │   ├── stations_{variable}/           — séries brutas por estação
#   │   └── stations_{variable}_organized/ — séries organizadas
#   ├── images/
#   │   ├── disponibilidade_{variable}.png
#   │   ├── tendencias_estacoes_{variable}.png
#   │   ├── mapa_tendencias_classificadas_{variable}.png
#   │   └── ...
#   ├── consolidated/
#   │   ├── {RUN_NAME}_disponibilidade_long.parquet
#   │   ├── {RUN_NAME}_disponibilidade_wide.parquet
#   │   └── {RUN_NAME}_disponibilidade_spatial.parquet
#   └── resumo_disponibilidade_{variable}.parquet
#
# Dependências:
#   CRAN: arrow, fs, here, hydrobr, Kendall, lubridate, patchwork, purrr,
#         scales, sf, sfarrow, tidyverse, trend, zyp, zoo
#   GitHub: devtools::install_github("hydroversebr/hydrobr", build_vignettes = FALSE)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. MÓDULOS DO PROJETO
# -----------------------------------------------------------------------------
source(here::here("config/variable_configs.R"))
source(here::here("R/setup.R"))        # setup_dirs() + bibliotecas
source(here::here("R/inventory.R"))    # obter_inventario(), baixar_e_organizar(), selecionar_estacoes()
source(here::here("R/analysis.R"))     # analisar_estacao(), analisar_todas_estacoes()
source(here::here("R/summary.R"))      # build_tabela_resumo(), build_dados_empilhados()
source(here::here("R/plots.R"))        # gerar_graficos(), salvar_graficos()
source(here::here("R/consolidate.R"))  # consolidar_resultados(), salvar_consolidado()
source(here::here("R/report.R"))          # renderizar_todos()
source(here::here("R/seasonality.R"))


# -----------------------------------------------------------------------------
# 1. CONFIGURAÇÃO DO RUN
# -----------------------------------------------------------------------------
RUN_NAME <- "xingu_river"

# Variáveis a processar — subconjunto de VARIABLE_CONFIGS.
# Para rodar só precipitação: VARIAVEIS_ATIVAS <- c("precipitation")
VARIAVEIS_ATIVAS <- c("discharge", "water_level", "precipitation")

# Filtro de estações: NULL usa todas do inventário.
# Para filtrar: estacoes_filtradas <- st_read(path(dirs$input_dir, "estacoes.gpkg"))
ESTACOES_FILTRADAS <- NULL

# Controla se gera e salva os gráficos de tendência (pode ser lento para muitas estações)
GERAR_GRAFICOS_TENDENCIA <- TRUE

# -----------------------------------------------------------------------------
# 2. SETUP DE DIRETÓRIOS
# -----------------------------------------------------------------------------
dirs <- setup_dirs(RUN_NAME)

# -----------------------------------------------------------------------------
# 3. LEITURA DAS CAMADAS GEOGRÁFICAS
# -----------------------------------------------------------------------------
area_estudo <- st_read(dirs$study_area_path, quiet = TRUE)

# Camadas opcionais — se ausentes, os mapas são gerados sem elas
rios_path   <- path(dirs$input_dir, "ne_10m_rivers_lake_centerlines_bacias_amacro.gpkg")
amacro_path <- path(dirs$input_dir, "AMACRO", "AMACRO.shp")

rios   <- if (file_exists(rios_path))   st_read(rios_path,   quiet = TRUE) else NULL
amacro <- if (file_exists(amacro_path)) st_read(amacro_path, quiet = TRUE) else NULL

if (is.null(rios))   message("Aviso: camada de rios não encontrada — mapas gerados sem hidrografia.")
if (is.null(amacro)) message("Aviso: camada AMACRO não encontrada — G5 gerado sem polígono AMACRO.")

# Mapa rápido de conferência da área de estudo
ggplot() +
  geom_sf(data = area_estudo, fill = NA, color = "red", linewidth = 1) +
  { if (!is.null(rios)) geom_sf(data = rios, color = "steelblue", linewidth = 0.4) } +
  theme_classic() +
  labs(title = paste("Área de estudo —", RUN_NAME))

# -----------------------------------------------------------------------------
# 4. PIPELINE POR VARIÁVEL
# -----------------------------------------------------------------------------
# Cada elemento de `resultados_por_variavel` contém:
#   $cfg, $inventario, $dados_selecionados, $resultados,
#   $tabela_resumo, $dados_empilhados, $graficos (se GERAR_GRAFICOS_TENDENCIA)
resultados_por_variavel <- map(
  VARIABLE_CONFIGS[VARIAVEIS_ATIVAS],
  function(cfg) {
    
    message(sprintf("\n====== %s ======", toupper(cfg$label)))
    
    var_dirs <- dirs$vars[[cfg$id]]
    
    # -- 4a. Inventário --------------------------------------------------------
    inventario <- obter_inventario(
      cfg                = cfg,
      area_estudo        = area_estudo,
      output_path        = var_dirs$inventario,
      estacoes_filtradas = ESTACOES_FILTRADAS
    )
    
    # -- 4b. Download + organização -------------------------------------------
    dados_org <- baixar_e_organizar(
      cfg        = cfg,
      inventario = inventario,
      run_name   = RUN_NAME,
      raw_dir    = var_dirs$raw_dir,
      org_dir    = var_dirs$org_dir
    )
    
    # -- 4c. Seleção por qualidade --------------------------------------------
    estacoes_sel <- selecionar_estacoes(cfg, dados_org)
    
    # -- 4d. Análise estatística ----------------------------------------------
    resultados <- analisar_todas_estacoes(estacoes_sel, cfg)
    
    # -- 4e. Tabela resumo + séries empilhadas --------------------------------
    tabela_resumo    <- build_tabela_resumo(resultados, cfg)
    dados_empilhados <- build_dados_empilhados(resultados)
    
    write_parquet(tabela_resumo, var_dirs$resumo)
    message(sprintf("[%s] Resumo salvo: %d estações", cfg$label, nrow(tabela_resumo)))
    
    # -- 4f. Gráficos de disponibilidade --------------------------------------
    img_disp <- path(dirs$image_dir, paste0("disponibilidade_", cfg$id, ".png"))
    ggsave(img_disp, estacoes_sel$plot, width = 14, height = 8, dpi = 150)
    message(sprintf("[%s] Disponibilidade salva: %s", cfg$label, img_disp))
    
    # -- 4g. Gráficos de tendência (opcional) ---------------------------------
    graficos <- NULL
    
    if (GERAR_GRAFICOS_TENDENCIA) {
      message(sprintf("[%s] Gerando gráficos de tendência...", cfg$label))
      
      graficos <- gerar_graficos(
        tabela_resumo    = tabela_resumo,
        dados_empilhados = dados_empilhados,
        inventario       = inventario,
        area_estudo      = area_estudo,
        cfg              = cfg,
        rios             = rios,
        amacro           = amacro
      )
      
      # Adiciona plot de disponibilidade à lista de gráficos
      graficos$disponibilidade <- estacoes_sel$plot
      
      salvar_graficos(graficos, dirs$image_dir, cfg$id)
    }
    
    list(
      cfg              = cfg,
      inventario       = inventario,
      dados_selecionados = estacoes_sel,
      resultados       = resultados,
      tabela_resumo    = tabela_resumo,
      dados_empilhados = dados_empilhados,
      graficos         = graficos
    )
  }
)

# -----------------------------------------------------------------------------
# 5. CONSOLIDAÇÃO DOS RESULTADOS
# -----------------------------------------------------------------------------
message("\n====== CONSOLIDANDO RESULTADOS ======")

resumos_list     <- map(resultados_por_variavel, "tabela_resumo")
inventarios_list <- map(resultados_por_variavel, "inventario")

consolidado <- consolidar_resultados(
  resumos_list    = resumos_list,
  inventario_list = inventarios_list
)

salvar_consolidado(consolidado, dirs$consolidated_dir, RUN_NAME)

# -----------------------------------------------------------------------------
# 6. CAMADA ANALÍTICA ESPACIAL (analysed_stations)
# -----------------------------------------------------------------------------
# O objeto `consolidado$spatial` já é o sf consolidado (wide + geometria).
# Esta etapa apenas reporta e salva uma cópia nomeada para uso nas etapas
# futuras (Moran, regionalização, conectividade, etc.)

analysed_stations <- consolidado$spatial

if (!is.null(analysed_stations)) {
  glimpse(analysed_stations)
  cat(sprintf("\nEstações na camada analítica: %d\n", nrow(analysed_stations)))
  cat(sprintf("CRS: %s\n", st_crs(analysed_stations)$input))
  cat(sprintf("Variáveis consolidadas: %s\n",
              paste(VARIAVEIS_ATIVAS, collapse = ", ")))
}


# -----------------------------------------------------------------------------
# 7. RELATÓRIOS PDF
# -----------------------------------------------------------------------------
# Requer LaTeX. Para instalar: tinytex::install_tinytex()
# Gera: report/relatorio_{variavel}.pdf  (um por variável)
#        report/relatorio_consolidado_{run_name}.pdf
GERAR_RELATORIOS <- TRUE

if (GERAR_RELATORIOS) {
  message("\n====== GERANDO RELATÓRIOS PDF ======")
  renderizar_todos(
    resultados_por_variavel = resultados_por_variavel,
    consolidado             = consolidado,
    run_name                = RUN_NAME,
    dirs                    = dirs
  )
}

# -----------------------------------------------------------------------------
# 8. VERIFICAÇÃO FINAL
# -----------------------------------------------------------------------------
message("\n====== PIPELINE CONCLUÍDO ======")
message(sprintf("Run:       %s", RUN_NAME))
message(sprintf("Variáveis: %s", paste(VARIAVEIS_ATIVAS, collapse = ", ")))

walk(VARIAVEIS_ATIVAS, function(v) {
  n <- nrow(resultados_por_variavel[[v]]$tabela_resumo)
  message(sprintf("  %-15s → %d estações com análise", v, n))
})

message(sprintf(
  "Consolidado — long: %d linhas | wide: %d colunas | spatial: %s",
  nrow(consolidado$long),
  ncol(consolidado$wide),
  if (!is.null(consolidado$spatial)) paste(nrow(consolidado$spatial), "feições") else "não gerado"
))

# -----------------------------------------------------------------------------
# 9. ANÁLISE DE SAZONALIDADE HIDROLÓGICA (MÓDULO JORNALÍSTICO)
# -----------------------------------------------------------------------------
message("\n====== PROCESSANDO SAZONALIDADE JORNALÍSTICA ENRIQUECIDA ======")

# Loop dinâmico pelas variáveis ativas (vazão, cota, chuva)
walk(VARIAVEIS_ATIVAS, function(v) {
  cfg      <- VARIABLE_CONFIGS[[v]]
  var_dirs <- dirs$vars[[v]]
  
  # 1. Executa o processamento matemático por estação/ano
  df_sazonal <- processar_sazonalidade_pipeline(cfg, var_dirs)
  
  if (!is.null(df_sazonal) && nrow(df_sazonal) > 0) {
    
    # 2. Gera a assinatura histórica resumida (médias históricas)
    df_assinatura <- gerar_assinatura_sazonal(df_sazonal)
    
    # 3. Salva os dados brutos consolidados na pasta 'consolidated/'
    path_sazonal    <- path(dirs$consolidated_dir, paste0(RUN_NAME, "_sazonalidade_anual_", v, ".parquet"))
    path_assinatura <- path(dirs$consolidated_dir, paste0(RUN_NAME, "_assinatura_sazonal_media_", v, ".parquet"))
    
    write_parquet(df_sazonal,    path_sazonal)
    write_parquet(df_assinatura, path_assinatura)
    
    # 4. GERAÇÃO E SALVAMENTO DAS VISUALIZAÇÕES JORNALÍSTICAS
    message(sprintf("  [%s] Renderizando gráficos de sazonalidade...", v))
    lista_plots <- gerar_graficos_sazonalidade(df_sazonal, cfg)
    
    # Salva na pasta oficial de imagens do projeto (ex: output/xingu_river/images/)
    salvar_graficos_sazonalidade(lista_plots, dirs$image_dir, v)
    
    message(sprintf("  [%s] Processo concluído com sucesso!", v))
  }
})

# -----------------------------------------------------------------------------
# 10. VERIFICAÇÃO FINAL
# -----------------------------------------------------------------------------
message("\n====== PIPELINE CONCLUÍDO ======")
message(sprintf("Run:       %s", RUN_NAME))
message(sprintf("Variáveis: %s", paste(VARIAVEIS_ATIVAS, collapse = ", \\")))
