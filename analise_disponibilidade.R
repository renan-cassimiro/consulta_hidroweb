# if (!require(devtools)) install.packages("devtools")re
# library(devtools)
# renv::install("dplyr@0.8.5")
# install_github("hydroversebr/hydrobr", build_vignettes = FALSE)

library(dplyr)
library(hydrobr)
library(Kendall)
library(lubridate)
library(openxlsx)
library(patchwork)   # composição de painéis
library(scales)
library(sf)
library(tidyr)
library(tidyverse)
library(trend)
library(zyp)
library(zoo)


# Define o diretório para onde o script está salvo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions/functions.R")

## Área de estudo
area_estudo <- st_read("resources/hybas_lake_sa_lev03_v1c_bacia_amazonica.gpkg")
# estacoes_filtradas <- st_read("resources/estacoes_fluviometricas_filtradas_tamanho_bacia_vazao.gpkg")
# estacoes_filtradas <- st_read("resources/estacoes_fluviometricas_selecionadas_pelo_nome.gpkg")
estacoes_filtradas <- st_read("resources/dourada_geoft_estacao_hidrometeorologica_filtradas.gpkg")

inventario <- inventory(stationType = "flu", as_sf = T, aoi=area_estudo)
#TODO aplicar filtro antes de baixar os dados, este abaixo é só para teste offline
inventario <- inventario %>% filter(inventario$station_code %in% estacoes_filtradas$CodigoEstacao)

saveRDS(object=inventario , file="resources/inventario_dourada")

###TODO Melhorar gráfico de apresentação
mapa_inicial <- ggplot() +
  geom_sf(data = inventario) +
  geom_sf(data = area_estudo, fill = NA, color = "red")+
  theme_classic()

### Baixar dados das estações
####TODO retirara o filtro para baixar todos os dados
dados_inventario <- stationsData(inventoryResult = inventario)
saveRDS(object=dados_inventario , file="resources/dados_inventario_dourada")
# dados_inventario <- readRDS(file="resources/dados_inventario")


### Organizar dados das estações
dados_inventario_organizado <- organize(dados_inventario)

#TODO aplicar filtro antes de baixar os dados, este abaixo é só para teste offline
# dados_inventario_organizado <- dados_inventario_organizado[estacoes_filtradas$CodigoEstacao]


saveRDS(object=dados_inventario_organizado , file="resources/dados_inventario_organizado_dourada")


### Organizar dados das estações
dadosestacoes_selecionadas <- selectStations(organizeResult = dados_inventario_organizado,
                                             mode = "yearly",
                                             # maxMissing = 5,
                                             minYears = 2,
                                             month = 1,
                                             iniYear = 1900,
                                             finYear = 2025,
                                             consistedOnly = F)

ggsave("relatorio_dourada/disponibilidade_estacoes.png", width = 6, height = 12, dpi = 330)

saveRDS(object=dadosestacoes_selecionadas , file="resources/dadosestacoes_selecionadas")

# #usando dados já baixados para teste
# dadosestacoes_selecionadas$series <- readRDS('dados_inventario')
# dadosestacoes_selecionadas <- readRDS('resources/dadosestacoes_selecionadas')

resultados <- map2(dadosestacoes_selecionadas$series, names(dadosestacoes_selecionadas$series), analisar_estacao)

# Extrair tabela resumo
tabela_resumo <- map_dfr(resultados, function(x) {
  tibble(
    station_code = x$station_code,
    tau_mk = round(x$mann_kendall$tau, 2),
    p_valor_mk = round(x$mann_kendall$sl, 2),
    slope_sen = round(as.numeric(x$sens_slope$estimates), 2),
    p_valor_sen = round(x$sens_slope$p.value, 2),
    ponto_mudanca = round(x$pettitt$estimate, 2),
    p_valor_pettitt = x$pettitt$p.value,
    n_anomalias = x$n_anomalias,
    n_extremos = x$n_extremos,
    tendencia = ifelse(x$mann_kendall$sl < 0.05 & x$sens_slope$estimates < 0, "Negativa",
                       ifelse(x$mann_kendall$sl < 0.05 & x$sens_slope$estimates > 0, "Positiva", "Não significativa"))
  )
})

# Empilhar todos os dados para gráfico
dados_empilhados <- map_dfr(resultados, function(x) {
  x$dados |> mutate(station_code = x$station_code)
})

graficos <- gerar_relatorio(tabela_resumo, dados_empilhados)

# =============================================================================
# ANÁLISE DESCRITIVA E MAPAS TEMÁTICOS — ESTAÇÕES ANA
# =============================================================================
# Assumido: `tabela_resumo` é o data.frame com os resultados de tendência
#           `estacoes_sf` é o objeto sf com geometria (ponto por estação)
#           ambos com `station_code` como chave de junção
# =============================================================================
      # formatação de eixos

# Paleta e ordem fixos para tendência (use em todos os gráficos)
CORES_TENDENCIA <- c(
  "Negativa"          = "#d73027",
  "Positiva"          = "#4575b4",
  "Não significativa" = "#969696"
)
ORDEM_TENDENCIA <- c("Negativa", "Não significativa", "Positiva")

tabela_resumo <- tabela_resumo |>
  mutate(tendencia = factor(tendencia, levels = ORDEM_TENDENCIA))


# =============================================================================
# 1. CONTAGENS
# =============================================================================

# --- 1.1 Frequência de tendência (barplot) + proporção no label --------------

contagem_tend <- tabela_resumo |>
  count(tendencia) |>
  mutate(prop = n / sum(n),
         label_bar = paste0(n, "\n(", percent(prop, accuracy = 1), ")"))

p1 <- ggplot(contagem_tend, aes(x = tendencia, y = n, fill = tendencia)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = label_bar), vjust = -0.3, size = 3.5, lineheight = 0.9) +
  scale_fill_manual(values = CORES_TENDENCIA) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Frequência de tendência",
       x = NULL, y = "Nº de estações") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    base_size = 14
    )

# --- 1.2 n_anomalias por tendência (boxplot + jitter) ------------------------

p2 <- ggplot(tabela_resumo,
             aes(x = tendencia, y = n_anomalias, fill = tendencia)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.7,
               show.legend = FALSE) +
  geom_jitter(aes(color = tendencia), width = 0.15, size = 2,
              alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values  = CORES_TENDENCIA) +
  scale_color_manual(values = CORES_TENDENCIA) +
  labs(title = "Anomalias por tendência",
       x = NULL, y = "n_anomalias") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major.x = element_blank())

# --- 1.3 n_extremos por tendência --------------------------------------------
# n_extremos tem muitos zeros; violin mostra bem a massa em zero

p3 <- ggplot(tabela_resumo,
             aes(x = tendencia, y = n_extremos, fill = tendencia)) +
  geom_violin(width = 0.7, alpha = 0.5, show.legend = FALSE) +
  geom_jitter(aes(color = tendencia), width = 0.12, size = 2.2,
              alpha = 0.85, show.legend = FALSE) +
  scale_fill_manual(values  = CORES_TENDENCIA) +
  scale_color_manual(values = CORES_TENDENCIA) +
  labs(title = "Extremos por tendência",
       x = NULL, y = "n_extremos") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major.x = element_blank())

# Composição dos 3 painéis de contagem
painel_contagens <- p1 | p2 | p3
painel_contagens + plot_annotation(
  title    = "Distribuição das estações ANA por categoria de tendência",
  theme    = theme(plot.title = element_text(size = 13, face = "bold"))
)

ggsave("relatorio_dourada/contagens_tendencia.png", width = 12, height = 5, dpi = 150)


# =============================================================================
# 2. MAPAS TEMÁTICOS
# =============================================================================

# Junta atributos analíticos ao objeto espacial
estacoes_map <- inventario |>
  left_join(tabela_resumo, by = "station_code") |>
  mutate(abs_tau = abs(tau_mk))

# Base cartográfica mínima — substitua por geobr::read_state() se quiser UFs
# library(geobr)
# brasil <- read_country(year = 2020, simplified = TRUE)

# Tema limpo para mapas
tema_mapa <- theme_void(base_size = 14) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(size = 12, color = "gray40"),
    legend.position  = "right",
    legend.key.width = unit(1, "cm")
  )

# --- 2.1 Cor = tendência | Tamanho = |tau_mk| --------------------------------
rios <- st_read("resources/ne_10m_rivers_lake_centerlines_amazonia.gpkg")
amacro <- st_read("resources/AMACRO/AMACRO.shp")

m1 <- ggplot() +
  # geom_sf(data = brasil, fill = "gray96", color = "gray70", linewidth = 0.3) +
  geom_sf(data = amacro, fill = "pink", linewidth=0.8)+
  geom_sf(data = rios, color="blue", linewidth=0.5)+
  geom_sf(data = area_estudo, color = "black", linewidth=1)+
  geom_sf(data = estacoes_map,
          aes(color = tendencia, size = abs_tau)) +
  scale_color_manual(values = CORES_TENDENCIA, name = "Tendência") +
  scale_size_continuous(
    name   = "|τ Mann-Kendall|",
    range  = c(1.5, 6),
    breaks = c(0.1, 0.2, 0.3, 0.4)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    size  = guide_legend()
  ) +
  labs(title    = "Direção e força da tendência",
       subtitle = "Cor = categoria | Tamanho = |τ|") +
  tema_mapa

# --- 2.2 Cor = slope_sen (divergente) ----------------------------------------
# Centraliza a escala em 0; limita outliers extremos com oob = squish

lim_slope <- quantile(estacoes_map$slope_sen, probs = c(0.02, 0.98),
                      na.rm = TRUE)

m2 <- ggplot() +
  # geom_sf(data = brasil, fill = "gray96", color = "gray70", linewidth = 0.3) +
  geom_sf(data = estacoes_map,
          aes(color = slope_sen), size = 3, alpha = 0.9) +
  geom_sf(data = area_estudo, fill = NA, color = "red")+
  scale_color_distiller(
    palette  = "RdBu",
    direction = 1,            # azul = negativo (redução), vermelho = positivo
    name     = "Slope Sen\n(unid/tempo)",
    limits   = c(-max(abs(lim_slope)), max(abs(lim_slope))),
    oob      = scales::squish,
    labels   = label_number(accuracy = 0.1)
  ) +
  labs(title    = "Magnitude da tendência (slope de Sen)",
       subtitle = "Azul = redução | Vermelho = aumento") +
  tema_mapa

# --- 2.3 Cor = ponto_mudanca (quando ocorreu) --------------------------------

m3 <- ggplot() +
  # geom_sf(data = brasil, fill = "gray96", color = "gray70", linewidth = 0.3) +
  geom_sf(data = estacoes_map,
          aes(color = ponto_mudanca), size = 3, alpha = 0.9) +
  geom_sf(data = area_estudo, fill = NA, color = "red")+
  scale_color_viridis_c(
    option   = "plasma",
    name     = "Índice do ponto\nde mudança",
    direction = -1            # roxo = mudanças mais antigas, amarelo = recentes
  ) +
  labs(title    = "Ponto de mudança estrutural (Pettitt)",
       subtitle = "Valores menores = mudança mais cedo na série") +
  tema_mapa

# Composição dos 3 mapas (empilhados ou lado a lado — ajuste conforme aspect ratio)
painel_mapas <- m1 / m2 / m3   # empilhado; use | para lado a lado

painel_mapas + plot_annotation(
  title = "Mapas temáticos — Tendências hidrológicas (ANA)",
  theme = theme(plot.title = element_text(size = 13, face = "bold"))
)

ggsave("relatorio_dourada/mapas_tematicos.png", width = 8, height = 16, dpi = 150)
# Para lado a lado: width = 18, height = 7

rmarkdown::render("relatorio_dourada/relatorio_dourada.Rmd", output_format = "pdf_document")

### TODO
# Cruzar os códigos das estações com a camada que tem o nome dos rios, 
# bacia, area de captação, entre outros para filtrar o conteúdo antes de chamar
# as funções do hydrobr e para avançar em análises geográfica.

#Aplicar as análises temporais disponíveis no pacote hydrobr

## Futuras aplicações, permitir que sejam selecionadas diversas estações para se 
## via o site do shyni, permitindo também a seleção de bacias hidrográficas e rios.
