# if (!require(devtools)) install.packages("devtools")re
# library(devtools)
# renv::install("dplyr@0.8.5")
# install_github("hydroversebr/hydrobr", build_vignettes = FALSE)
library(fs)
library(here)
library(hydrobr) 
library(Kendall)
library(lubridate)
library(openxlsx)
library(patchwork)   # composição de painéis
library(scales)
library(sf)
library(tidyverse)
library(trend)
library(zyp)
library(zoo)

source(here("functions/functions.R"))

RESOURCES_DIR <- here("resources")
OUTPUT_DIR <- here("output", "refactor_test")
REPORT_DIR <- path(OUTPUT_DIR, "report")
IMAGE_DIR <- path(OUTPUT_DIR, "images")
DATA_DIR <- path(OUTPUT_DIR, "data")

## Área de estudo
area_estudo <- st_read(path(RESOURCES_DIR,"hybas_lake_sa_lev03_v1c_bacia_amazonica.gpkg"))

#Set estacoes_filtradas = NULL if you not preselected the stations
# estacoes_filtradas <- NULL
estacoes_filtradas <- st_read(path(RESOURCES_DIR, "dourada_geoft_estacao_hidrometeorologica_filtradas.gpkg"))

inventario <- inventory(stationType = "flu", as_sf = T, aoi=area_estudo)

if(!is.null(estacoes_filtradas)){
  inventario <- inventario %>% filter(inventario$station_code %in% estacoes_filtradas$CodigoEstacao)
}

saveRDS(object=inventario , file=path(DATA_DIR, "inventario"))

###TODO Melhorar gráfico de apresentação, incluir cama de rio, estados
ggplot() +
  geom_sf(data = inventario) +
  geom_sf(data = area_estudo, fill = NA, color = "red")+
  theme_classic()

### Download stations data
dados_inventario <- stationsData(inventoryResult = inventario)
saveRDS(object=dados_inventario , file=path(DATA_DIR, "dados_inventario"))

### Organizar dados das estações
dados_inventario_organizado <- organize(dados_inventario)
saveRDS(object=dados_inventario_organizado , file=path(DATA_DIR, "dados_inventario_organizado"))

### Organizar dados das estações
dadosestacoes_selecionadas <- selectStations(organizeResult = dados_inventario_organizado,
                                             mode = "yearly",
                                             maxMissing = 5,
                                             minYears = 10,
                                             month = 1,
                                             iniYear = 1900,
                                             finYear = 2025,
                                             consistedOnly = F)

saveRDS(object=dadosestacoes_selecionadas , file=path(DATA_DIR, "dadosestacoes_selecionadas"))
ggsave(file=path(IMAGE_DIR, "disponibilidade_estacoes.png"), width = 6, height = 12, dpi = 330)


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

print(tabela_resumo)
# Exportar resultados
write.csv(tabela_resumo, path(OUTPUT_DIR, "resumo_tendencias_mensal.csv"), row.names = FALSE)
# saveRDS(resultados, "resultados_analises_mensal.rds")

# Empilhar todos os dados para gráfico
dados_empilhados <- map_dfr(resultados, function(x) {
  x$dados |> mutate(station_code = x$station_code)
})

graficos <- gerar_graficos(tabela_resumo, dados_empilhados, inventario)
graficos$disponibilidade <- dadosestacoes_selecionadas$plot
 
# ggsave(path(IMAGE_DIR, "disponibilidade_estacoes.png"), graficos$disponibilidade, width = 10, height = 8, dpi = 300)
# ggsave(path(IMAGE_DIR, "tendencias_estacoes.png"), graficos$tendencias_estacoes, width = 10, height = 8, dpi = 300)
# ggsave(path(IMAGE_DIR, "mapa_tendencias_classificadas.png"), graficos$mapa_tendencias_classificadas, width = 16, height = 12, dpi = 300)
# ggsave(path(IMAGE_DIR, "contagens_tendencia.png"), graficos$contagens_tendencia, width = 12, height = 5, dpi = 150)
# ggsave(path(IMAGE_DIR, "mapas_tematicos.png"), graficos$mapa_tendencia_hidrologica, width = 8, height = 16, dpi = 150)

rmarkdown::render(path("relatorio.Rmd"), output_format = "pdf_document", output_dir = path(REPORT_DIR),
                  params=list(graficos=graficos), envir = new.env(parent = globalenv()))

### TODO
# Cruzar os códigos das estações com a camada que tem o nome dos rios, 
# bacia, area de captação, entre outros para filtrar o conteúdo antes de chamar
# as funções do hydrobr e para avançar em análises geográfica.

#Aplicar as análises temporais disponíveis no pacote hydrobr

## Futuras aplicações, permitir que sejam selecionadas diversas estações para se 
## via o site do shyni, permitindo também a seleção de bacias hidrográficas e rios.
