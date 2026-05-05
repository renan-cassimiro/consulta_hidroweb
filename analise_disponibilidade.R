# if (!require(devtools)) install.packages("devtools")re
# library(devtools)
# renv::install("dplyr@0.8.5")
# install_github("hydroversebr/hydrobr", build_vignettes = FALSE)

library(sf)
library(hydrobr)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(zoo)
library(trend)
library(Kendall)
library(zyp)
library(dplyr)
library(tidyr)

# Define o diretório para onde o script está salvo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("functions/functions.R")

## Área de estudo
area_estudo <- st_read("resources/hybas_lake_sa_lev03_v1c_bacia_amazonica.gpkg")

inventario <- inventory(stationType = "flu", as_sf = T, aoi=area_estudo)

saveRDS(object=inventario , file="resources/inventario")

###TODO Melhorar gráfico de apresentação
ggplot() +
  geom_sf(data = inventario) +
  geom_sf(data = area_estudo, fill = NA, color = "red")+
  theme_classic()

### Baixar dados das estações
####TODO retirara o filtro para baixar todos os dados
dados_inventario <- stationsData(inventoryResult = inventario[seq(1,100,1),])
saveRDS(object=dados_inventario , file="resources/dados_inventario")

### Organizar dados das estações
dados_inventario_organizado <- organize(dados_inventario)
saveRDS(object=dados_inventario_organizado , file="resources/dados_inventario_organizado")

### Organizar dados das estações
dadosestacoes_selecionadas <- selectStations(organizeResult = dados_inventario_organizado,
                                             mode = "yearly",
                                             maxMissing = 5,
                                             minYears = 10,
                                             month = 1,
                                             iniYear = 2010,
                                             finYear = 2025,
                                             consistedOnly = F)
saveRDS(object=dadosestacoes_selecionadas , file="resources/dadosestacoes_selecionadas")

# #usando dados já baixados para teste
# dadosestacoes_selecionadas$series <- readRDS('dados_inventario')

resultados <- map2(dadosestacoes_selecionadas$series, names(dadosestacoes_selecionadas$series), analisar_estacao)

# Extrair tabela resumo
tabela_resumo <- map_dfr(resultados, function(x) {
  tibble(
    station_code = x$station_code,
    tau_mk = x$mann_kendall$tau,
    p_valor_mk = x$mann_kendall$sl,
    slope_sen = as.numeric(x$sens_slope$estimates),
    p_valor_sen = x$sens_slope$p.value,
    ponto_mudanca = x$pettitt$estimate,
    p_valor_pettitt = x$pettitt$p.value,
    n_anomalias = x$n_anomalias,
    n_extremos = x$n_extremos,
    tendencia = ifelse(x$mann_kendall$sl < 0.05 & x$sens_slope$estimates < 0, "Negativa",
                       ifelse(x$mann_kendall$sl < 0.05 & x$sens_slope$estimates > 0, "Positiva", "Não significativa"))
  )
})


# Empilhar todos os dados para gráfico
dados_empilhados <- map_dfr(resultados, function(x) {
  x$dados %>% mutate(station_code = x$station_code)
})


gerar_relatorio(tabela_resumo, dados_empilhados)

### TODO
# Cruzar os códigos das estações com a camada que tem o nome dos rios, 
# bacia, area de captação, entre outros para filtrar o conteúdo antes de chamar
# as funções do hydrobr e para avançar em análises geográfica.

#Aplicar as análises temporais disponíveis no pacote hydrobr

## Futuras aplicações, permitir que sejam selecionadas diversas estações para se 
## via o site do shyni, permitindo também a seleção de bacias hidrográficas e rios.
