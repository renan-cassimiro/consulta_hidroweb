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

print(tabela_resumo)

# Exportar resultados
write.csv(tabela_resumo, "resumo_tendencias_mensal.csv", row.names = FALSE)
# saveRDS(resultados, "resultados_analises_mensal.rds")

# Adicionar anotação com o valor da tendência nas barras
tabela_resumo$label <- paste0(round(tabela_resumo$slope_sen, 2), 
                              ifelse(tabela_resumo$tendencia == "Positiva", " ▲", 
                                     ifelse(tabela_resumo$tendencia == "Negativa", " ▼", " ○")))

# Empilhar todos os dados para gráfico
dados_empilhados <- map_dfr(resultados, function(x) {
  x$dados %>% mutate(station_code = x$station_code)
})

ggplot(tabela_resumo, aes(x = reorder(station_code, slope_sen), 
                          y = slope_sen, 
                          fill = tendencia)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label), hjust = ifelse(tabela_resumo$slope_sen < 0, 1.1, -0.1), 
            size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = c("Positiva" = "steelblue", 
                               "Negativa" = "darkred", 
                               "Não significativa" = "gray70")) +
  labs(title = "Tendência de vazão por estação",
       subtitle = "Valores em m³/s/ano | ▲ positivo ▼ negativo ○ não significativo",
       x = "Estação", 
       y = "Tendência (m³/s/ano)",
       fill = "Tendência") +
  theme_minimal()

# Salvar gráfico
ggsave("tendencias_estacoes.png", width = 10, height = 8, dpi = 300)

# Preparar dados com classificação de tendência
dados_com_classificacao <- dados_empilhados %>%
  left_join(tabela_resumo %>% select(station_code, slope_sen, p_valor_mk, tendencia), by = "station_code") %>%
  mutate(
    cor_tendencia = case_when(
      tendencia == "Negativa" ~ "Negativa",
      tendencia == "Positiva" ~ "Positiva",
      TRUE ~ "Não significativa"
    ),
    titulo_facet = paste0(station_code, "\n", 
                          round(slope_sen, 2), " m³/s/ano | p = ", round(p_valor_mk, 4),
                          ifelse(tendencia == "Negativa", " ▼", 
                                 ifelse(tendencia == "Positiva", " ▲", " ○")))
  )

### Esse tá sendo o melhor
# Gráfico com cores diferentes para cada classificação
ggplot(dados_com_classificacao, aes(x = date, y = stream_flow_m3_s)) +
  geom_line(alpha = 0.3, size = 0.3, color = "gray50") +
  geom_line(aes(y = trend, color = cor_tendencia), size = 1.2) +
  facet_wrap(~titulo_facet, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c(
      "Negativa" = "#D32F2F",      # vermelho
      "Positiva" = "#1976D2",      # azul
      "Não significativa" = "#757575"  # cinza
    ),
    name = "Tendência"
  ) +
  labs(
    title = "Tendência de vazão por estação - Bacia Amazônica",
    subtitle = "Série original (cinza) | Linha colorida = tendência (STL) | ▼ Negativa ▲ Positiva ○ Não significativa",
    x = "Data", 
    y = "Vazão (m³/s)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(size = 8, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# Salvar
ggsave("tendencias_classificadas.png", width = 16, height = 12, dpi = 300)

### Plotar estação por tendência
dados_com_classificacao_localizacao <- st_as_sf(
  inner_join(dados_com_classificacao, inventario[,c('station_code','lat','long', 'geometry')], by='station_code'))

ggplot() +
  geom_sf(data = dados_com_classificacao_localizacao, aes(color=tendencia), size=2) +
  geom_sf(data = area_estudo, fill = NA, color = "red")+
  theme_classic()

### TODO
# Cruzar os códigos das estações com a camada que tem o nome dos rios, 
# bacia, area de captação, entre outros para filtrar o conteúdo antes de chamar
# as funções do hydrobr e para avançar em análises geográfica.

#Aplicar as análises temporais disponíveis no pacote hydrobr

## Futuras aplicações, permitir que sejam selecionadas diversas estações para se 
## via o site do shyni, permitindo também a seleção de bacias hidrográficas e rios.
