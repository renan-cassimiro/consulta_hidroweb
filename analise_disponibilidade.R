# if (!require(devtools)) install.packages("devtools")
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

setwd("C:/Users/renan/OneDrive/Projetos/amacro/r")

# Função que aplica toda análise para UMA estação (com dados mensais)
analisar_estacao <- function(df, nome_estacao) {
  
  # Converter para médias mensais (versão sem aviso)
  dt_mensal <- df %>%
    mutate(
      date = as.Date(date),
      ano = year(date),
      mes = month(date)
    ) %>%
    group_by(ano, mes) %>%
    summarise(
      stream_flow_m3_s = mean(stream_flow_m3_s, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(date = as.Date(paste(ano, mes, "01", sep = "-"))) %>%
    arrange(date) %>%
    filter(!is.na(stream_flow_m3_s))
  
  # Criar série temporal para STL (frequência = 12 para mensal)
  ts_vazao <- ts(dt_mensal$stream_flow_m3_s, 
                 frequency = 12, 
                 start = c(min(dt_mensal$ano), min(dt_mensal$mes)))
  
  # STL decomposition
  decomp <- stl(ts_vazao, s.window = "periodic")
  
  trend <- as.numeric(decomp$time.series[, "trend"])
  residual <- as.numeric(decomp$time.series[, "remainder"])
  
  dt_mensal <- dt_mensal %>%
    mutate(
      trend = trend,
      resid = residual
    )
  
  # Teste de Mann-Kendall na tendência
  mk <- MannKendall(dt_mensal$trend)
  
  # Teste com correção Yue-Pilon (para autocorrelação)
  mk_mod <- zyp.trend.vector(dt_mensal$stream_flow_m3_s, method = "yuepilon")
  
  # Estimador de Sen
  sen <- sens.slope(dt_mensal$stream_flow_m3_s)
  
  # Climatologia mensal para anomalias (agora usando os meses 1-12)
  clim <- dt_mensal %>%
    group_by(mes) %>%
    summarise(
      media = mean(stream_flow_m3_s, na.rm = TRUE),
      sd = sd(stream_flow_m3_s, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Juntar e calcular anomalias
  dt_mensal <- dt_mensal %>%
    left_join(clim, by = "mes") %>%
    mutate(
      z = (stream_flow_m3_s - media) / sd,
      anomalia = ifelse(abs(z) > 2, "severa", ifelse(abs(z) > 1, "moderada", "normal")),
      extremo = abs(z) > 3,
      moderada = abs(z) > 2 & abs(z) <= 3
    )
  
  # Teste de Pettitt (ponto de mudança)
  pettitt <- pettitt.test(dt_mensal$stream_flow_m3_s)
  
  # Retornar resultados
  list(
    estacao = nome_estacao,
    dados = dt_mensal,
    decomposicao = decomp,
    mann_kendall = mk,
    yue_pilon = mk_mod,
    sens_slope = sen,
    pettitt = pettitt,
    n_anomalias = sum(dt_mensal$anomalia == "severa"),
    n_extremos = sum(dt_mensal$extremo)
  )
}
## Área de estudo
area_estudo <- st_read("../vetores/hybas_lake_sa_lev03_v1c/bacias_amacro_hybas_lake_sa_lev03_v1c_dissolvido.gpkg")

plot(area_estudo[,1])

inventario <- inventory(stationType = "flu", as_sf = T, aoi=area_estudo)
estacoes_selecionadas <- st_read("../resultados/ANA/estacoes_fluviometricas_selecionadas_principais_rios.gpkg")

inventario <- inventario %>% filter(station_code %in% estacoes_selecionadas$CodigoEstacao)

plot(inventario[,1], add= TRUE)


### Baixar dados das estações
dados_inventario <- stationsData(inventoryResult = inventario)

inventario_filtrado_com_dados <- inventario %>% filter(station_code %in% names(dados_inventario))
# plot(inventario_filtrado_com_dados[,1], add= TRUE)


### Organizar dados das estações
dados_inventario_organizado <- organize(dados_inventario)


### Organizar dados das estações
dadosestacoes_selecionadas <- selectStations(organizeResult = dados_inventario_organizado,
                                             mode = "yearly",
                                             maxMissing = 5,
                                             minYears = 1,
                                             month = 1,
                                             iniYear = 2010,
                                             finYear = 2025,
                                             consistedOnly = F)

dadosestacoes_selecionadas$series %>%
  bind_rows(.id = "station_code ") %>%
  ggplot(aes(x = date, y = stream_flow_m3_s )) +
  geom_line(color = "steelblue", size = 0.3) +
  facet_wrap(~station_code, scales = "free_y", ncol = 2) +
  labs(title = "Séries históricas - Estações fluviométricas",
       x = "Data", y = "Vazão (m³/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dadosestacoes_selecionadas$series[[3]] <- NULL 

resultados <- map2(dadosestacoes_selecionadas$series, names(dadosestacoes_selecionadas$series), analisar_estacao)
names(resultados) <- names(dadosestacoes_selecionadas$series)

# Extrair tabela resumo
tabela_resumo <- map_dfr(resultados, function(x) {
  tibble(
    estacao = x$estacao,
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

# Empilhar todos os dados para gráfico
dados_empilhados <- map_dfr(resultados, function(x) {
  x$dados %>% mutate(estacao = x$estacao)
})

# Gráfico com linha da média climatológica mensal
ggplot(dados_empilhados, aes(x = date, y = stream_flow_m3_s)) +
  geom_col(alpha = 0.5, size = 0.3, fill = "steelblue", width = 25) +
  
  # Linha da média mensal (climatologia)
  geom_line(aes(y = media), color = "black", size = 1, linetype = "dashed") +
  
  # Extremos (|z| > 3)
  geom_point(data = ~ filter(.x, extremo == TRUE), 
             aes(x = date, y = stream_flow_m3_s), 
             color = "red", size = 3) +
  
  # Anomalias severas (|z| > 2)
  geom_point(data = ~ filter(.x, anomalia == "severa" & !extremo), 
             aes(x = date, y = stream_flow_m3_s), 
             color = "orange", size = 2) +
  
  facet_wrap(~estacao, scales = "free_y", ncol = 2) +
  
  scale_x_date(date_breaks = "6 months", 
               date_minor_breaks = "1 month", 
               expand = c(0, 0)) +
  
  labs(title = "Vazão mensal com eventos extremos destacados",
       subtitle = "Vermelho = |z| > 3 | Laranja = |z| > 2 | Linha tracejada = média climatológica mensal",
       x = "Data", y = "Vazão (m³/s)") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 9, face = "bold"))


# Gráfico de série temporal mensal com anomalias
ggplot(dados_empilhados, aes(x = date, y = stream_flow_m3_s)) +
  geom_col(alpha = 0.7, size = 0.4, color="blue") +
  geom_point(data = ~ filter(.x, extremo == TRUE), 
             aes(x = date, y = stream_flow_m3_s), 
             color = "red", size = 3) +
  geom_point(data = ~ filter(.x, moderada == TRUE), 
             aes(x = date, y = stream_flow_m3_s), 
             color = "orange", size = 2) +
  facet_wrap(~estacao, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks ="6 months", date_minor_breaks = "1 month", expand = c(0,0))+
  labs(title = "Vazão mensal com eventos extremos destacados",
       subtitle = "Pontos vermelhos = anomalias com |z| > 3",
       x = "Data", y = "Vazão (m³/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exportar resultados
write.csv(tabela_resumo, "resumo_tendencias_mensal.csv", row.names = FALSE)
# saveRDS(resultados, "resultados_analises_mensal.rds")

# Adicionar anotação com o valor da tendência nas barras
tabela_resumo$label <- paste0(round(tabela_resumo$slope_sen, 2), 
                              ifelse(tabela_resumo$tendencia == "Positiva", " ▲", 
                                     ifelse(tabela_resumo$tendencia == "Negativa", " ▼", " ○")))

ggplot(tabela_resumo, aes(x = reorder(estacao, slope_sen), 
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
  left_join(tabela_resumo %>% select(estacao, slope_sen, p_valor_mk, tendencia), by = "estacao") %>%
  mutate(
    cor_tendencia = case_when(
      tendencia == "Negativa" ~ "Negativa",
      tendencia == "Positiva" ~ "Positiva",
      TRUE ~ "Não significativa"
    ),
    titulo_facet = paste0(estacao, "\n", 
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

# Gráfico adicional: só as tendências (sem série original)
ggplot(dados_com_classificacao, aes(x = date, y = trend, color = cor_tendencia)) +
  geom_line(size = 1.2) +
  facet_wrap(~titulo_facet, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c("Negativa" = "#D32F2F", "Positiva" = "#1976D2", "Não significativa" = "#757575"),
    name = "Tendência"
  ) +
  labs(
    title = "Tendência de vazão por estação (apenas componente de tendência)",
    subtitle = "Vermelho = tendência negativa | Azul = positiva | Cinza = não significativa",
    x = "Data", 
    y = "Tendência (m³/s)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(size = 8, face = "bold"),
    legend.position = "bottom"
  )

ggsave("tendencias_classificadas_apenas_trend.png", width = 16, height = 12, dpi = 300)