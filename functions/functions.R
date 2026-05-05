
# Função que aplica toda análise para UMA estação (com dados mensais)
analisar_estacao <- function(df, station_code) {
  
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
    station_code  = station_code,
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