
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

gerar_relatorio <- function(tabela_resumo, dados_empilhados){
  
  print(tabela_resumo)
  # Exportar resultados
  write.csv(tabela_resumo, "relatorio/resumo_tendencias_mensal.csv", row.names = FALSE)
  # saveRDS(resultados, "resultados_analises_mensal.rds")
  
  # Adicionar anotação com o valor da tendência nas barras
  tabela_resumo$label <- paste0(round(tabela_resumo$slope_sen, 2), 
                                ifelse(tabela_resumo$tendencia == "Positiva", " ▲", 
                                       ifelse(tabela_resumo$tendencia == "Negativa", " ▼", " ○")))
  
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
  ggsave("relatorio/tendencias_estacoes.png", width = 10, height = 8, dpi = 300)
  
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
  ggsave("relatorio/tendencias_classificadas.png", width = 16, height = 12, dpi = 300)
  
  ### Plotar estação por tendência
  dados_com_classificacao_localizacao <- st_as_sf(
    inner_join(dados_com_classificacao, inventario[,c('station_code','lat','long', 'geometry')], by='station_code'))
  
  ggplot() +
    geom_sf(data = dados_com_classificacao_localizacao, aes(color=tendencia), size=2) +
    geom_sf(data = area_estudo, fill = NA, color = "red")+
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      strip.text = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  ggsave("relatorio/mapa_tendencias_classificadas.png", width = 16, height = 12, dpi = 300)
  
  
}