
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

gerar_graficos <- function(tabela_resumo, dados_empilhados, inventario){
  
  tabela_resumo$label <- paste0(round(tabela_resumo$slope_sen, 2), 
                                ifelse(tabela_resumo$tendencia == "Positiva", " ▲", 
                                       ifelse(tabela_resumo$tendencia == "Negativa", " ▼", " ○")))
  
  tendencias_estacoes <- ggplot(tabela_resumo, aes(x = reorder(station_code, slope_sen), 
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
  tendencias_classificadas <- ggplot(dados_com_classificacao, aes(x = date, y = stream_flow_m3_s)) +
    geom_line(alpha = 0.3, linewidth = 0.3, color = "gray50") +
    geom_line(aes(y = trend, color = cor_tendencia), linewidth = 1.2) +
    facet_wrap(~titulo_facet, scales = "free_y", ncol = 7) +
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
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = element_text(size = 7),
      strip.text = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  ### Plotar estação por tendência
  dados_com_classificacao_localizacao <- st_as_sf(
    inner_join(dados_com_classificacao, inventario[,c('station_code','lat','long', 'geometry')], by='station_code'))
  
  rios <- st_read(path(RESOURCES_DIR, "ne_10m_rivers_lake_centerlines_amazonia.gpkg"))
  
  mapa_tendencias_classificadas <- ggplot() +
    geom_sf(data = area_estudo, color = "black", linewidth=1)+
    geom_sf(data = rios, color="blue", linewidth=0.5)+
    geom_sf(data = dados_com_classificacao_localizacao, aes(color=tendencia), size=5) +
    theme_minimal() +
    scale_color_manual(
      values = c(
        "Negativa" = "#D32F2F",      # vermelho
        "Positiva" = "#1976D2",      # azul
        "Não significativa" = "#757575"  # cinza
      ),
      name = "Tendência"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      strip.text = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
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
  
  contagens_tendencia <- ggplot(contagem_tend, aes(x = tendencia, y = n, fill = tendencia)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = label_bar), vjust = -0.3, size = 3.5, lineheight = 0.9) +
    scale_fill_manual(values = CORES_TENDENCIA) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Frequência de tendência",
         x = NULL, y = "Nº de estações") +
    theme(
      panel.grid.major.x = element_blank(),
      base_size = 14
    )
  
  # Composição dos 3 painéis de contagem
  contagens_tendencia <- contagens_tendencia + plot_annotation(
    title    = "Distribuição das estações ANA por categoria de tendência",
    theme    = theme(plot.title = element_text(size = 13, face = "bold"))
  )
  
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
  rios <- st_read(path(RESOURCES_DIR, "ne_10m_rivers_lake_centerlines_amazonia.gpkg"))
  amacro <- st_read(path(RESOURCES_DIR, "AMACRO/AMACRO.shp"))
  
  mapa_tendencia_hidrologica <- ggplot() +
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
  
  mapa_tendencia_hidrologica <- mapa_tendencia_hidrologica + plot_annotation(
    title = "Mapas de tendências hidrológicas (ANA)",
    theme = theme(plot.title = element_text(size = 13, face = "bold"))
  )
  
  # Retornar resultados
  list(
    tendencias_estacoes  = tendencias_estacoes,
    tendencias_classificadas = tendencias_classificadas,
    mapa_tendencias_classificadas = mapa_tendencias_classificadas,
    contagens_tendencia = contagens_tendencia,
    mapa_tendencia_hidrologica = mapa_tendencia_hidrologica
  )
  
}