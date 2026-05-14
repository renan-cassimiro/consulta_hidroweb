# =============================================================================
# FUNÇÕES DE ANÁLISE HIDROLÓGICA
# =============================================================================
# 
# Autor:   Renan Cassimiro Brito
# Data:    2026-05-13
# 
# Descrição:
#   Funções auxiliares para análise estatística de séries de vazão e geração
#   de visualizações. Chamadas pelo script principal main.R.
#
# Funções exportadas:
#   analisar_estacao(df, station_code)       — análise completa de uma estação
#   gerar_graficos(tabela_resumo,            — produz todos os gráficos do
#                  dados_empilhados,           relatório como lista de objetos
#                  inventario)                 ggplot
#
# Dependências (carregadas no main.R):
#   Kendall, trend, zyp, lubridate, tidyverse, sf, patchwork, scales, fs, here
# =============================================================================


# =============================================================================
# analisar_estacao
# =============================================================================
#' Aplica análise estatística completa a uma estação fluviométrica
#'
#' Recebe a série diária de vazão de uma estação, agrega para médias mensais,
#' realiza decomposição STL e aplica os seguintes testes:
#'   - Mann-Kendall clássico (na componente de tendência STL)
#'   - Mann-Kendall com correção Yue-Pilon (na série original, para autocorrelação)
#'   - Estimador de slope de Sen (na série original)
#'   - Teste de Pettitt (detecção de ponto de mudança)
#'
#' Anomalias são classificadas por z-score em relação à climatologia mensal:
#'   |z| > 1 → moderada | |z| > 2 → severa | |z| > 3 → extremo
#'
#' @param df           data.frame com colunas `date` e `stream_flow_m3_s`
#' @param station_code character. Código ANA da estação (8 dígitos)
#'
#' @return Lista com os elementos:
#'   $station_code   — código da estação
#'   $dados          — data.frame mensal com tendência, resíduo e anomalias
#'   $decomposicao   — objeto stl com as 3 componentes da decomposição
#'   $mann_kendall   — resultado de MannKendall() (tau, sl)
#'   $yue_pilon      — resultado de zyp.trend.vector() (correção de autocorrelação)
#'   $sens_slope     — resultado de sens.slope() (estimates, p.value)
#'   $pettitt        — resultado de pettitt.test() (estimate, p.value)
#'   $n_anomalias    — nº de meses com anomalia severa (|z| > 2)
#'   $n_extremos     — nº de meses com anomalia extrema (|z| > 3)
# =============================================================================

analisar_estacao <- function(df, station_code) {
  
  # --- 1. Agregação para médias mensais ---------------------------------------
  dt_mensal <- df |>
    mutate(
      date = as.Date(date),
      ano  = year(date),
      mes  = month(date)
    ) |>
    group_by(ano, mes) |>
    summarise(
      stream_flow_m3_s = mean(stream_flow_m3_s, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(date = as.Date(paste(ano, mes, "01", sep = "-"))) |>
    arrange(date) |>
    filter(!is.na(stream_flow_m3_s))
  
  # --- 2. Decomposição STL ----------------------------------------------------
  # Frequência = 12 (série mensal); s.window = "periodic" assume sazonalidade
  # estável ao longo do tempo
  ts_vazao <- ts(dt_mensal$stream_flow_m3_s,
                 frequency = 12,
                 start = c(min(dt_mensal$ano), min(dt_mensal$mes)))
  
  decomp <- stl(ts_vazao, s.window = "periodic")
  
  dt_mensal <- dt_mensal |>
    mutate(
      trend = as.numeric(decomp$time.series[, "trend"]),
      resid = as.numeric(decomp$time.series[, "remainder"])
    )
  
  # --- 3. Testes estatísticos -------------------------------------------------
  
  # Mann-Kendall aplicado à componente de tendência (sem sazonalidade nem ruído)
  mk <- MannKendall(dt_mensal$trend)
  
  # Yue-Pilon: correção para autocorrelação serial (aplicado à série original)
  mk_mod <- zyp.trend.vector(dt_mensal$stream_flow_m3_s, method = "yuepilon")
  
  # Slope de Sen: estimativa não-paramétrica da taxa de mudança (série original)
  sen <- sens.slope(dt_mensal$stream_flow_m3_s)
  
  # --- 4. Anomalias por z-score (climatologia mensal) ------------------------
  clim <- dt_mensal |>
    group_by(mes) |>
    summarise(
      media = mean(stream_flow_m3_s, na.rm = TRUE),
      sd    = sd(stream_flow_m3_s,   na.rm = TRUE),
      .groups = "drop"
    )
  
  dt_mensal <- dt_mensal |>
    left_join(clim, by = "mes") |>
    mutate(
      z        = (stream_flow_m3_s - media) / sd,
      anomalia = case_when(
        abs(z) > 2 ~ "severa",
        abs(z) > 1 ~ "moderada",
        .default   = "normal"
      ),
      extremo  = abs(z) > 3,
      moderada = abs(z) > 2 & abs(z) <= 3
    )
  
  # --- 5. Ponto de mudança (Pettitt) -----------------------------------------
  pettitt <- pettitt.test(dt_mensal$stream_flow_m3_s)
  
  # --- 6. Retorno -------------------------------------------------------------
  list(
    station_code = station_code,
    dados        = dt_mensal,
    decomposicao = decomp,
    mann_kendall = mk,
    yue_pilon    = mk_mod,
    sens_slope   = sen,
    pettitt      = pettitt,
    n_anomalias  = sum(dt_mensal$anomalia == "severa"),
    n_extremos   = sum(dt_mensal$extremo)
  )
}


# =============================================================================
# gerar_graficos
# =============================================================================
#' Gera todos os gráficos do relatório hidrológico
#'
#' Produz visualizações a partir da tabela de resumo estatístico e das séries
#' temporais de todas as estações. Os objetos ggplot são retornados em lista
#' para uso no RMarkdown via `params`, sem efeitos colaterais de I/O.
#'
#' Nota: lê arquivos de camadas geográficas (rios, AMACRO) diretamente de
#' RESOURCES_DIR. Variável definida no main.R e acessada do ambiente global.
#' TODO: passar como argumento para eliminar dependência de variável global.
#'
#' @param tabela_resumo    data.frame com colunas: station_code, slope_sen,
#'                         p_valor_mk, tau_mk, tendencia (e demais métricas)
#' @param dados_empilhados data.frame longo com séries mensais de todas as
#'                         estações; colunas: date, stream_flow_m3_s, trend,
#'                         station_code
#' @param inventario       objeto sf com geometria das estações; deve conter
#'                         colunas: station_code, lat, long, geometry
#'
#' @return Lista com objetos ggplot:
#'   $tendencias_estacoes           — barplot horizontal de slope por estação
#'   $tendencias_classificadas      — facets: série original + tendência STL
#'   $mapa_tendencias_classificadas — mapa por categoria de tendência
#'   $contagens_tendencia           — barplot de frequência/proporção
#'   $mapa_tendencia_hidrologica    — mapa com tamanho = |τ| e camadas geo
# =============================================================================

gerar_graficos <- function(tabela_resumo, dados_empilhados, inventario) {
  
  # ---------------------------------------------------------------------------
  # Constantes de estilo (consistência entre todos os gráficos)
  # ---------------------------------------------------------------------------
  CORES_TENDENCIA <- c(
    "Negativa"          = "#d73027",
    "Positiva"          = "#4575b4",
    "Não significativa" = "#969696"
  )
  ORDEM_TENDENCIA <- c("Negativa", "Não significativa", "Positiva")
  
  # Garante ordem dos níveis do fator em toda a função
  tabela_resumo <- tabela_resumo |>
    mutate(tendencia = factor(tendencia, levels = ORDEM_TENDENCIA))
  
  # ---------------------------------------------------------------------------
  # G1. Barplot horizontal: slope de Sen por estação
  # ---------------------------------------------------------------------------
  # Label com valor numérico e símbolo de direção (▲/▼/○)
  tabela_resumo <- tabela_resumo |>
    mutate(label = paste0(
      round(slope_sen, 2),
      case_when(
        tendencia == "Positiva" ~ " ▲",
        tendencia == "Negativa" ~ " ▼",
        .default                = " ○"
      )
    ))
  
  tendencias_estacoes <- ggplot(
    tabela_resumo,
    aes(x = reorder(station_code, slope_sen), y = slope_sen, fill = tendencia)
  ) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = label),
              hjust = ifelse(tabela_resumo$slope_sen < 0, 1.1, -0.1),
              size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_fill_manual(values = c(
      "Positiva"          = "steelblue",
      "Negativa"          = "darkred",
      "Não significativa" = "gray70"
    )) +
    labs(
      title    = "Tendência de vazão por estação",
      subtitle = "Valores em m³/s/ano | ▲ positivo ▼ negativo ○ não significativo",
      x        = "Estação",
      y        = "Tendência (m³/s/ano)",
      fill     = "Tendência"
    ) +
    theme_minimal()
  
  # ---------------------------------------------------------------------------
  # G2. Facets: série temporal original + tendência STL por estação
  # ---------------------------------------------------------------------------
  # titulo_facet inclui código, slope e p-valor para facilitar leitura direta
  dados_com_classificacao <- dados_empilhados |>
    left_join(
      tabela_resumo |> select(station_code, slope_sen, p_valor_mk, tendencia),
      by = "station_code"
    ) |>
    mutate(
      titulo_facet = paste0(
        station_code, "\n",
        round(slope_sen, 2), " m³/s/ano | p = ", round(p_valor_mk, 4),
        case_when(
          tendencia == "Negativa" ~ " ▼",
          tendencia == "Positiva" ~ " ▲",
          .default                = " ○"
        )
      )
    )
  
  tendencias_classificadas <- ggplot(
    dados_com_classificacao,
    aes(x = date, y = stream_flow_m3_s)
  ) +
    geom_line(alpha = 0.3, linewidth = 0.3, color = "gray50") +
    geom_line(aes(y = trend, color = tendencia), linewidth = 1.2) +
    facet_wrap(~titulo_facet, scales = "free_y", ncol = 7) +
    scale_color_manual(values = CORES_TENDENCIA, name = "Tendência") +
    labs(
      title    = "Tendência de vazão por estação — Bacia Amazônica",
      subtitle = "Série original (cinza) | Colorido = tendência STL | ▼ Negativa ▲ Positiva ○ Não significativa",
      x        = "Data",
      y        = "Vazão (m³/s)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y  = element_text(size = 7),
      strip.text   = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      plot.title   = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  # ---------------------------------------------------------------------------
  # G3. Mapa: categoria de tendência por estação
  # ---------------------------------------------------------------------------
  # Junta geometria das estações para plotagem espacial
  dados_com_classificacao_localizacao <- dados_com_classificacao |>
    inner_join(
      inventario |> select(station_code, lat, long, geometry),
      by = "station_code"
    ) |>
    st_as_sf()
  
  rios <- st_read(path(RESOURCES_DIR, "ne_10m_rivers_lake_centerlines_amazonia.gpkg"),
                  quiet = TRUE)
  
  mapa_tendencias_classificadas <- ggplot() +
    geom_sf(data = area_estudo, color = "black", linewidth = 1) +
    geom_sf(data = rios, color = "blue", linewidth = 0.5) +
    geom_sf(data = dados_com_classificacao_localizacao,
            aes(color = tendencia), size = 5) +
    scale_color_manual(values = CORES_TENDENCIA, name = "Tendência") +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y   = element_text(size = 7),
      strip.text    = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      plot.title    = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
  
  # ---------------------------------------------------------------------------
  # G4. Barplot: frequência e proporção de estações por categoria de tendência
  # ---------------------------------------------------------------------------
  contagem_tend <- tabela_resumo |>
    count(tendencia) |>
    mutate(
      prop      = n / sum(n),
      label_bar = paste0(n, "\n(", round(prop * 100, 1), "%)")
    )
  
  contagens_tendencia <- ggplot(
    contagem_tend,
    aes(x = tendencia, y = n, fill = tendencia)
  ) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = label_bar), vjust = -0.3, size = 3.5, lineheight = 0.9) +
    scale_fill_manual(values = CORES_TENDENCIA) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = "Frequência de tendência", x = NULL, y = "Nº de estações") +
    theme_bw(base_size = 14) +
    theme(panel.grid.major.x = element_blank()) +
    plot_annotation(
      title = "Distribuição das estações por categoria de tendência",
      theme = theme(plot.title = element_text(size = 13, face = "bold"))
    )
  
  # ---------------------------------------------------------------------------
  # G5. Mapa temático: tamanho = |τ|, com camadas AMACRO e hidrografia
  # ---------------------------------------------------------------------------
  estacoes_map <- inventario |>
    left_join(tabela_resumo, by = "station_code") |>
    mutate(abs_tau = abs(tau_mk))
  
  amacro <- st_read(path(RESOURCES_DIR, "AMACRO/AMACRO.shp"), quiet = TRUE)
  
  tema_mapa <- theme_void(base_size = 14) +
    theme(
      plot.title       = element_text(face = "bold", size = 16),
      plot.subtitle    = element_text(size = 12, color = "gray40"),
      legend.position  = "right",
      legend.key.width = unit(1, "cm")
    )
  
  mapa_tendencia_hidrologica <- ggplot() +
    geom_sf(data = amacro,      fill = "pink",  linewidth = 0.8) +
    geom_sf(data = rios,        color = "blue", linewidth = 0.5) +
    geom_sf(data = area_estudo, color = "black", linewidth = 1) +
    geom_sf(data = estacoes_map, aes(color = tendencia, size = abs_tau)) +
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
    labs(
      title    = "Direção e força da tendência",
      subtitle = "Cor = categoria | Tamanho = |τ|"
    ) +
    tema_mapa +
    plot_annotation(
      title = "Tendências hidrológicas — Bacia Amazônica (ANA)",
      theme = theme(plot.title = element_text(size = 13, face = "bold"))
    )
  
  # ---------------------------------------------------------------------------
  # Retorno: lista de objetos ggplot para uso no RMarkdown
  # ---------------------------------------------------------------------------
  list(
    tendencias_estacoes           = tendencias_estacoes,
    tendencias_classificadas      = tendencias_classificadas,
    mapa_tendencias_classificadas = mapa_tendencias_classificadas,
    contagens_tendencia           = contagens_tendencia,
    mapa_tendencia_hidrologica    = mapa_tendencia_hidrologica
  )
}