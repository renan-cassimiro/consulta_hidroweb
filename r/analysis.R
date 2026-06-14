# =============================================================================
# ANÁLISE ESTATÍSTICA DE ESTAÇÕES
# =============================================================================
#
# Descrição:
#   Funções de análise estatística de tendência para séries hidrológicas.
#   A função principal analisar_estacao() é genérica: recebe o nome da coluna
#   de valor via argumento, funcionando para vazão, cota e precipitação sem
#   código duplicado.
# =============================================================================


# -----------------------------------------------------------------------------
#' Análise estatística completa de uma estação hidrológica
#'
#' Agrega a série diária para médias mensais, aplica decomposição STL e
#' executa os testes de Mann-Kendall, Yue-Pilon, slope de Sen e Pettitt.
#' Anomalias são classificadas por z-score em relação à climatologia mensal.
#'
#' @param df           data.frame com colunas `date` e `<value_col>`.
#' @param station_code character. Código ANA da estação.
#' @param value_col    character. Nome da coluna de valor numérico
#'                     (ex: "stream_flow_m3_s", "rainfall_mm", "water_level_cm").
#'
#' @return Lista com:
#'   $station_code, $variable, $dados (mensal com trend/anomalias),
#'   $decomposicao, $mann_kendall, $yue_pilon, $sens_slope,
#'   $pettitt, $n_anomalias, $n_extremos
# -----------------------------------------------------------------------------
analisar_estacao <- function(df, station_code, value_col) {
  
  # Garante que value_col existe no data.frame
  if (!value_col %in% names(df)) {
    stop(sprintf(
      "Coluna '%s' não encontrada na estação %s. Colunas disponíveis: %s",
      value_col, station_code, paste(names(df), collapse = ", ")
    ))
  }
  
  # --- 1. Agregação para médias mensais --------------------------------------
  dt_mensal <- df |>
    mutate(
      date = as.Date(date),
      ano  = year(date),
      mes  = month(date)
    ) |>
    group_by(ano, mes) |>
    summarise(
      value = mean(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(!!value_col := value) |>
    mutate(date = as.Date(paste(ano, mes, "01", sep = "-"))) |>
    arrange(date) |>
    filter(!is.na(.data[[value_col]]))
  
  # Série numérica para os testes (evita repetição de .data[[]])
  serie <- dt_mensal[[value_col]]
  
  # --- 2. Decomposição STL ---------------------------------------------------
  ts_serie <- ts(
    serie,
    frequency = 12,
    start     = c(min(dt_mensal$ano), min(dt_mensal$mes))
  )
  
  decomp <- stl(ts_serie, s.window = "periodic")
  
  dt_mensal <- dt_mensal |>
    mutate(
      trend = as.numeric(decomp$time.series[, "trend"]),
      resid = as.numeric(decomp$time.series[, "remainder"])
    )
  
  # --- 3. Testes estatísticos ------------------------------------------------
  mk     <- MannKendall(dt_mensal$trend)
  mk_mod <- zyp.trend.vector(serie, method = "yuepilon")
  sen    <- sens.slope(serie)
  
  # --- 4. Anomalias por z-score (climatologia mensal) -----------------------
  clim <- dt_mensal |>
    group_by(mes) |>
    summarise(
      media = mean(.data[[value_col]], na.rm = TRUE),
      sd    = sd(.data[[value_col]],   na.rm = TRUE),
      .groups = "drop"
    )
  
  dt_mensal <- dt_mensal |>
    left_join(clim, by = "mes") |>
    mutate(
      z        = (.data[[value_col]] - media) / sd,
      anomalia = case_when(
        abs(z) > 2 ~ "severa",
        abs(z) > 1 ~ "moderada",
        .default   = "normal"
      ),
      extremo  = abs(z) > 3,
      moderada = abs(z) > 2 & abs(z) <= 3
    )
  
  # --- 5. Ponto de mudança (Pettitt) ----------------------------------------
  pettitt <- pettitt.test(serie)
  
  # --- 6. Retorno ------------------------------------------------------------
  list(
    station_code = station_code,
    variable     = value_col,
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


# -----------------------------------------------------------------------------
#' Aplica analisar_estacao() a todas as estações selecionadas
#'
#' @param estacoes_selecionadas  Resultado de selecionar_estacoes() (lista com $series).
#' @param cfg                    Lista de configuração da variável.
#'
#' @return Lista de resultados, um por estação.
# -----------------------------------------------------------------------------
analisar_todas_estacoes <- function(cfg, estacoes_selecionadas) {
  
  message(sprintf("[%s] Analisando %d estações...",
                  cfg$label, length(estacoes_selecionadas$series)))
  
  map2(
    estacoes_selecionadas$series,
    names(estacoes_selecionadas$series),
    ~ analisar_estacao(.x, .y, value_col = cfg$value_col)
  )
}