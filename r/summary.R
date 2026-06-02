# =============================================================================
# TABELA RESUMO
# =============================================================================
#
# Descrição:
#   Constrói a tabela de métricas de tendência a partir dos resultados
#   de analisar_todas_estacoes(). Também agrega as séries para visualização.
# =============================================================================


# -----------------------------------------------------------------------------
#' Constrói a tabela resumo de tendências para uma variável
#'
#' @param resultados  Lista retornada por analisar_todas_estacoes().
#' @param cfg         Lista de configuração da variável.
#'
#' @return tibble com uma linha por estação e colunas de métricas estatísticas.
# -----------------------------------------------------------------------------
build_tabela_resumo <- function(resultados, cfg) {
  
  map_dfr(resultados, function(x) {
    tibble(
      station_code    = x$station_code,
      variable        = cfg$id,
      tau_mk          = round(x$mann_kendall$tau, 4),
      p_valor_mk      = round(x$mann_kendall$sl,  4),
      slope_sen       = round(as.numeric(x$sens_slope$estimates), 4),
      p_valor_sen     = round(x$sens_slope$p.value, 4),
      ponto_mudanca   = round(x$pettitt$estimate, 0),
      p_valor_pettitt = round(x$pettitt$p.value,  4),
      n_anomalias     = x$n_anomalias,
      n_extremos      = x$n_extremos,
      tendencia       = case_when(
        x$mann_kendall$sl < 0.05 & x$sens_slope$estimates < 0 ~ "Negativa",
        x$mann_kendall$sl < 0.05 & x$sens_slope$estimates > 0 ~ "Positiva",
        .default = "Não significativa"
      )
    )
  })
}


# -----------------------------------------------------------------------------
#' Empilha as séries mensais de todas as estações em formato longo
#'
#' @param resultados  Lista retornada por analisar_todas_estacoes().
#'
#' @return tibble longo com colunas: date, <value_col>, trend, station_code.
# -----------------------------------------------------------------------------
build_dados_empilhados <- function(resultados) {
  map_dfr(resultados, function(x) {
    x$dados |> mutate(station_code = x$station_code,
                      variable      = x$variable)
  })
}