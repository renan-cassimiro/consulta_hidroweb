# =============================================================================
# CONSOLIDAÇÃO DOS RESULTADOS
# =============================================================================
#
# Descrição:
#   Junta as tabelas de resumo de todas as variáveis (vazão, cota,
#   precipitação) em dois formatos de saída:
#
#   - Long:  uma linha por (station_code × variable). Útil para filtragem
#            e análises comparativas com dplyr/ggplot2.
#   - Wide:  uma linha por station_code, com sufixo por variável nas colunas.
#            Útil para joins com camadas espaciais e relatórios tabulares.
#
#   A camada espacial consolidada (sf) é derivada do formato wide.
# =============================================================================


# -----------------------------------------------------------------------------
#' Consolida resumos de múltiplas variáveis em formatos long e wide
#'
#' @param resumos_list  Lista nomeada de tibbles (saída de build_tabela_resumo),
#'                      onde o nome de cada elemento é o id da variável
#'                      (ex: list(discharge = ..., precipitation = ...)).
#' @param inventario_list Lista nomeada de objetos sf com o inventário de cada
#'                        variável. Usado para enriquecer com geometria.
#'                        Se NULL, retorna apenas tabelas (sem geometria).
#'
#' @return Lista com:
#'   $long    — tibble longo (station_code, variable, métricas...)
#'   $wide    — tibble wide  (station_code, métrica_discharge, métrica_precipitation, ...)
#'   $spatial — objeto sf com geometria + atributos wide (ou NULL se sem inventário)
# -----------------------------------------------------------------------------
consolidar_resultados <- function(resumos_list, inventario_list = NULL) {
  
  # --- 1. Formato longo -------------------------------------------------------
  # Simplesmente empilha os resumos (já têm a coluna `variable`)
  resumo_long <- bind_rows(resumos_list)
  
  # --- 2. Formato wide --------------------------------------------------------
  # Pivota: cada métrica recebe sufixo com o id da variável
  # ex: slope_sen_discharge, slope_sen_precipitation
  metricas_pivot <- c(
    "tau_mk", "p_valor_mk", "slope_sen", "p_valor_sen",
    "ponto_mudanca", "p_valor_pettitt", "n_anomalias", "n_extremos", "tendencia"
  )
  
  resumo_wide <- resumo_long |>
    select(station_code, variable, all_of(metricas_pivot)) |>
    pivot_wider(
      names_from   = variable,
      values_from  = all_of(metricas_pivot),
      names_glue   = "{.value}_{variable}"
    )
  
  # --- 3. Camada espacial (opcional) -----------------------------------------
  resumo_spatial <- NULL
  
  if (!is.null(inventario_list)) {
    
    # Junta todos os inventários e mantém geometria única por station_code
    # (mesma estação pode aparecer em flu/plu — mantemos só a primeira ocorrência)
    inventario_todos <- bind_rows(inventario_list) |>
      select(station_code, name, area_km2, lat, long, geometry) |>
      distinct(station_code, .keep_all = TRUE) |>
      st_as_sf()
    
    resumo_spatial <- inventario_todos |>
      left_join(resumo_wide, by = "station_code")
  }
  
  list(
    long    = resumo_long,
    wide    = resumo_wide,
    spatial = resumo_spatial
  )
}


# -----------------------------------------------------------------------------
#' Persiste os três formatos consolidados em disco
#'
#' @param consolidado    Saída de consolidar_resultados().
#' @param output_dir     Diretório de saída (consolidated/).
#' @param run_name       Prefixo dos arquivos.
# -----------------------------------------------------------------------------
salvar_consolidado <- function(consolidado, output_dir, run_name) {
  
  write_parquet(
    consolidado$long,
    path(output_dir, paste0(run_name, "_disponibilidade_long.parquet"))
  )
  message("Salvo: consolidado long")
  
  write_parquet(
    consolidado$wide,
    path(output_dir, paste0(run_name, "_disponibilidade_wide.parquet"))
  )
  message("Salvo: consolidado wide")
  
  if (!is.null(consolidado$spatial)) {
    st_write_parquet(
      consolidado$spatial,
      path(output_dir, paste0(run_name, "_disponibilidade_spatial.parquet")),
      compression = "zstd"
    )
    message("Salvo: consolidado spatial (sf + wide)")
  }
  
  invisible(consolidado)
}