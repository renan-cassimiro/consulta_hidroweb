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
    mutate(row_id = paste0(variable, station_code))|>
    pivot_wider(
      id_cols = c(row_id, station_code),
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
      left_join(resumo_wide, by = "station_code") |>
      filter(!is.na(row_id))
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
# =============================================================================
# MÓDULO DE LEITURA DE CACHE E RECONSTRUÇÃO DE DADOS
# =============================================================================

#' Auxiliar interna para corrigir distorções de colunas salvas como listas no Parquet
corrigir_colunas_lista <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  df %>%
    mutate(
      # 1. Trata apenas as listas que contêm dados Numéricos
      across(
        .cols = where(~ is.list(.x) && !inherits(.x, "sfc") && is.numeric(unlist(purrr::compact(.x)))), 
        .fns = ~ purrr::map_dbl(.x, ~ ifelse(is.null(.x) || length(.x) == 0, NA_real_, as.numeric(.x)))
      ),
      # 2. Trata as listas que contêm Caracteres (Texto)
      across(
        .cols = where(~ is.list(.x) && !inherits(.x, "sfc")), 
        .fns = ~ purrr::map_chr(.x, ~ ifelse(is.null(.x) || length(.x) == 0, NA_character_, as.character(.x)))
      )
    )
}

#' Reconstrói e lê todos os objetos do cenário para renderização de relatórios
ler_consolidado <- function(run_name = RUN_NAME, 
                            active_vars = VARIAVEIS_ATIVAS, 
                            gerar_graficos_tendencia = TRUE,
                            dirs = setup_dirs(run_name)) {
  
  message("\n====== [1/4] Carregando arquivos estáticos locais ======")
  # 1. Camadas Geográficas Básicas
  area_estudo <- sf::st_read(dirs$study_area_path, quiet = TRUE)
  
  rios_path   <- fs::path(dirs$input_dir, "ne_10m_rivers_lake_centerlines_bacias_amacro.gpkg")
  amacro_path <- fs::path(dirs$input_dir, "AMACRO", "AMACRO.shp")
  
  rios   <- if (fs::file_exists(rios_path))   sf::st_read(rios_path,   quiet = TRUE) else NULL
  amacro <- if (fs::file_exists(amacro_path)) sf::st_read(amacro_path, quiet = TRUE) else NULL
  
  # 2. Estações com Snap (Busca no diretório base ou no subdiretório do DEM se houver)
  #TODO Salvar em um lugar só
  snap_path_old <- fs::path(dirs$data_dir, paste0(run_name, "_snapped_stations.gpkg"))
  snap_path_dem <- fs::path(dirs$data_dir, "dem", "estacoes_snap.gpkg")
  snap_path     <- if (fs::file_exists(snap_path_dem)) snap_path_dem else snap_path_old
  
  estacoes_snap <- if (fs::file_exists(snap_path)) {
    sf::st_read(snap_path, quiet = TRUE)
  } else {
    message("Aviso: estacoes_snap não encontrado. Agrupamento por rede indisponível.")
    NULL
  }
  
  # 3. Sazonalidade e Residual
  path_saz <- fs::path(dirs$consolidated_dir, paste0(run_name, "_sazonalidade_anual_discharge.parquet"))
  df_sazonalidade_discharge <- if (fs::file_exists(path_saz)) arrow::read_parquet(path_saz) else NULL
  
  path_residual <- fs::path(dirs$output_dir, "results", "residual_hidrologico_consolidado.parquet")
  df_residual_chirps <- if (fs::file_exists(path_residual)) arrow::read_parquet(path_residual) else NULL
  
  # 4. Tabelas Consolidadas (Aplicando a correção de listas)
  path_spatial <- fs::path(dirs$consolidated_dir, paste0(run_name, "_disponibilidade_spatial.parquet"))
  if (!fs::file_exists(path_spatial)) stop("Arquivo spatial consolidado ausente: ", path_spatial)
  spatial_consolidado <- sfarrow::st_read_parquet(path_spatial) |> corrigir_colunas_lista()
  
  
    path_wide <- fs::path(dirs$consolidated_dir, paste0(run_name, "_disponibilidade_wide.parquet"))
  if (!fs::file_exists(path_wide)) stop("Arquivo wide consolidado ausente: ", path_wide)
  wide_consolidado <- arrow::read_parquet(path_wide) |> corrigir_colunas_lista()
  
  # -----------------------------------------------------------------------------
  message("\n====== [2/4] Reconstruindo resultados por variável (cache local) ======")
  
  resultados_por_variavel <- purrr::map(
    VARIABLE_CONFIGS[active_vars],
    function(cfg) {
      message(sprintf("  -> Processando cache de: %s", cfg$label))
      
      var_dirs <- dirs$vars[[cfg$id]]
      
      inventario <- spatial_consolidado |>
        dplyr::select(dplyr::any_of(c("station_code", "name", "area_km2", "lat", "long", "geometry"))) |>
        dplyr::distinct(station_code, .keep_all = TRUE)
      
      ficheiros_existentes <- list.files(var_dirs$org_dir, pattern = "\\.parquet$", full.names = TRUE)
      if (length(ficheiros_existentes) == 0) return(NULL)
      
      dados_org <- purrr::map(ficheiros_existentes, arrow::read_parquet)
      names(dados_org) <- tools::file_path_sans_ext(basename(ficheiros_existentes))
      
      estacoes_sel     <- selecionar_estacoes(cfg, dados_org)
      resultados       <- analisar_todas_estacoes(cfg, estacoes_sel)
      tabela_resumo    <- build_tabela_resumo(resultados, cfg)
      dados_empilhados <- build_dados_empilhados(resultados)
      
      graficos <- NULL
      if (gerar_graficos_tendencia) {
        graficos <- gerar_graficos(
          tabela_resumo = tabela_resumo, dados_empilhados = dados_empilhados,
          inventario = inventario, area_estudo = area_estudo,
          cfg = cfg, rios = rios, amacro = amacro
        )
        graficos$disponibilidade <- estacoes_sel$plot
      }
      
      list(
        cfg = cfg, inventario = inventario, dados_selecionados = estacoes_sel,
        resultados = resultados, tabela_resumo = tabela_resumo,
        dados_empilhados = dados_empilhados, graficos = graficos
      )
    }
  ) |> purrr::compact()
  
  # -----------------------------------------------------------------------------
  message("\n====== [3/4] Montando objeto estruturado consolidado ======")
  
  # path_long <- fs::path(dirs$consolidated_dir, paste0(run_name, "_disponibilidade_long.parquet"))
  # long_consolidado <- if (fs::file_exists(path_long)) {
  #   arrow::read_parquet(path_long)
  # } else {
  #   dplyr::bind_rows(purrr::map(resultados_por_variavel, "tabela_resumo"))
  # }
  # 
  
  resumos_list     <- map(resultados_por_variavel, "tabela_resumo")
  inventarios_list <- map(resultados_por_variavel, "inventario")
  
  consolidado <- consolidar_resultados(
    resumos_list    = resumos_list,
    inventario_list = inventarios_list)
  
  salvar_consolidado(consolidado, dirs$consolidated_dir, RUN_NAME)
  # 
  # 
  # consolidado <- list(
  #   long    = long_consolidado,
  #   wide    = wide_consolidado,
  #   spatial = spatial_consolidado
  # )
  
  # -----------------------------------------------------------------------------
  message("\n====== [4/4] Sumário de Verificação Técnica ======")
  purrr::walk(names(resultados_por_variavel), function(v) {
    n <- nrow(resultados_por_variavel[[v]]$tabela_resumo)
    message(sprintf("  %-15s → %d estações validadas", v, n))
  })
  
  # Retorna tudo envelopado em uma lista estruturada pronta para o Rmd
  return(list(
    resultados_por_variavel   = resultados_por_variavel,
    consolidado               = consolidado,
    df_sazonalidade_discharge = df_sazonalidade_discharge,
    df_residual_chirps        = df_residual_chirps,
    estacoes_snap             = estacoes_snap,
    area_estudo               = area_estudo,
    rios                      = rios,
    amacro                    = amacro
  ))
}