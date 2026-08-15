# =============================================================================
# INVENTÁRIO, DOWNLOAD E ORGANIZAÇÃO
# =============================================================================
#
# Descrição:
#   Funções genéricas para obter o inventário da ANA, baixar as séries
#   históricas e organizar os dados no formato do {hydrobr}.
#   Todas as funções são puras em relação a I/O: recebem caminhos como
#   argumento.
# =============================================================================


# -----------------------------------------------------------------------------
#' Obtém e salva o inventário de estações para uma variável
#'
#' Consulta a API HidroWeb via hydrobr::inventory(), aplica filtro opcional
#' por estações pré-selecionadas e persiste o resultado em parquet.
#'
#' @param cfg           Lista de configuração da variável (de VARIABLE_CONFIGS).
#' @param area_estudo   Objeto sf com o polígono da área de interesse.
#' @param estacoes_filtradas sf ou NULL. Se fornecido, filtra por
#'                           $CodigoEstacao. Default NULL (sem filtro).
#'
#' @return Objeto sf com o inventário de estações.
# -----------------------------------------------------------------------------
obter_inventario <- function(cfg, states, estacoes_filtradas = NULL) {
  
  message(sprintf("[%s] Consultando inventário (stationType = '%s')...",
                  cfg$label, cfg$station_type))
  
  inventario <- inventory(
    states = states,
    stationType = cfg$station_type,
    as_sf       = TRUE
  )
  
  
  #Baixar tudo e filtrar por data, a função acima tem um defeito de não estar 
  #aceitando o AOI
  if (!is.null(estacoes_filtradas)) {
    n_antes <- nrow(inventario)
    inventario <- inventario |>
      filter(station_code %in% estacoes_filtradas$CodigoEstacao)
    message(sprintf("[%s] Filtro aplicado: %d → %d estações.",
                    cfg$label, n_antes, nrow(inventario)))
  }
  
  inventario
}

# -----------------------------------------------------------------------------
#' Baixa as séries históricas de uma variável
#'
#' Executa stationsData()
#'
#' @param cfg         Lista de configuração da variável (de VARIABLE_CONFIGS).
#' @param inventario  Objeto sf retornado por obter_inventario().
#' @param run_name    character. Prefixo usado nos nomes dos arquivos.
#' @param raw_dir     Caminho para salvar dados brutos por estação.
#'
#' @return Lista de data.frames organizados (resultado de organize()),
#'         nomeada pelo código de cada estação.
# -----------------------------------------------------------------------------
download_station_data <- function(cfg, inventory) {
  
  message(sprintf("[%s] Baixando séries históricas (%d estações)...", cfg$label, nrow(inventory)))
  raw_data <- stationsData(inventoryResult = inventory, waterLevel = cfg$water_level)

  return(raw_data)
}



# -----------------------------------------------------------------------------
#' Baixa, organiza e persiste as séries históricas de uma variável
#'
#' Executa stationsData() → organize() e salva cada estação individualmente
#' em parquet (dados brutos e organizados).
#'
#' @param cfg         Lista de configuração da variável (de VARIABLE_CONFIGS).
#' @param raw_data    Lista com os dados das estações.
#'
#' @return Lista de data.frames organizados (resultado de organize()),
#'         nomeada pelo código de cada estação.
# -----------------------------------------------------------------------------
organize_station_data <- function(cfg, raw_data) {
  
  message(sprintf("[%s] Organizando dados...", cfg$label))
  org_data <- organize(raw_data)
  
  return(org_data)
}


# -----------------------------------------------------------------------------
#' Seleciona estações por critérios de qualidade
#'
#' Wrapper sobre hydrobr::selectStations() que usa os parâmetros definidos
#' em cfg$select_stations_params, evitando repetição de argumentos.
#'
#' @param cfg          Lista de configuração da variável.
#' @param dados_org    Lista de data.frames organizados (saída de organize()).
#'
#' @return Lista com $series (estações selecionadas) e $plot (gráfico de
#'         disponibilidade gerado pelo {hydrobr}).
# -----------------------------------------------------------------------------
selecionar_estacoes <- function(cfg, dados_org) {
  
  message(sprintf("[%s] Selecionando estações por qualidade...", cfg$label))
  message(sprintf("Total de estações: %d", length(dados_org)))
  
  params <- cfg$select_stations_params
  
  resultado <- selectStations(
    organizeResult = dados_org,
    mode           = params$mode,
    maxMissing     = params$maxMissing,
    minYears       = params$minYears,
    month          = params$month,
    iniYear        = params$iniYear,
    finYear        = params$finYear,
    consistedOnly  = params$consistedOnly
  )
  
  n_sel <- length(resultado$series)
  message(sprintf("[%s] %d estações selecionadas.", cfg$label, n_sel))
  
  resultado
}