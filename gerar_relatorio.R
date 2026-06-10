# =============================================================================
# CARREGAMENTO INDEPENDENTE — GERAÇÃO DO RELATÓRIO INTEGRADO
# =============================================================================
#
# Descrição:
#   Reconstrói todos os objetos necessários para renderizar o
#   relatorio_integrado.Rmd sem precisar rodar o pipeline completo.
#
#   Fluxo:
#     1. Setup de diretórios e configurações (igual ao main.R)
#     2. Carrega arquivos estáticos do disco (spatial, snapped, residual, etc.)
#     3. Reconstrói resultados_por_variavel usando APENAS cache local
#        (pula inventário via API e download da ANA)
#     4. Reconstrói consolidado a partir dos parquets salvos
#     5. Renderiza o PDF
#
# Uso:
#   source("carregar_para_relatorio.R")
# =============================================================================

# -----------------------------------------------------------------------------
# 0. MÓDULOS E CONFIGURAÇÕES
# -----------------------------------------------------------------------------
library(here)

source(here("config/variable_configs.R"))
source(here("R/setup.R"))
source(here("R/inventory.R"))
source(here("R/analysis.R"))
source(here("R/summary.R"))
source(here("R/plots.R"))
source(here("R/consolidate.R"))
source(here("R/seasonality.R"))

RUN_NAME         <- "amacro"
VARIAVEIS_ATIVAS <- c("discharge", "water_level", "precipitation")
GERAR_GRAFICOS_TENDENCIA <- TRUE

dirs <- setup_dirs(RUN_NAME)

# -----------------------------------------------------------------------------
# 1. ARQUIVOS ESTÁTICOS — carrega direto do disco
# -----------------------------------------------------------------------------
message("\n====== [1/5] Carregando arquivos estáticos ======")

# Área de estudo e hidrografia
area_estudo <- st_read(dirs$study_area_path, quiet = TRUE)

rios_path   <- path(dirs$input_dir, "ne_10m_rivers_lake_centerlines_bacias_amacro.gpkg")
amacro_path <- path(dirs$input_dir, "AMACRO", "AMACRO.shp")

rios   <- if (file_exists(rios_path))   st_read(rios_path,   quiet = TRUE) else NULL
amacro <- if (file_exists(amacro_path)) st_read(amacro_path, quiet = TRUE) else NULL

# Estações com snap hidrológico (posicao_rede, area_contrib_km2, etc.)
snap_path <- path(dirs$data_dir, paste0(RUN_NAME, "_snapped_stations.gpkg"))
estacoes_snap <- if (file_exists(snap_path)) {
  st_read(snap_path, quiet = TRUE)
} else {
  message("Aviso: snapped_stations não encontrado — agrupamento por posicao_rede indisponível.")
  NULL
}

# Sazonalidade anual de vazão
path_saz <- path(dirs$consolidated_dir,
                 paste0(RUN_NAME, "_sazonalidade_anual_discharge.parquet"))
df_sazonalidade_discharge <- if (file_exists(path_saz)) {
  read_parquet(path_saz)
} else {
  message("Aviso: parquet de sazonalidade não encontrado.")
  NULL
}

# Residual hidrológico CHIRPS × ANA
path_residual <- path(dirs$output_dir, "results",
                      "residual_hidrologico_consolidado.parquet")
df_residual_chirps <- if (file_exists(path_residual)) {
  read_parquet(path_residual)
} else {
  message("Aviso: parquet de residual não encontrado.")
  NULL
}

# Camada spatial consolidada (spatial = inventário enriquecido com tendências)
path_spatial <- path(dirs$consolidated_dir,
                     paste0(RUN_NAME, "_disponibilidade_spatial.parquet"))
if (file_exists(path_spatial)) {
  spatial_consolidado <- st_read_parquet(path_spatial)
  #TODO Gambiarra para converter campos numericos salvados como list
  spatial_consolidado <- spatial_consolidado %>%
    mutate(
      # 1. Trata apenas as listas que contêm dados Numéricos
      across(
        .cols = where(~ is.list(.x) && !inherits(.x, "sfc") && is.numeric(unlist(compact(.x)))), 
        .fns = ~ map_dbl(.x, ~ ifelse(is.null(.x) || length(.x) == 0, NA_real_, as.numeric(.x)))
      ),
      # 2. Trata as listas que contêm Caracteres (Texto)
      across(
        .cols = where(~ is.list(.x) && !inherits(.x, "sfc")), 
        .fns = ~ map_chr(.x, ~ ifelse(is.null(.x) || length(.x) == 0, NA_character_, as.character(.x)))
      )
    )
} else {
  stop("Arquivo spatial consolidado não encontrado: ", path_spatial)
}

# Wide consolidado
path_wide <- path(dirs$consolidated_dir,
                  paste0(RUN_NAME, "_disponibilidade_wide.parquet"))
if (file_exists(path_wide)) {
  
  wide_consolidado <- read_parquet(path_wide)
  #TODO Gambiarra para converter campos numericos salvados como list
  wide_consolidado <- wide_consolidado %>%
    mutate(
      # 1. Trata apenas as listas que contêm dados Numéricos
      across(
        .cols = where(~ is.list(.x) && !inherits(.x, "sfc") && is.numeric(unlist(compact(.x)))), 
        .fns = ~ map_dbl(.x, ~ ifelse(is.null(.x) || length(.x) == 0, NA_real_, as.numeric(.x)))
      ),
      # 2. Trata as listas que contêm Caracteres (Texto)
      across(
        .cols = where(~ is.list(.x) && !inherits(.x, "sfc")), 
        .fns = ~ map_chr(.x, ~ ifelse(is.null(.x) || length(.x) == 0, NA_character_, as.character(.x)))
      )
    )
} else {
  stop("Arquivo wide consolidado não encontrado: ", path_wide)
}

# -----------------------------------------------------------------------------
# 2. RECONSTRÓI resultados_por_variavel — SEM download, SEM inventário via API
# -----------------------------------------------------------------------------
message("\n====== [2/5] Reconstruindo resultados_por_variavel (cache local) ======")

resultados_por_variavel <- map(
  VARIABLE_CONFIGS[VARIAVEIS_ATIVAS],
  function(cfg) {
    
    message(sprintf("\n  --- %s ---", cfg$label))
    var_dirs <- dirs$vars[[cfg$id]]
    # -- Inventário: usa o spatial consolidado salvo, sem chamar a API ---------
    # O spatial consolidado já tem station_code + geometry, que é tudo que
    # gerar_graficos() precisa do $inventario.
    inventario <- spatial_consolidado |>
      select(any_of(c("station_code", "name", "area_km2",
                      "lat", "long", "geometry"))) |>
      distinct(station_code, .keep_all = TRUE)
    
    # -- Dados organizados: lê do cache local ----------------------------------
    ficheiros_existentes <- list.files(
      var_dirs$org_dir, pattern = "\\.parquet$", full.names = TRUE
    )
    
    if (length(ficheiros_existentes) == 0) {
      message(sprintf("  [%s] Nenhum arquivo em cache — variável ignorada.", cfg$label))
      return(NULL)
    }
    
    message(sprintf("  [%s] Carregando %d estações do disco local.",
                    cfg$label, length(ficheiros_existentes)))
    
    dados_org <- map(ficheiros_existentes, read_parquet)
    names(dados_org) <- tools::file_path_sans_ext(basename(ficheiros_existentes))
    
    # -- Seleção por qualidade -------------------------------------------------
    estacoes_sel <- selecionar_estacoes(cfg, dados_org)
    
    # -- Análise estatística ---------------------------------------------------
    resultados <- analisar_todas_estacoes(estacoes_sel, cfg)
    
    # -- Tabela resumo + séries empilhadas ------------------------------------
    tabela_resumo    <- build_tabela_resumo(resultados, cfg)
    dados_empilhados <- build_dados_empilhados(resultados)
    
    # -- Gráficos -------------------------------------------------------------
    graficos <- NULL
    
    if (GERAR_GRAFICOS_TENDENCIA) {
      message(sprintf("  [%s] Gerando gráficos...", cfg$label))
      
      graficos <- gerar_graficos(
        tabela_resumo    = tabela_resumo,
        dados_empilhados = dados_empilhados,
        inventario       = inventario,
        area_estudo      = area_estudo,
        cfg              = cfg,
        rios             = rios,
        amacro           = amacro
      )
      
      graficos$disponibilidade <- estacoes_sel$plot
    }
    
    list(
      cfg                = cfg,
      inventario         = inventario,
      dados_selecionados = estacoes_sel,
      resultados         = resultados,
      tabela_resumo      = tabela_resumo,
      dados_empilhados   = dados_empilhados,
      graficos           = graficos
    )
  }
)

# Remove variáveis que não tiveram cache
resultados_por_variavel <- compact(resultados_por_variavel)

message(sprintf("\n  Variáveis reconstruídas: %s",
                paste(names(resultados_por_variavel), collapse = ", ")))

# -----------------------------------------------------------------------------
# 3. RECONSTRÓI consolidado a partir dos parquets e do spatial já carregado
# -----------------------------------------------------------------------------
message("\n====== [3/5] Reconstruindo objeto consolidado ======")

# Long: lê do parquet se existir, senão reconstrói das tabelas resumo
path_long <- path(dirs$consolidated_dir,
                  paste0(RUN_NAME, "_disponibilidade_long.parquet"))

long_consolidado <- if (file_exists(path_long)) {
  read_parquet(path_long)
} else {
  message("  Parquet long não encontrado — reconstruindo das tabelas resumo.")
  bind_rows(map(resultados_por_variavel, "tabela_resumo"))
}

consolidado <- list(
  long    = long_consolidado,
  wide    = wide_consolidado,
  spatial = spatial_consolidado
)

message("  Objeto consolidado montado.")

# -----------------------------------------------------------------------------
# 4. VERIFICAÇÃO RÁPIDA
# -----------------------------------------------------------------------------
message("\n====== [4/5] Verificação ======")

walk(names(resultados_por_variavel), function(v) {
  n <- nrow(resultados_por_variavel[[v]]$tabela_resumo)
  message(sprintf("  %-15s → %d estações", v, n))
})

message(sprintf("  Spatial: %d feições | Wide: %d colunas",
                nrow(consolidado$spatial), ncol(consolidado$wide)))
message(sprintf("  Sazonalidade: %s",
                if (!is.null(df_sazonalidade_discharge))
                  paste(nrow(df_sazonalidade_discharge), "linhas")
                else "não disponível"))
message(sprintf("  Residual CHIRPS: %s",
                if (!is.null(df_residual_chirps))
                  paste(nrow(df_residual_chirps), "linhas")
                else "não disponível"))

# -----------------------------------------------------------------------------
# 5. RENDERIZA O RELATÓRIO
# -----------------------------------------------------------------------------
message("\n====== [5/5] Renderizando relatório PDF ======")

caminho_template <- here("relatorio_integrado.Rmd")
dir_saida        <- path(dirs$output_dir, "results")
dir_create(dir_saida)
arquivo_pdf      <- paste0("relatorio_integrado_", RUN_NAME, ".pdf")

rmarkdown::render(
  input       = caminho_template,
  output_file = arquivo_pdf,
  output_dir  = dir_saida,
  params      = list(
    resultados_por_variavel = resultados_por_variavel,
    consolidado             = consolidado,
    df_sazonalidade         = df_sazonalidade_discharge,
    df_residual             = df_residual_chirps,
    estacoes_snap           = estacoes_snap,
    area_estudo             = area_estudo,
    rios                    = rios,
    run_name                = RUN_NAME
  ),
  envir  = new.env(parent = globalenv()),
  quiet  = FALSE
)

message(sprintf("\n[OK] Relatório gerado: %s/%s", dir_saida, arquivo_pdf))