# 1. Carrega dependências de configuração e módulos de lógica
source("config/session_configs.R")
source("config/threshold_configs.R")
source("R/setup.R")
source("R/analysis.R")
source("R/summary.R")
source("R/plots.R")
source("R/consolidate.R") # Carrega a nova função desenvolvida

# Definições Locais de Execução (Sobrescreve se necessário)
VARIAVEIS_ATIVAS <- c("discharge", "water_level", "precipitation")

dirs <- setup_dirs(run_name = RUN_NAME)

# 2. Executa a leitura robusta mapeada diretamente do Cache
dados_cache <- ler_consolidado(
  run_name                 = RUN_NAME,
  active_vars              = VARIAVEIS_ATIVAS,
  gerar_graficos_tendencia = TRUE,
  dirs                     = dirs
)

# 3. Preparação de ambiente de saída e Renderização do PDF
message("\n====== [Compilação] Renderizando relatório integrado via Rmd ======")

caminho_template <- here("templates/relatorio_integrado.Rmd")
dir_saida        <- fs::path(dirs$output_dir, "results")
fs::dir_create(dir_saida)
arquivo_pdf      <- paste0("relatorio_integrado_", RUN_NAME, ".pdf")

rmarkdown::render(
  input       = caminho_template,
  output_file = arquivo_pdf,
  output_dir  = dir_saida,
  params      = list(
    resultados_por_variavel = dados_cache$resultados_por_variavel,
    consolidado             = dados_cache$consolidado,
    df_sazonalidade         = dados_cache$df_sazonalidade_discharge,
    df_residual             = dados_cache$df_residual_chirps,
    estacoes_snap           = dados_cache$estacoes_snap,
    area_estudo             = dados_cache$area_estudo,
    rios                    = dados_cache$rios,
    run_name                = RUN_NAME
  ),
  envir  = new.env(parent = globalenv()),
  quiet  = FALSE
)

message("\n>>> Success! Relatório gerado dinamicamente em: ", dir_saida, "/", arquivo_pdf)
