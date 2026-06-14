source("config/variable_configs.R")
source("R/setup.R")
source("R/plots_report.R")

# Carrega os dados processados do disco
dirs <- setup_dirs(run_name = RUN_NAME)
resultados <- ler_consolidado(dirs$consolidated)
df_residual <- arrow::read_parquet(paste0(dirs$results, "/residual.parquet"))

# Renderiza o PDF
rmarkdown::render(
  input = "templates/relatorio_integrado.Rmd",
  output_dir = dirs$report,
  params = list(
    resultados_por_variavel = resultados,
    df_residual = df_residual,
    run_name = RUN_NAME
  )
)