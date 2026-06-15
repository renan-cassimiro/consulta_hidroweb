source("R/residual_hidrologico.R")
source("R/setup.R")

dirs <- setup_dirs(run_name = RUN_NAME)

# Sugestão: crie uma "variável falsa" na sua arquitetura chamada "chirps"
pasta_chirps_organizado <- here::here(dirs$data_dir, "stations_chirps", "organized")
pasta_resultados <- here::here(dirs$data_dir, "results")

# Executa o modelo
tabela_reportagem <- analisar_residual_hidrologico(
  dir_vazao  = dirs$vars$discharge$org_dir,
  col_vazao  = "stream_flow_m3_s", # Insira o nome correto da coluna do seu parquet da ANA
  dir_chirps = pasta_chirps_organizado,
  dir_saida  = pasta_resultados
)
