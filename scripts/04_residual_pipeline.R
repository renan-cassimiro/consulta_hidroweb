source("R/residual.R")

# Carregar dados já processados nas etapas anteriores
df_chirps <- arrow::read_parquet(paste0("output/", RUN_NAME, "/data/chirps_bacias.parquet"))
df_vazao <- arrow::read_parquet(paste0("output/", RUN_NAME, "/data/discharge_limpo.parquet"))

# Calcular Residual
df_residual <- analisar_residual_hidrologico(df_chirps, df_vazao)

# Salvar
arrow::write_parquet(df_residual, paste0("output/", RUN_NAME, "/results/residual.parquet"))