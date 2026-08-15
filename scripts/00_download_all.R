# No início do script 01_ana_pipeline.R
source("config/session_configs.R")
source("config/variable_configs.R")
source("R/setup.R")
source("R/inventory.R")

dirs <- setup_dirs(run_name = RUN_NAME)

states <- c("ACRE", "ALAGOAS", "AMAPÁ", "AMAZONAS", "BAHIA", "CEARÁ", 
            "DISTRITO FEDERAL", "ESPÍRITO SANTO", "GOIÁS", "MARANHÃO", 
            "MATO GROSSO", "MATO GROSSO DO SUL", "MINAS GERAIS_F", "PARÁ", 
            "PARAÍBA", "PARANÁ_F", "PERNAMBUCO", "PIAUÍ", "RIO DE JANEIRO",
            "RIO GRANDE DO NORTE", "RIO GRANDE DO SUL", "RONDÔNIA", "RORAIMA",
            "SANTA CATARINA", "SÃO PAULO", "SERGIPE", "TOCANTINS")

for (state in states) {
  for (var_id in names(VARIABLE_CONFIGS)) {
    cfg <- VARIABLE_CONFIGS[[var_id]]
    var_dir <- dirs$vars[[var_id]]
    
    message(sprintf("\n====== %s ======", toupper(var_id)))
    
    # Fase 1: Ingestão
    inv <- obter_inventario(cfg, state)
    
    df_raw <- download_station_data(cfg, inv)
    
    # Persiste dados brutos
    walk2(df_raw, names(df_raw),
          ~ write_parquet(.x, path(var_dir$raw_dir, paste0(RUN_NAME, "_", .y, ".parquet")),
                          compression = "zstd"))
  
  }
}

states_run_name <- c("sao_paulo")

for (run_name in states_run_name) {

  message(sprintf("\n====== %s ======", run_name))
  dirs <- setup_dirs(run_name = run_name)
  
  for (var_id in names(VARIABLE_CONFIGS)) {
    cfg <- VARIABLE_CONFIGS[[var_id]]
    var_dir <- dirs$vars[[var_id]]
    file_paths <- dir_ls(var_dir$raw_dir)
    message(sprintf("\n====== %s ======", toupper(var_id)))
    
    df_raw <- map(file_paths, ~ read_parquet(.x))
    
    names(df_raw) <- file_paths |> 
      path_file() |> 
      path_ext_remove() |> 
      stringr::str_remove(paste0(run_name, "_"))

    org_data <- organize_station_data(cfg, df_raw)

    # Persiste dados organizados
    walk2(org_data, names(org_data),
          ~ write_parquet(.x, path(var_dir$org_dir, paste0(run_name, "_", .y, ".parquet")),
                          compression = "zstd"
          )
    )
    
    message(sprintf("[%s] Download concluído: %d estações salvas em %s", cfg$label, length(org_data), dirs$var_dirs$org_dir))
  }
}
