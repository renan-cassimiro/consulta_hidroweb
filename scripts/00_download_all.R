# No início do script 01_ana_pipeline.R
source("config/session_configs.R")
source("config/variable_configs.R")
source("R/setup.R")
source("R/inventory.R")
source("R/analysis.R")
source("R/summary.R")
source("R/consolidate.R")

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
# 
    df_raw <- download_station_data(cfg, inv)
    
    # Persiste dados brutos
    walk2(df_raw, names(df_raw),
          ~ write_parquet(.x, path(var_dir$raw_dir, paste0(RUN_NAME, "_", .y, ".parquet")),
                          compression = "zstd"))
  
  }
}

states_run_name <- c("amacro")

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

states_run_name <- c("jacare_guacu")

# 2. Execução por Variável (Vazão, Cota, Chuva)
resultados_por_variavel <- list()

states <- c("SAO PAULO")

for (run_name in states_run_name) {
  message(sprintf("\n====== %s ======", run_name))
  dirs <- setup_dirs(run_name = run_name)
  
  for (var_id in names(VARIABLE_CONFIGS)) {
    cfg <- VARIABLE_CONFIGS[[var_id]]
    var_dir <- dirs$vars[[var_id]]
    
    file_paths <- dir_ls(var_dir$raw_dir)
    message(sprintf("\n====== %s ======", toupper(var_id)))
    
    # 1. Identifica todos os arquivos parquet do 'run_name' específico
    files <- dir_ls(var_dir$org_dir, regexp = paste0(run_name, "_.*\\.parquet$"))
    print(paste0(run_name, "_(.*)\\.parquet"))
    print(names(files))

        # 2. Lê os arquivos e reconstrói a lista com os nomes originais
    org_data <- map(files, read_parquet) %>% 
      set_names(str_match(names(.), paste0(run_name, "_(.*)\\.parquet"))[, 2])
    print("foi não")
    
    selected_data <- selecionar_estacoes(cfg, org_data)
# 
    # Salvar parquets intermediários
    # arrow::write_parquet(df_limpo, paste0(dirs$data, "/", var_id, "_limpo.parquet"))
    
    # Fase 2: Análise
    resultados <- analisar_todas_estacoes(cfg, selected_data)
    
    # -- 4e. Tabela resumo + séries empilhadas --------------------------------
    tabela_resumo    <- build_tabela_resumo(resultados, cfg)
    dados_empilhados <- build_dados_empilhados(resultados)
    # write_parquet(tabela_resumo, cfg$var_dirs$resumo)
    # message(sprintf("[%s] Resumo salvo: %d estações", cfg$label, nrow(tabela_resumo)))
    
    
    resultados_por_variavel[[var_id]] <- list(
      cfg              = cfg,
      resultados       = resultados,
      tabela_resumo    = tabela_resumo,
      dados_empilhados = dados_empilhados)
    
  }
}

resumos_list     <- map(resultados_por_variavel, "tabela_resumo")

for (run_name in states_run_name) {
  message(sprintf("\n====== %s ======", run_name))
  dirs <- setup_dirs(run_name = run_name)
  
  for (var_id in names(VARIABLE_CONFIGS)) {
    cfg <- VARIABLE_CONFIGS[[var_id]]
    var_dir <- dirs$vars[[var_id]]
    
    inv <- obter_inventario(cfg, states)
    
    area_estudo <- sf::st_read(dirs$study_area_path, quiet = TRUE)
    area_estudo <- st_transform(area_estudo, st_crs(inv))
    
    inventario_area <- st_filter(inv, area_estudo, .predicate = st_intersects)
    
    resultados_por_variavel[[var_id]] <- list(
      cfg              = cfg,
      inventario       = inventario_area)
    
  }
}

inventario_list <- map(resultados_por_variavel, "inventario")

consolidado <- consolidar_resultados(resumos_list = resumos_list, inventario_list = inventario_list)

salvar_consolidado(consolidado, dirs$consolidated_dir, RUN_NAME)



