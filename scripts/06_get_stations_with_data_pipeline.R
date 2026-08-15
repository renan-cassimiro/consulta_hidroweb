source("config/session_configs.R")
source("config/variable_configs.R")
source("R/setup.R")

dirs <- setup_dirs(run_name = RUN_NAME)


for (var_id in names(VARIABLE_CONFIGS)) {
  cfg <- VARIABLE_CONFIGS[[var_id]]
  var_dir <- dirs$vars[[var_id]] 
  
  inventario <- if (fs::file_exists(var_dir$inventario)) arrow::read_parquet(var_dir$inventario) else NULL
  
  ficheiros_existentes <- list.files(var_dir$org_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(ficheiros_existentes) == 0) return(NULL)
  
  dados_org <- purrr::map(ficheiros_existentes, arrow::read_parquet)
  names(dados_org) <- tools::file_path_sans_ext(basename(ficheiros_existentes))
  
  estacoes <- str_extract(names(dados_org), "\\d+$")
  
  print(estacoes)
  
  print(nrow(inventario))
  
  inventario_filtrado <- inventario %>% filter(station_code %in% estacoes)
  
  print(nrow(inventario_filtrado))
  
  write_parquet(inventario_filtrado, path(var_dir$org_inventario), compression = "zstd")
}
