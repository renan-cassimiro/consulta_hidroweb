# =============================================================================
# PIPELINE 03: CLIMA E PRECIPITAÇÃO ESPACIAL (CHIRPS)
# =============================================================================

# 1. Setup e Carregamento de Módulos
source("config/variable_configs.R")
source("R/setup.R")
source("R/chirps_climate.R") # Contém a função de processamento (processar_rasters_chirps_mensal)

dirs <- setup_dirs(run_name = RUN_NAME)

message("Iniciando Fase 5: Extração Zonal do CHIRPS para a bacia ", RUN_NAME)

# 2. Carregar Insumos
# a. Polígonos das bacias (gerados no script 02_dem_pipeline.R)
bacias_sf <- sf::st_read(path(dirs$data,"bacias_contribuicao_cumulativa.gpkg"), quiet = TRUE)

# b. Diretório onde estão os TIFs do CHIRPS vindos do GEE
dir_chirps <- paste0("input/", RUN_NAME, "/chirps_anual_stack")


# 2. Definir a pasta onde o CHIRPS vai ficar guardado
# Sugestão: crie uma "variável falsa" na sua arquitetura chamada "chirps"
pasta_chirps_organizado <- here::here(dirs$data_dir, "stations_chirps", "organized")

# 3. Processamento: Extração Estatística Zonal (exactextractr)
# A função 'processar_rasters_chirps_mensal' (que fica em R/chirps.R) fará o loop 
# pelos arquivos TIF e calculará a média espacial exata para cada polígono de bacia.
processar_rasters_chirps_mensal(dir_rasters = dir_chirps, bacias_sf = bacias_sf,
  dir_saida = pasta_chirps_organizado)

message("Extração CHIRPS concluída! Salvo em: ", arquivo_saida)