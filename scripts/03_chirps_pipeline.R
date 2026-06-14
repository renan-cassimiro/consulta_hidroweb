# =============================================================================
# PIPELINE 03: CLIMA E PRECIPITAÇÃO ESPACIAL (CHIRPS)
# =============================================================================

# 1. Setup e Carregamento de Módulos
source("config/variable_configs.R")
source("R/setup.R")
source("R/chirps.R") # Contém a função de processamento (processar_rasters_chirps_mensal)

dirs <- setup_dirs(run_name = RUN_NAME)

message("Iniciando Fase 5: Extração Zonal do CHIRPS para a bacia ", RUN_NAME)

# 2. Carregar Insumos
# a. Polígonos das bacias (gerados no script 02_dem_pipeline.R)
bacias_sf <- sf::st_read(paste0(dirs$data, "/estacoes_snap.gpkg"), quiet = TRUE)

# b. Diretório onde estão os TIFs do CHIRPS vindos do GEE
dir_chirps <- paste0("input/", RUN_NAME, "/chirps_anual_stack")

# 3. Processamento: Extração Estatística Zonal (exactextractr)
# A função 'processar_rasters_chirps_mensal' (que fica em R/chirps.R) fará o loop 
# pelos arquivos TIF e calculará a média espacial exata para cada polígono de bacia.
df_chirps <- processar_rasters_chirps_mensal(
  dir_rasters = dir_chirps,
  bacias_sf   = bacias_sf,
  col_id      = "station_code" # Coluna que identifica cada bacia
)

# 4. Salvar Resultados
arquivo_saida <- paste0(dirs$data, "/stations_chirps/organized/chirps_bacias.parquet")
dir.create(dirname(arquivo_saida), recursive = TRUE, showWarnings = FALSE)

arrow::write_parquet(df_chirps, arquivo_saida)

message("Extração CHIRPS concluída! Salvo em: ", arquivo_saida)