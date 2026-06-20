# Variável global de controle para toda a rodada
RUN_NAME <- "dourada"

# Passo 1: Dados da ANA e Estatística de Tendência
source("scripts/01_ana_pipeline.R")

# Passo 2: Processamento do Relevo (DEM) e Ajuste Espacial
source("scripts/02_dem_pipeline.R")

# Passo 3: Clima (CHIRPS)
source("scripts/03_chirps_pipeline.R")

# Passo 4: Balanço Hídrico (Residual e Elasticidade)
source("scripts/04_residual_pipeline.R")

# Passo 5: Geração de Gráficos e PDF
source("scripts/05_report.R")

message("Pipeline executado com sucesso para a bacia: ", RUN_NAME)

area_estudo <- sf::st_read(dirs$study_area_path)
inventario <- inventory(stationType = "flu", as_sf = TRUE, aoi = area_estudo)
