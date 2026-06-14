# =============================================================================
# PIPELINE 02: REDE DE DRENAGEM E GRAFO HIDROLÓGICO (DEM)
# =============================================================================

# -----------------------------------------------------------------------------
# Setup, Módulos e Configurações
# -----------------------------------------------------------------------------
source("config/session_configs.R")
source("config/threshold_configs.R")
source("R/setup.R")
source("R/dem.R")

path <- fs::path # Garante escopo correto contra o igraph
dirs <- setup_dirs(run_name = RUN_NAME)

# Cria dinamicamente a pasta interna de trabalho do DEM
DEM_DIR <- path(dirs$data_dir, "dem")
dir.create(DEM_DIR, recursive = TRUE, showWarnings = FALSE)

# MAPEAMENTO DE CAMINHOS INTERMEDIÁRIOS E FINAIS (Usando a estrutura 'dirs')
PATHS_DEM <- list(
  dem_projected = path(DEM_DIR, paste0(RUN_NAME, "_dem_projected.tif")),
  dem_breach    = path(DEM_DIR, paste0(RUN_NAME, "_dem_breach.tif")),
  dem_fill      = path(DEM_DIR, paste0(RUN_NAME, "_dem_fill.tif")),
  d8_pointer    = path(DEM_DIR, paste0(RUN_NAME, "_flow_direction.tif")),
  d8_accum      = path(DEM_DIR, paste0(RUN_NAME, "_flow_accumulation.tif")),
  streams_rast  = path(DEM_DIR, paste0(RUN_NAME, "_drainage_network.tif")),
  stream_links  = path(DEM_DIR, paste0(RUN_NAME, "_stream_links.tif")),
  subbasins     = path(DEM_DIR, paste0(RUN_NAME, "subbasins.tif")),
  
  # Produtos Finais solicitados no seu cabeçalho
  bacias_gpkg   = path(dirs$data_dir, "dem", "bacias_estacoes.gpkg"),
  estacoes_snap = path(dirs$data_dir, "dem", "estacoes_snap.gpkg"),
  grafo_rds     = path(dirs$data_dir, "grafo_hidrologico.rds"),
  analise_v2    = path(dirs$data_dir, "estacoes_analise_v2.gpkg")
)

# -----------------------------------------------------------------------------
# 1. PRÉ-PROCESSAMENTO DO DEM
# -----------------------------------------------------------------------------
message("\n=== Etapa 1: Pré-processamento do DEM ===")

area_estudo <- sf::st_read(dirs$study_area_path, quiet = TRUE)
dem_raw     <- terra::rast(dirs$dem_path)

message("Reprojetando DEM para Albers Equal Area...")
dem_proj <- terra::project(dem_raw, THRESHOLDS$dem_crs, method = "bilinear")
terra::writeRaster(dem_proj, PATHS_DEM$dem_projected, overwrite = TRUE)

message("Executando Breach de depressões...")
whitebox::wbt_breach_depressions_least_cost(
  dem    = PATHS_DEM$dem_projected,
  output = PATHS_DEM$dem_breach,
  dist   = 10,
  fill   = TRUE
)

message("Executando Fill residual...")
whitebox::wbt_fill_depressions_wang_and_liu(
  dem    = PATHS_DEM$dem_breach,
  output = PATHS_DEM$dem_fill
)

# -----------------------------------------------------------------------------
# 2. DIREÇÃO E ACUMULAÇÃO DE FLUXO (D8)
# -----------------------------------------------------------------------------
message("\n=== Etapa 2: Direção e acumulação de fluxo (D8) ===")

whitebox::wbt_d8_pointer(dem = PATHS_DEM$dem_fill, output = PATHS_DEM$d8_pointer)
whitebox::wbt_d8_flow_accumulation(input = PATHS_DEM$dem_fill, output = PATHS_DEM$d8_accum, out_type = "cells")

# -----------------------------------------------------------------------------
# 3. EXTRAÇÃO DA REDE DE DRENAGEM
# -----------------------------------------------------------------------------
message("\n=== Etapa 3: Extração da rede de drenagem ===")

message("Aplicando Limiar de acumulação: ", THRESHOLDS$dem_limiar_accum, " células")
whitebox::wbt_extract_streams(
  flow_accum = PATHS_DEM$d8_accum, 
  output     = PATHS_DEM$streams_rast, 
  threshold  = THRESHOLDS$dem_limiar_accum
)

# -----------------------------------------------------------------------------
# 4. SNAP HIDROLÓGICO DAS ESTAÇÕES
# -----------------------------------------------------------------------------
message("\n=== Etapa 4: Snap hidrológico das estações ===")

# Carrega dados gerados no pipeline anterior (ANA / Fases 1-3)
analysed_stations <- sfarrow::st_read_parquet(
  path(dirs$consolidated_dir, paste0(RUN_NAME, "_disponibilidade_spatial.parquet"))
)

# Aplica o filtro de tamanho de bacia definido nos limiares
estacoes_foco <- analysed_stations |> 
  dplyr::filter(area_km2 > THRESHOLDS$dem_min_area_foco_km2)

# Sincroniza CRS com o DEM antes de processar espacialmente
estacoes_dem <- sf::st_transform(estacoes_foco, terra::crs(terra::rast(PATHS_DEM$d8_accum)))

# Executa o loop estruturado através da nossa função
estacoes_snap <- snap_estacoes_pipeline(
  estacoes_sf  = estacoes_dem,
  streams_rast = terra::rast(PATHS_DEM$streams_rast),
  accum_rast   = terra::rast(PATHS_DEM$d8_accum),
  snap_dist    = THRESHOLDS$dem_snap_dist_m
)

#TODO incluir análise de estações problemáticas (muito distantes)???

# Controle de qualidade na tela
message("Snap concluído. Distância Média: ", round(mean(estacoes_snap$dist_snap_m, na.rm = TRUE), 1), " m")

# Salva o produto intermediário do Snap em disco para o WhiteboxTools ler na sequência
sf::st_write(estacoes_snap, PATHS_DEM$estacoes_snap, delete_dsn = TRUE, quiet = TRUE)

# -----------------------------------------------------------------------------
# 5. DELIMITAÇÃO DAS BACIAS DE CONTRIBUIÇÃO
# -----------------------------------------------------------------------------
message("\n=== Etapa 5: Delimitação das bacias de contribuição ===")
message(sprintf("\n====== INICIANDO DELIMITAÇÃO INDIVIDUAL PARA %d ESTAÇÕES ======", length(codigos_estacoes)))

# wbt_stream_link_identifier(streams = PATHS_DEM$streams_rast, d8_pntr = PATHS_DEM$d8_pointer, output = PATHS_DEM$stream_links)
# wbt_subbasins(d8_pntr = PATHS_DEM$d8_pointer, streams =PATHS_DEM$stream_links, output = PATHS_DEM$subbasins)
# 
# bacias_estacoes <- as.polygons(rast(PATHS_DEM$subbasins),  dissolve = TRUE) |>
#   st_as_sf() |>  st_set_crs(crs(rast(PATHS_DEM$subbasins)))
# 
# delimitar_bacias_estacoes(
#   estacoes_snap_path = PATHS_DEM$estacoes_snap,
#   d8_pointer_path    = PATHS_DEM$d8_pointer,
#   output_path        = PATHS_DEM$bacias_gpkg
# )

codigos_estacoes <- unique(estacoes_snap$station_code)

# Lista para armazenar os polígonos de cada bacia
lista_bacias <- list()
dir_create(path(dirs$output_dir, 'temp'))

# ----------------------------------------------------------------------------
# 2. LOOP POR ESTAÇÃO (Garante o acúmulo real a montante)
# ----------------------------------------------------------------------------
for (i in seq_along(codigos_estacoes)) {
  cod <- codigos_estacoes[i]
  message(sprintf("[%d/%d] Delimitando bacia da estação: %s", i, length(codigos_estacoes), cod))
  
  # Caminhos específicos desta estação
  pt_shp  <- path(dirs$temp_dir, paste0("pt_", cod, ".shp"))
  out_tif <- path(dirs$temp_dir, paste0("wsh_", cod, ".tif"))
  
  # 1. Filtra e isola apenas o ponto desta estação
  ponto_individual <- estacoes_snap |> filter(station_code == cod) |> select(station_code)
  st_write(ponto_individual, pt_shp, delete_dsn = TRUE, quiet = TRUE)
  
  # 2. Executa o Watershed do Whitebox apenas para este ponto
  wbt_watershed(
    d8_pntr  = PATHS_DEM$d8_pointer,
    pour_pts = pt_shp,
    output   = out_tif
  )
  
  # 3. Lê o raster gerado e transforma em polígono se ele existir e for válido
  # if (file_exists(out_tif)) {
  r_bacia <- rast(out_tif)
  
  # Verifica se o raster não está vazio (pode acontecer se o snap falhou drasticamente)
  # if (global(r_bacia, "not_na")$not_na > 0) {
  poligono_sf <- as.polygons(r_bacia) |>
    st_as_sf() |>
    st_transform(4326) |>
    mutate(
      station_code    = cod,
      # Área calculada ainda em projeção plana — reprojetamos só para o cálculo
      area_contrib_km2 = as.numeric(
        st_area(st_transform(geometry, crs(r_bacia)))
      ) / 1e6
    ) |>
    select(station_code, area_contrib_km2, geometry)
  
  
  lista_bacias[[cod]] <- poligono_sf
  # } else {
  # warning(sprintf("A bacia da estação %s gerou um raster vazio. Verifique o snap.", cod))
  # }
  # }
}


# ----------------------------------------------------------------------------
# 3. CONSOLIDANDO AS GEOMETRIAS SOBREPOSTAS
# ----------------------------------------------------------------------------
message("\n[Consolidando polígonos e salvando produto final...]")

bacias_cumulativas_todas <- bind_rows(lista_bacias)

estacoes_snap <- estacoes_snap |>
  left_join(
    bacias_cumulativas_todas |>
      st_drop_geometry() |>
      select(station_code, area_contrib_km2),
    by = "station_code"
  )

st_write(estacoes_snap, PATHS_DEM$estacoes_snap, delete_dsn = TRUE, quiet = TRUE)

# Salva o arquivo final com todas as bacias cumulativas
st_write(bacias_cumulativas_todas, PATHS_DEM$bacias_gpkg, delete_dsn = TRUE)

# Limpa a pasta temporária para poupar espaço em disco
dir_delete(path(dirs$output_dir, 'temp'))

message("Sucesso! O arquivo 'bacias_contribuicao_cumulativa.gpkg' foi gerado com bacias sobrepostas reais.")

# -----------------------------------------------------------------------------
# 6. CONSTRUÇÃO DO GRAFO HIDROLÓGICO (igraph)
# -----------------------------------------------------------------------------
message("\n=== Etapa 6: Construção do grafo hidrológico ===")

grafo <- construir_grafo_hidrologico(
  estacoes_sf          = estacoes_snap, 
  rede_drenagem_path   = PATHS_DEM$dem_streams_rast
)

# -----------------------------------------------------------------------------
# 7. EXTRAÇÃO DE ATRIBUTOS TOPOLÓGICOS
# -----------------------------------------------------------------------------
message("\n=== Etapa 7: Extração de atributos topológicos ===")

estacoes_enriquecidas <- extrair_atributos_topologicos(
  grafo       = grafo, 
  estacoes_sf = estacoes_snap
)

# -----------------------------------------------------------------------------
# 8. EXPORTAÇÃO DOS PRODUTOS FINAIS
# -----------------------------------------------------------------------------
message("\n=== Etapa 8: Exportação final de artefatos do relevo ===")

# Salva o Grafo Hidrológico puro em formato binário estável do R
saveRDS(grafo, file = PATHS_DEM$grafo_rds)

# Salva a camada final analítica enriquecida (v2) requisitada pelo seu layout
sf::st_write(estacoes_enriquecidas, PATHS_DEM$analise_v2, delete_dsn = TRUE, quiet = TRUE)

message(">>> Pipeline DEM concluído com sucesso para o cenário: ", RUN_NAME)