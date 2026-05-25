# =============================================================================
# REDE DE DRENAGEM E GRAFO HIDROLÓGICO — DEM 30m
# =============================================================================
#
# Autor:   Renan Cassimiro Brito
# Data:    2026-05-21
#
# Descrição:
#   Deriva a rede de drenagem e o grafo hidrológico a partir do FathomDEM 30m.
#   Para cada estação fluviométrica, delimita a bacia de contribuição e extrai
#   atributos topológicos da rede. O produto final é a camada `estacoes_analise`
#   enriquecida com atributos de bacia e posição na rede, pronta para as
#   etapas de Moran e análise climática/territorial.
#
# Fluxo:
#   1. Pré-processamento do DEM (clip, breach, fill)
#   2. Direção e acumulação de fluxo (D8)
#   3. Extração da rede de drenagem
#   4. Snap das estações à rede
#   5. Delimitação das bacias de contribuição
#   6. Construção do grafo hidrológico (igraph)
#   7. Extração de atributos topológicos
#   8. Exportação
#
# Inputs:
#   DEM_PATH                        — FathomDEM 30m (definir abaixo)
#   resources/                      — área de estudo e estações
#   data/estacoes_analise.gpkg      — camada analítica (Etapa 1)
#
# Outputs:
#   data/dem/dem_processado.tif     — DEM condicionado
#   data/dem/direcao_fluxo.tif      — D8 pointer
#   data/dem/acumulacao_fluxo.tif   — acumulação de fluxo
#   data/dem/rede_drenagem.tif      — rede extraída (raster)
#   data/dem/rede_drenagem.gpkg     — rede vetorizada
#   data/dem/bacias_estacoes.gpkg   — bacias de contribuição por estação
#   data/dem/estacoes_snap.gpkg     — estações ajustadas à rede
#   data/grafo_hidrologico.rds      — grafo igraph com atributos completos
#   data/estacoes_analise_v2.gpkg   — camada analítica enriquecida
#
# Dependências:
#   CRAN: terra, sf, whitebox, igraph, tidyverse, fs, here, scales
#   whitebox requer instalação do executável:
#     whitebox::install_whitebox()
# =============================================================================
library(arrow)
library(here)
library(igraph)
library(scales)
library(sf)
library(sfarrow)
library(terra)
library(tidyverse)
library(whitebox)

# Garante que path() resolve para fs, não igraph
path <- fs::path

# Instala o executável do WhiteboxTools se necessário (rodar uma vez)
# whitebox::install_whitebox()

# -----------------------------------------------------------------------------
# Caminhos — ajuste DEM_PATH para o arquivo do FathomDEM
# -----------------------------------------------------------------------------
source(here("functions/functions.R"))

RESOURCES_DIR <- here("resources")
OUTPUT_DIR <- here("output", "iriri_river")
DATA_DIR <- path(OUTPUT_DIR, "data")
DEM_PATH <- path(RESOURCES_DIR, "fathomdem_bacia_rio_iriri_recortado_metros.tif")   # ex: "resources/dem/fathomdem_30m.tif"
DEM_DIR <- path(DATA_DIR, "dem")

dir.create(DATA_DIR)
dir.create(DEM_DIR)

# Caminhos dos produtos intermediários
DEM_PATH_PROJ <- path(DEM_DIR, "dem_projetado.tif")
DEM_CLIP      <- path(DEM_DIR, "dem_clip.tif")
DEM_BREACH    <- path(DEM_DIR, "dem_breach.tif")
DEM_FILL      <- path(DEM_DIR, "dem_fill.tif")
D8_POINTER    <- path(DEM_DIR, "direcao_fluxo.tif")
D8_ACCUM      <- path(DEM_DIR, "acumulacao_fluxo.tif")
STREAMS_RAST  <- path(DEM_DIR, "rede_drenagem.tif")
STREAMS_VECT  <- path(DEM_DIR, "rede_drenagem.gpkg")
BACIAS_PATH   <- path(DEM_DIR, "bacias_estacoes.gpkg")
SNAP_PATH     <- path(DEM_DIR, "estacoes_snap.gpkg")

# CRS de destino — South America Albers Equal Area
# Preserva área, adequado para toda a extensão amazônica
CRS_PROJ <- "ESRI:102033"

# -----------------------------------------------------------------------------
# 1. PRÉ-PROCESSAMENTO DO DEM
# -----------------------------------------------------------------------------
message("=== 1. Pré-processamento do DEM ===")

# Carrega área de estudo e reprojeta para CRS do DEM
area_estudo <- st_read(path(RESOURCES_DIR, "hybas_lake_sa_lev05_v1c_rio_iriri.gpkg"))

dem_raw <- rast(DEM_PATH)
message("  DEM original: ", nrow(dem_raw), " x ", ncol(dem_raw), " | CRS: ", crs(dem_raw, describe = TRUE)$code)

# Reprojeta o DEM (operação pesada — salva em disco)
message("Reprojetando DEM para Albers Equal Area...")
dem_proj <- project(dem_raw, CRS_PROJ, method = "bilinear")
writeRaster(dem_proj, path(DEM_DIR, "dem_projetado.tif"), overwrite = TRUE)

# TODO: Clip se a área de estudo for menor que o DEM
ext_estudo <- area_estudo 

# Breach de depressões (remove barreiras artificiais preservando a topografia)
# Preferível ao fill puro para DEMs em áreas com planícies e várzeas
message("  Breach de depressões...")
wbt_breach_depressions_least_cost(
  dem = DEM_PATH_PROJ,
  output = DEM_BREACH,
  dist = 10, # distância máxima de breach (células)
  fill = TRUE  # preenche o que não foi breachado
)

# Fill residual para garantir que não restam depressões
message("  Fill residual...")
wbt_fill_depressions_wang_and_liu(
  dem    = DEM_BREACH,
  output = DEM_FILL
)

# -----------------------------------------------------------------------------
# 2. DIREÇÃO E ACUMULAÇÃO DE FLUXO (D8)
# -----------------------------------------------------------------------------
message("\n=== 2. Direção e acumulação de fluxo ===")

# D8 pointer (direção de fluxo)
message("  Calculando direção de fluxo D8...")
wbt_d8_pointer(
  dem    = DEM_FILL,
  output = D8_POINTER
)

# Acumulação de fluxo (número de células a montante)
message("  Calculando acumulação de fluxo...")
wbt_d8_flow_accumulation(
  input  = DEM_FILL,
  output = D8_ACCUM,
  out_type = "cells"
)

# -----------------------------------------------------------------------------
# 3. EXTRAÇÃO DA REDE DE DRENAGEM
# -----------------------------------------------------------------------------
message("\n=== 3. Extração da rede de drenagem ===")

# Limiar de acumulação para definir início de canal
# 30m: ~1000 células ≈ 0.9 km² de área contribuinte mínima
# Ajuste conforme necessário para a densidade de drenagem da região
LIMIAR_ACUMULACAO <- 5000

message("  Limiar de acumulação: ", LIMIAR_ACUMULACAO, " células (~",
        round(LIMIAR_ACUMULACAO * 30^2 / 1e6, 2), " km²)")
wbt_extract_streams(
  flow_accum = D8_ACCUM,
  output     = STREAMS_RAST,
  threshold  = LIMIAR_ACUMULACAO
)

#Deixar a vetorização para depois
# Vetorizar a rede de drenagem
# message("  Vetorizando rede de drenagem...")
# 
# streams_rast <- rast(STREAMS_RAST)
# 
# # Converte pixels de rede (valor = 1) para linhas via terra
# rede_drenagem <- as.lines(streams_rast) |>
#   st_as_sf() |>
#   st_set_crs(crs(streams_rast))
# 
# st_write(rede_drenagem, STREAMS_VECT, delete_dsn = TRUE)
# st_write_parquet(rede_drenagem, STREAMS_VECT)
# 
# rede_drenagem <- st_read(STREAMS_VECT, quiet = TRUE) |>
#   st_set_crs(crs(rast(DEM_FILL)))   # whitebox não propaga CRS no vetor
# 
# message("  Segmentos extraídos: ", nrow(rede_drenagem))


# -----------------------------------------------------------------------------
# 4. SNAP DAS ESTAÇÕES À MAIOR ACUMULAÇÃO LOCAL
# -----------------------------------------------------------------------------

message("\n=== 4. Snap hidrológico das estações ===")

# Carrega estações
analysed_stations <- st_read_parquet(path(DATA_DIR, "analysed_stations.parquet"))

# Reprojeta para CRS do DEM
estacoes_dem <- st_transform(analysed_stations, crs(rast(D8_ACCUM)))

# Raster de acumulação
streams_rast <- rast(STREAMS_RAST)
accum_rast <- rast(D8_ACCUM)


# Distância máxima de busca (m)
SNAP_DIST <- 3000

# -----------------------------------------------------------------------------
# Aplicar snap
# -----------------------------------------------------------------------------

snapped_list <- vector("list", nrow(estacoes_dem))

for (i in seq_len(nrow(estacoes_dem))) {
  message("[", i, "/", nrow(estacoes_dem), "] ", estacoes_dem$station_code[i])
  
  ponto <- estacoes_dem[i, ]
  
  snap <- snap_para_rede(
    ponto_sf   = ponto,
    streams_rast = streams_rast,
    accum_rast = accum_rast,
    snap_dist  = SNAP_DIST
  )
  
  # Caso falhe
  if (is.null(snap)) {
    next
  }
  
  # Mantém atributos
  snap$station_code <- ponto$station_code
  
  snapped_list[[i]] <- snap
}

# Junta resultados
estacoes_snap <- bind_rows(snapped_list)

# -----------------------------------------------------------------------------
# Controle de qualidade
# -----------------------------------------------------------------------------

message("Snap concluído.")

message("Distância média: ", round(mean(estacoes_snap$dist_snap_m, na.rm = TRUE), 1),  " m")
message("Distância máxima: ",  round(max(estacoes_snap$dist_snap_m, na.rm = TRUE), 1),  " m")

# Estações suspeitas
problematicas <- estacoes_snap |>  filter(dist_snap_m > SNAP_DIST * 0.8)

if (nrow(problematicas) > 0) {
  warning(nrow(problematicas), " estações com snap distante.")
  print(problematicas |> st_drop_geometry() |> select(station_code, dist_snap_m))
}

# -----------------------------------------------------------------------------
# Exportar
# -----------------------------------------------------------------------------
st_write(estacoes_snap, SNAP_PATH,  delete_dsn = TRUE, quiet = TRUE)

# -----------------------------------------------------------------------------
# 5. DELIMITAÇÃO DAS BACIAS DE CONTRIBUIÇÃO
# -----------------------------------------------------------------------------
message("\n=== 5. Delimitação das bacias de contribuição ===")
message("  Processando ", nrow(estacoes_snap), " estações — pode demorar alguns minutos...")

# Delimita watershed para cada estação individualmente
# TODO Melhorar processo, ele tá rodando a mesma coisa um onte de vez
# Também não parece estar trazendo a delimitação certa da bacia
bacias_lista <- vector("list", nrow(estacoes_snap))

for (i in seq_len(nrow(estacoes_snap))) {
  cod <- estacoes_snap$station_code[i]
  message("  [", i, "/", nrow(estacoes_snap), "] ", cod)
  
  ponto_tmp  <- path(DEM_DIR, paste0("pour_", cod, ".shp"))
  bacia_tmp  <- path(DEM_DIR, paste0("bacia_", cod, ".tif"))
  
  # Exporta ponto individual
  st_write(estacoes_snap[i, ], ponto_tmp, delete_dsn = TRUE, quiet = TRUE)
  
  # Delimita watershed
  wbt_watershed(
    d8_pntr   = D8_POINTER,
    pour_pts  = ponto_tmp,
    output    = bacia_tmp
  )
  
  # Vetoriza e calcula área
  bacia_rast <- rast(bacia_tmp)
  bacia_vect <- as.polygons(bacia_rast == 1) |>
    st_as_sf() |>
    st_set_crs(crs(bacia_rast)) |>
    # filter(bacia_rast == 1) |>   # mantém só a bacia (valor 1)
    summarise(geometry = st_union(geometry)) |>
    mutate(
      station_code  = cod,
      area_bacia_km2 = as.numeric(st_area(geometry)) / 1e6
    )
  
  bacias_lista[[i]] <- bacia_vect
  
  # Limpeza dos arquivos temporários individuais
  # file_delete(c(ponto_tmp,
  #               path(DEM_DIR, paste0("pour_", cod, ".dbf")),
  #               path(DEM_DIR, paste0("pour_", cod, ".prj")),
  #               path(DEM_DIR, paste0("pour_", cod, ".shx")),
  #               bacia_tmp))
}

bacias_estacoes <- bind_rows(bacias_lista)

message("\n  Área de bacia — min: ", round(min(bacias_estacoes$area_bacia_km2), 1),
        " km² | max: ", round(max(bacias_estacoes$area_bacia_km2), 1), " km²")

st_write(bacias_estacoes, BACIAS_PATH, delete_dsn = TRUE, quiet = TRUE)

# -----------------------------------------------------------------------------
# 6. ATRIBUTOS TOPOLÓGICOS DA REDE
# -----------------------------------------------------------------------------

message("\n=== 6. Atributos topológicos ===")

# Acumulação de fluxo no ponto de cada estação (proxy de área contribuinte)
accum_rast <- rast(D8_ACCUM)
accum_vals <- terra::extract(accum_rast, vect(estacoes_snap))

estacoes_snap <- estacoes_snap |>
  mutate(
    acumulacao_celulas = accum_vals[[2]],
    area_contrib_km2   = acumulacao_celulas * (30^2) / 1e6   # 30m resolução
  )

# Ordem de Strahler via whitebox
STRAHLER_PATH <- path(DEM_DIR, "strahler.tif")
wbt_strahler_stream_order(
  streams  = STREAMS_RAST,
  d8_pntr  = D8_POINTER,
  output   = STRAHLER_PATH
)

strahler_rast <- rast(STRAHLER_PATH)
strahler_vals <- terra::extract(strahler_rast, vect(estacoes_snap))

estacoes_snap <- estacoes_snap |>
  mutate(ordem_strahler = strahler_vals[[2]])

# Distância ao exutório (ponto de maior acumulação = saída da bacia)
# Identificado como o pixel de máxima acumulação dentro da área de estudo
accum_max_idx <- which.max(values(accum_rast))
exutorio_coords <- xyFromCell(accum_rast, accum_max_idx)
exutorio_sf <- st_point(exutorio_coords) |>
  st_sfc(crs = crs(accum_rast)) |>
  st_as_sf()

DIST_EXUT_PATH <- path(DEM_DIR, "dist_exutorio.tif")
wbt_downslope_distance_to_stream(
  dem     = DEM_FILL,
  streams = STREAMS_RAST,
  output  = DIST_EXUT_PATH
)

dist_vals <- terra::extract(rast(DIST_EXUT_PATH), vect(estacoes_snap))
estacoes_snap <- estacoes_snap |>
  mutate(dist_exutorio_km = dist_vals[[2]] / 1000)

# Classificação por ordem de Strahler
estacoes_snap <- estacoes_snap |>
  mutate(
    posicao_rede = case_when(
      ordem_strahler >= 6 ~ "Rio principal",
      ordem_strahler >= 4 ~ "Tributário principal",
      TRUE                ~ "Tributário secundário"
    ),
    posicao_rede = factor(
      posicao_rede,
      levels = c("Rio principal", "Tributário principal", "Tributário secundário")
    )
  )

message("  Distribuição por posição na rede:")
print(table(estacoes_snap$posicao_rede))

# -----------------------------------------------------------------------------
# 7. CONSTRUÇÃO DO GRAFO HIDROLÓGICO
# -----------------------------------------------------------------------------

message("\n=== 7. Construção do grafo hidrológico ===")

# Estratégia: duas estações são vizinhas na rede se uma está na bacia
# de contribuição da outra (relação montante-jusante)
n <- nrow(estacoes_snap)
arestas <- data.frame(from = character(), to = character(),
                      dist_km = numeric(), stringsAsFactors = FALSE)

for (i in seq_len(n)) {
  bacia_i <- bacias_estacoes |> filter(station_code == estacoes_snap$station_code[i])
  
  for (j in seq_len(n)) {
    if (i == j) next
    
    ponto_j <- estacoes_snap[j, ]
    
    # Estação j está dentro da bacia de i? → j é montante de i
    if (nrow(st_intersection(ponto_j, bacia_i)) > 0) {
      dist <- as.numeric(
        st_distance(estacoes_snap[i, ], estacoes_snap[j, ])
      ) / 1000
      
      arestas <- bind_rows(arestas, data.frame(
        from    = estacoes_snap$station_code[j],   # montante
        to      = estacoes_snap$station_code[i],   # jusante
        dist_km = round(dist, 2)
      ))
    }
  }
}

message("  Arestas encontradas: ", nrow(arestas))

# Cria grafo direcionado (montante → jusante)
nos <- estacoes_snap |>
  st_drop_geometry() |>
  select(station_code, posicao_rede, ordem_strahler,
         area_contrib_km2, acumulacao_celulas)

grafo <- graph_from_data_frame(
  d        = arestas,
  directed = TRUE,
  vertices = nos
)

# Atributos de centralidade na rede
V(grafo)$grau_entrada  <- degree(grafo, mode = "in")   # tributários que chegam
V(grafo)$grau_saida    <- degree(grafo, mode = "out")  # destinos a jusante
V(grafo)$betweenness   <- betweenness(grafo, directed = TRUE, normalized = TRUE)

message("  Grafo criado com ", vcount(grafo), " nós e ", ecount(grafo), " arestas")
message("  Estações sem conexão (isoladas): ", sum(degree(grafo) == 0))

saveRDS(grafo, path(DATA_DIR, "grafo_hidrologico.rds"))

# Personalizando cores e tamanho dos vértices
V(grafo)$color <- "lightblue"
V(grafo)$size <- 15

# Desenhando o gráfico
plot(grafo, layout = layout_with_kk(grafo), vertex.label = NA)


# -----------------------------------------------------------------------------
# 8. ENRIQUECIMENTO DA CAMADA ANALÍTICA
# -----------------------------------------------------------------------------

message("\n=== 8. Enriquecimento da camada analítica ===")

# Atributos topológicos extraídos do grafo
atributos_grafo <- data.frame(
  station_code   = V(grafo)$name,
  grau_entrada   = V(grafo)$grau_entrada,
  grau_saida     = V(grafo)$grau_saida,
  betweenness    = round(V(grafo)$betweenness, 4)
)

# Junta tudo na camada analítica
estacoes_analise_v2 <- analysed_stations |>
  left_join(
    estacoes_snap |>
      st_drop_geometry() |>
      select(station_code, ordem_strahler, posicao_rede,
             area_contrib_km2, dist_snap_m),
    by = "station_code"
  ) |>
  left_join(
    bacias_estacoes |>
      st_drop_geometry() |>
      select(station_code, area_bacia_km2),
    by = "station_code"
  ) |>
  left_join(atributos_grafo, by = "station_code")

# Exporta camada enriquecida
st_write(estacoes_analise_v2,
         path(DATA_DIR, "estacoes_analise_v2.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

estacoes_analise_v2 |>
  st_drop_geometry() |>
  arrow::write_parquet(path(DATA_DIR, "estacoes_analise_v2.parquet"),
                       compression = "zstd")

message("\nCamada analítica v2 salva.")

# -----------------------------------------------------------------------------
# 9. MAPA DE CONFERÊNCIA
# -----------------------------------------------------------------------------

ggplot() +
  geom_sf(data = area_estudo, fill = "gray96", color = "gray40", linewidth = 0.8) +
  # geom_sf(data = rede_drenagem, color = "steelblue", linewidth = 0.3, alpha = 0.6) +
  geom_sf(data = bacias_estacoes, fill = NA, color = "gray60",
          linewidth = 0.4, linetype = "dashed") +
  geom_sf(
    data = estacoes_analise_v2,
    aes(color = posicao_rede, size = area_contrib_km2)
  ) +
  scale_color_manual(
    values = c(
      "Rio principal"         = "#1a6348",
      "Tributário principal"  = "#f4a261",
      "Tributário secundário" = "#e76f51"
    ),
    name = "Posição na rede"
  ) +
  scale_size_continuous(
    name   = "Área contribuinte (km²)",
    range  = c(1.5, 6),
    labels = label_number(big.mark = ".")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    size  = guide_legend()
  ) +
  labs(
    title    = "Rede de drenagem e bacias de contribuição",
    subtitle = "Cor = posição na rede (Strahler) | Tracejado = bacia por estação"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "right"
  )

ggsave(
  path(IMAGE_DIR, "etapa1_rede_drenagem.png"),
  width = 12, height = 10, dpi = 200
)

message("\n=== Processamento concluído ===")
message("Produtos gerados em: ", DATA_DIR)
