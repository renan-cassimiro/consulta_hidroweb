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
library(dplyr)
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

RUN_NAME <- "xingu_river"

INPUT_DIR <- here("input", RUN_NAME)
DEM_PATH <- path(INPUT_DIR, "bacias_amacro_fathomdem_low_res.tif")
STUDAY_AREA_PATH <- path(INPUT_DIR, "bacias_amacro_hybas_lake_sa_lev03_v1c_dissolvido.gpkg")

OUTPUT_DIR <- here("output", RUN_NAME)
DATA_DIR <- path(OUTPUT_DIR, "data")
DEM_DIR <- path(DATA_DIR, "dem")

dir.create(RUN_NAME)
dir.create(DATA_DIR)
dir.create(DEM_DIR)

# Caminhos dos produtos intermediários
DEM_PATH_PROJ <- path(DEM_DIR, paste0(RUN_NAME, "_dem_projected.tif"))
DEM_CLIP      <- path(DEM_DIR, paste0(RUN_NAME, "_dem_clip.tif"))
DEM_BREACH    <- path(DEM_DIR, paste0(RUN_NAME, "_dem_breach.tif"))
DEM_FILL      <- path(DEM_DIR, paste0(RUN_NAME, "_dem_fill.tif"))
D8_POINTER    <- path(DEM_DIR, paste0(RUN_NAME, "_flow_direction.tif"))
D8_ACCUM      <- path(DEM_DIR, paste0(RUN_NAME, "_flow_accumulation.tif"))
STREAMS_RAST  <- path(DEM_DIR, paste0(RUN_NAME, "_drainge_network.tif"))
STREAMS_VECT  <- path(DEM_DIR, paste0(RUN_NAME, "_drainge_network.gpkg"))
BACIAS_PATH   <- path(DATA_DIR, paste0(RUN_NAME, "_station_watersheds.gpkg"))
SNAP_PATH     <- path(DATA_DIR, paste0(RUN_NAME, "_snapped_stations.gpkg"))

BIG_STREAMS_RAST <- path(DEM_DIR, paste0(RUN_NAME, "_higher_order_drainge_network.tif"))
STREAM_LINKS <- path(DEM_DIR, paste0(RUN_NAME, "_higher_order_stream_links.tif"))
SUBBASINS <- path(DEM_DIR, paste0(RUN_NAME, "_higher_order_subbasins.tif"))
SUBBASINS_VEC <- path(DEM_DIR, paste0(RUN_NAME, "_higher_order_subbasins.gpkg"))

# CRS de destino — South America Albers Equal Area
# Preserva área, adequado para toda a extensão amazônica
CRS_PROJ <- "ESRI:102033"

# -----------------------------------------------------------------------------
# 1. PRÉ-PROCESSAMENTO DO DEM
# -----------------------------------------------------------------------------
message("=== 1. Pré-processamento do DEM ===")

# Carrega área de estudo e reprojeta para CRS do DEM
area_estudo <- st_read(STUDAY_AREA_PATH)

dem_raw <- rast(DEM_PATH)
message("  DEM original: ", nrow(dem_raw), " x ", ncol(dem_raw), " | CRS: ", crs(dem_raw, describe = TRUE)$code)

# Reprojeta o DEM (operação pesada — salva em disco)
message("Reprojetando DEM para Albers Equal Area...")
dem_proj <- project(dem_raw, CRS_PROJ, method = "bilinear")
writeRaster(dem_proj, DEM_PATH_PROJ, overwrite = TRUE)

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
wbt_d8_pointer(dem    = DEM_FILL, output = D8_POINTER)

# Acumulação de fluxo (número de células a montante)
message("  Calculando acumulação de fluxo...")
wbt_d8_flow_accumulation(input  = DEM_FILL, output = D8_ACCUM, out_type = "cells")

# -----------------------------------------------------------------------------
# 3. EXTRAÇÃO DA REDE DE DRENAGEM
# -----------------------------------------------------------------------------
message("\n=== 3. Extração da rede de drenagem ===")

# Limiar de acumulação para definir início de canal
# 30m: ~1000 células ≈ 0.9 km² de área contribuinte mínima
# Ajuste conforme necessário para a densidade de drenagem da região
LIMIAR_ACUMULACAO <- 50

message("  Limiar de acumulação: ", LIMIAR_ACUMULACAO, " células (~", round(LIMIAR_ACUMULACAO * 30^2 / 1e6, 2), " km²)")
wbt_extract_streams(flow_accum = D8_ACCUM, output = STREAMS_RAST, threshold = LIMIAR_ACUMULACAO)

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

###Filtrar por áreas de contribuição
analysed_stations <- filter(analysed_stations, area_km2>10000)

# Reprojeta para CRS do DEM
estacoes_dem <- st_transform(analysed_stations, crs(rast(D8_ACCUM)))

# Raster de acumulação
streams_rast <- rast(STREAMS_RAST)
accum_rast <- rast(D8_ACCUM)

# Distância máxima de busca (m)
SNAP_DIST <- 15000

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

wbt_extract_streams(flow_accum = D8_ACCUM, output = BIG_STREAMS_RAST, threshold = 5000)
wbt_stream_link_identifier(streams = BIG_STREAMS_RAST, d8_pntr = D8_POINTER, output = STREAM_LINKS)
wbt_subbasins(d8_pntr = D8_POINTER, streams = STREAM_LINKS, output = SUBBASINS)

bacias_estacoes <- as.polygons(rast(SUBBASINS),  dissolve = TRUE) |>
  st_as_sf() |>  st_set_crs(crs(rast(SUBBASINS)))

bacias_estacoes <- bacias_estacoes |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)

# Spatial join — cada estação herda a bacia onde cai
estacoes_com_bacia <- estacoes_snap |> st_join(bacias_estacoes, join = st_within) |>
  left_join(bacias_estacoes |> st_drop_geometry() |> select(amacro_higher_order_subbasins, area_km2), by = "amacro_higher_order_subbasins")

# Verifica distribuição
message("Estações por bacia:")
print(table(estacoes_com_bacia$amacro_higher_order_subbasins))

# 5.10 QA simples - estação precisa cair dentro da própria bacia
validacao <- st_intersects(estacoes_snap, bacias_estacoes)

n_fora <- sum(lengths(validacao) == 0)

if (n_fora > 0) {
  warning(n_fora, " estação(ões) fora da própria bacia.")
}

message("Bacias geradas: ", nrow(bacias_estacoes))
message("Área mín: ", round(min(bacias_estacoes$area_km2), 1), " km²")
message("Área máx: ", round(max(bacias_estacoes$area_km2), 1), " km²")

# 5.11 Salva
st_write(bacias_estacoes, path(SUBBASINS_VEC), delete_dsn = TRUE, quiet = TRUE)
message("  Bacias exportadas em: ", BACIAS_PATH)
table(is.na(bacias_estacoes$xingu_river_higher_order_subbasins))

# Mapa de conferência
ggplot() +
  geom_sf(data = bacias_estacoes, aes(fill = area_km2), color = "gray40", linewidth = 0.3) +
  geom_sf(data = estacoes_com_bacia, color = "blue", size = 2) +
  scale_fill_viridis_c(name = "Área (km²)", direction = -1) +
  labs(
    title    = "Subdivisão de bacias — wbt_basins",
    subtitle = paste0("N = ", nrow(bacias_estacoes), " bacias")
  ) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

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
# 7. CONSTRUÇÃO DO GRAFO HIDROLÓGICO (topologia D8 real - Refatorado e Vetorizado)
# -----------------------------------------------------------------------------
message("\n=== 7. Construção do grafo hidrológico ===")

# -------------------------------------------------------------------------
# Inputs e Valores
# -------------------------------------------------------------------------

d8 <- rast(D8_POINTER)
streams <- rast(STREAMS_RAST)

d8_vals     <- values(d8, mat = FALSE)
stream_vals <- values(streams, mat = FALSE)

# Filtramos os índices lineares (cell_id) que pertencem à rede hidrográfica
idx_rede <- which(stream_vals == 1)
message("  Pixels da rede: ", length(idx_rede))

# -------------------------------------------------------------------------
# Mapeamento de Direções (Convenção Default WhiteboxTools)
# -------------------------------------------------------------------------
# 1=NE, 2=E, 4=SE, 8=S, 16=SW, 32=W, 64=NW, 128=N
# dr = delta row (variação na linha) | dc = delta col (variação na coluna)
dir_map <- data.frame(
  dir = c(1, 2, 4, 8, 16, 32, 64, 128),
  dr  = c(-1, 0, 1, 1, 1, 0, -1, -1),
  dc  = c( 1, 1, 1, 0,-1,-1, -1,  0)
)

# -------------------------------------------------------------------------
# Construção Vetorizada das Arestas (Alta Performance)
# -------------------------------------------------------------------------

# 1. Puxamos a matriz exata de (linha, coluna) nativa do pacote terra
rc_origem <- terra::rowColFromCell(d8, idx_rede)

# 2. Montamos o dataframe com as informações da origem
df_edges <- data.frame(
  from_cell = idx_rede,
  dir       = d8_vals[idx_rede],
  row_orig  = rc_origem[, 1],
  col_orig  = rc_origem[, 2]
)

# 3. Aplicamos os deltas matemáticos baseados no D8 e calculamos o destino
df_edges <- df_edges %>%
  filter(!is.na(dir) & dir %in% dir_map$dir) %>%
  left_join(dir_map, by = "dir") %>%
  mutate(
    row_dest = row_orig + dr,
    col_dest = col_orig + dc
  )

# 4. Removemos destinos que caem fora da matriz do raster (bordas de mapa)
df_edges <- df_edges %>%
  filter(
    row_dest >= 1, row_dest <= nrow(d8),
    col_dest >= 1, col_dest <= ncol(d8)
  )

# 5. Convertemos as coordenadas de destino de volta para o índice linear (cell_id)
df_edges$to_cell <- terra::cellFromRowCol(d8, df_edges$row_dest, df_edges$col_dest)

# 6. Filtros de coerência topológica
df_edges <- df_edges %>%
  filter(from_cell != to_cell) %>% # Evita self-loops em caso de erro no raster
  filter(!is.na(stream_vals[to_cell]) & stream_vals[to_cell] == 1) # Mantém apenas se o destino também for rio

# 7. Preparamos as arestas em character para o igraph não confundir com índice numérico
edges_pix <- tibble(
  from = as.character(df_edges$from_cell),
  to   = as.character(df_edges$to_cell)
)

message("  Arestas geradas com sucesso: ", nrow(edges_pix))

# -------------------------------------------------------------------------
# Grafo completo da rede e Exportação
# -------------------------------------------------------------------------

grafo_rede <- graph_from_data_frame(edges_pix, directed = TRUE)
message("  Grafo estruturado: ", vcount(grafo_rede), " nós | ", ecount(grafo_rede), " arestas")

# 1. Extrai IDs (nomes) dos vértices 
node_ids <- as.numeric(V(grafo_rede)$name)

# 2. Resgata as coordenadas espaciais
coords <- terra::xyFromCell(d8, node_ids)

# 3. Formata o Dataframe
nodes_df <- data.frame(
  node_id = node_ids,
  x = coords[, "x"],
  y = coords[, "y"]
)

# 4. Converte para objeto espacial (sf)
nodes_sf <- st_as_sf(
  nodes_df, 
  coords = c("x", "y"), 
  crs = st_crs(d8)
)

# 5. Exporta para Shapefile para checagem visual no QGIS
st_write(
  nodes_sf, 
  "nos_do_grafo_rede.shp", 
  delete_layer = TRUE,
  quiet = TRUE
)

message("\n=== Shapefile 'nos_do_grafo_rede.shp' exportado ===")
# -------------------------------------------------------------------------
# Associar estação ao pixel da rede e construir o grafo entre estações
# -------------------------------------------------------------------------
message("\n=== Construindo o grafo das estações ===")

# 1. Tabela auxiliar de nós
station_nodes <- estacoes_snap |>
  st_drop_geometry() |>
  select(
    station_code,
    snap_cell,
    posicao_rede,
    ordem_strahler,
    area_contrib_km2,
    acumulacao_celulas
  )

# 2. Garante que os nós das estações existem no grafo_rede
nodes_est_char <- as.character(station_nodes$snap_cell)
valid_nodes <- nodes_est_char[nodes_est_char %in% V(grafo_rede)$name]

# --- A CORREÇÃO AQUI ---
# Remove duplicidades geradas por estações no mesmo pixel
unique_valid_nodes <- unique(valid_nodes)

if (length(valid_nodes) < length(nodes_est_char)) {
  warning("Atenção: Algumas estações não foram mapeadas na rede.")
}

# -------------------------------------------------------------------------
# Matriz de Distâncias Topológicas (Cálculo Otimizado igraph)
# -------------------------------------------------------------------------
message("  Calculando matriz de roteamento hidrológico...")
dist_mat <- distances(
  grafo_rede,
  v  = unique_valid_nodes,
  to = unique_valid_nodes, # Exige valores únicos
  mode = "out"
)

# -------------------------------------------------------------------------
# Identificar o vizinho imediato downstream (Versão Blindada Hidrologicamente)
# -------------------------------------------------------------------------
message("  Mapeando conexões estritas de jusante...")

# Cria um vetor nomeado para busca rápida de acumulação por célula
celula_para_accum <- setNames(
  station_nodes$acumulacao_celulas, 
  as.character(station_nodes$snap_cell)
)

edges_est_list <- lapply(seq_along(station_nodes$station_code), function(i) {
  from_code  <- station_nodes$station_code[i]
  from_cell  <- as.character(station_nodes$snap_cell[i])
  from_accum <- station_nodes$acumulacao_celulas[i]
  
  if (!(from_cell %in% unique_valid_nodes)) return(NULL)
  
  # Pega as distâncias a partir desta célula
  dists <- dist_mat[from_cell, ]
  alcançaveis <- dists[dists > 0 & is.finite(dists)]
  
  if (length(alcançaveis) > 0) {
    
    # --- A CORREÇÃO HIDROLÓGICA CRUCIAL ---
    # Só aceita células de destino cuja acumulação seja MAIOR que a da nossa origem
    acumulacoes_destino <- celula_para_accum[names(alcançaveis)]
    alcançaveis_jusante <- alcançaveis[acumulacoes_destino > from_accum]
    
    # Se sobrou alguma estação legítima rio abaixo...
    if (length(alcançaveis_jusante) > 0) {
      # Seleciona a mais próxima dentre as que estão estritamente à JUSANTE
      to_cell <- names(which.min(alcançaveis_jusante))
      code_to <- station_nodes$station_code[as.character(station_nodes$snap_cell) == to_cell]
      
      return(tibble(from = from_code, to = code_to))
    }
  }
  return(NULL)
})

edges_est <- bind_rows(edges_est_list)

# -------------------------------------------------------------------------
# Distância Euclidiana Vetorizada (Em Km)
# -------------------------------------------------------------------------
geom_from <- estacoes_snap$geometry[match(edges_est$from, estacoes_snap$station_code)]
geom_to   <- estacoes_snap$geometry[match(edges_est$to, estacoes_snap$station_code)]

edges_est$dist_km <- round(
  as.numeric(st_distance(geom_from, geom_to, by_element = TRUE)) / 1000, 
  2
)

message("  Arestas entre estações mapeadas: ", nrow(edges_est))

# -------------------------------------------------------------------------
# Grafo final entre estações
# -------------------------------------------------------------------------

vertices_est <- station_nodes |>
  rename(name = station_code)

grafo <- graph_from_data_frame(
  d = edges_est,
  directed = TRUE,
  vertices = vertices_est
)

# Métricas de Topologia (Centralidade)
V(grafo)$grau_entrada <- degree(grafo, mode = "in")
V(grafo)$grau_saida   <- degree(grafo, mode = "out")
V(grafo)$betweenness  <- betweenness(grafo, directed = TRUE, normalized = TRUE)

message(
  "  Grafo final estruturado: ",
  vcount(grafo), " nós | ",
  ecount(grafo), " arestas"
)

message("  Estações sem conexão jusante (exutórios ou isoladas): ", sum(degree(grafo, mode="out") == 0))

# -------------------------------------------------------------------------
# Salvar e Visualizar
# -------------------------------------------------------------------------

saveRDS(grafo, path(DATA_DIR, "grafo_hidrologico.rds"))

# Visual rápido
V(grafo)$size  <- 6
V(grafo)$color <- "lightblue"

plot(grafo, layout = layout_with_kk(grafo), vertex.label = NA,  
     edge.arrow.size = 0.3, main = "Rede de Conectividade das Estações"
)

# -----------------------------------------------------------------------------
# 8. ENRIQUECIMENTO DA CAMADA ANALÍTICA
# -----------------------------------------------------------------------------
message("\n=== 8. Enriquecimento da camada analítica ===")

# 1. Extrai as arestas do grafo (quem conecta com quem)
# Pegamos o 'from' (estação atual) e o 'to' (estação jusante)
edges_df <- igraph::as_data_frame(grafo, what = "edges") |>
  select(
    station_code = from, 
    downstream_station = to, 
    dist_downstream_km = dist_km
  )

# 2. Extrai os atributos topológicos dos nós e anexa os destinos
atributos_grafo <- data.frame(
  station_code = V(grafo)$name,
  grau_entrada = V(grafo)$grau_entrada,
  grau_saida   = V(grafo)$grau_saida,
  betweenness  = round(V(grafo)$betweenness, 4)
) |>
  left_join(edges_df, by = "station_code")

# 3. Consolida todas as informações na camada espacial analítica
estacoes_analise_v2 <- analysed_stations |>
  # Traz TODOS os campos do estacoes_snap (sem select restritivo)
  left_join(
    estacoes_snap |> st_drop_geometry(), 
    by = "station_code"
  ) |>
  # Mantive comentado conforme o seu código, caso o erro de nome da coluna persista
  # left_join(
  #   bacias_estacoes |> st_drop_geometry() |> select(station_code, area_bacia_km2), 
  #   by = "station_code"
  # ) |>
  left_join(atributos_grafo, by = "station_code")

# 4. Exportações das Tabelas
# GeoPackage para uso no QGIS/ArcGIS
st_write(
  estacoes_analise_v2, 
  path(DATA_DIR, "estacoes_analise_v2.gpkg"),
  delete_dsn = TRUE, 
  quiet = TRUE
)

# Parquet para alta performance em futuras análises (R/Python)
estacoes_analise_v2 |>
  st_drop_geometry() |>
  arrow::write_parquet(
    path(DATA_DIR, "estacoes_analise_v2.parquet"),
    compression = "zstd"
  )

message("  Camada analítica v2 salva com sucesso (GPKG e Parquet).")

# -----------------------------------------------------------------------------
# EXPORTAÇÃO DO GRAFO (Formatos de Rede)
# -----------------------------------------------------------------------------

# 1. Salva em RDS nativo do R (Este não tem frescura, salva o objeto perfeito)
saveRDS(grafo, path(DATA_DIR, "grafo_estacoes.rds"))

# 2. Prepara uma cópia limpa do grafo para o GraphML
grafo_graphml <- grafo

# Converte qualquer atributo problemático dos nós (vértices) para texto (character)
for (attr in vertex_attr_names(grafo_graphml)) {
  valores <- vertex_attr(grafo_graphml, attr)
  if (is.factor(valores) || is.logical(valores)) {
    grafo_graphml <- set_vertex_attr(grafo_graphml, attr, value = as.character(valores))
  }
}

# Faz o mesmo para as arestas (edges)
for (attr in edge_attr_names(grafo_graphml)) {
  valores <- edge_attr(grafo_graphml, attr)
  if (is.factor(valores) || is.logical(valores)) {
    grafo_graphml <- set_edge_attr(grafo_graphml, attr, value = as.character(valores))
  }
}

# 3. Salva em GraphML (Formato universal para abrir no Gephi, Python/NetworkX, etc)
write_graph(
  grafo_graphml, 
  path(DATA_DIR, "grafo_estacoes.graphml"), 
  format = "graphml"
)

message("  Topologia exportada com sucesso (RDS e GraphML).")

# -----------------------------------------------------------------------------
# 9. MAPA DE CONFERÊNCIA
# -----------------------------------------------------------------------------
message("\n=== 9. Gerando mapa de conferência ===")

library(ggplot2)
library(scales)

mapa_final <- ggplot() +
  # Área de estudo como fundo
  geom_sf(data = area_estudo, fill = "#f8f9fa", color = "#adb5bd", linewidth = 0.5) +
  
  # Bacias de contribuição (Tracejado)
  geom_sf(data = bacias_estacoes, fill = NA, color = "#6c757d", linewidth = 0.4, linetype = "dashed") +
  
  # Estações (Tamanho por área e Cor por posição)
  geom_sf(
    data = estacoes_analise_v2,
    aes(color = posicao_rede, size = area_contrib_km2),
    alpha = 0.9
  ) +
  
  # Escala de cores (Paleta robusta para daltônicos/impressão)
  scale_color_manual(
    values = c(
      "Rio principal"         = "#264653",
      "Tributário principal"  = "#e76f51",
      "Tributário secundário" = "#e9c46a"
    ),
    name = "Posição na rede"
  ) +
  
  # Escala de tamanho visualmente balanceada
  scale_size_continuous(
    name   = "Área contribuinte (km²)",
    range  = c(2, 7),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  
  # Organização da legenda
  guides(
    color = guide_legend(override.aes = list(size = 5), order = 1),
    size  = guide_legend(order = 2)
  ) +
  
  # Textos e Tema
  labs(
    title    = "Rede de Drenagem e Bacias de Contribuição",
    subtitle = "Cor: Posição na rede (Strahler) | Tracejado: Bacia por estação"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle   = element_text(color = "#495057", size = 11, margin = margin(b = 15)),
    legend.position = "right",
    legend.box      = "vertical",
    panel.grid      = element_blank(), # Remove as grades do fundo
    axis.text       = element_blank()  # Remove eixos lat/long para visual mais limpo
  )

# Salvamento do mapa garantindo fundo branco
ggsave(
  filename = path(IMAGE_DIR, "etapa1_rede_drenagem.png"),
  plot     = mapa_final,
  width    = 12, 
  height   = 10, 
  dpi      = 300, 
  bg       = "white" 
)

message("  Mapa salvo em: ", path(IMAGE_DIR, "etapa1_rede_drenagem.png"))
message("\n=== Processamento concluído com sucesso! ===")
message("  Produtos finais disponíveis em: ", DATA_DIR)
