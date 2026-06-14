# =============================================================================
# MÓDULO HIDROLÓGICO: PROCESSAMENTO DE RELEVO E GRAFOS
# =============================================================================
snap_para_rede <- function(ponto_sf, streams_rast, accum_rast, snap_dist = 15000) {
  
  # --------------------------------------------------
  # 1. Cria buffer de busca em metros
  # --------------------------------------------------
  buffer_busca <- ponto_sf |>
    st_transform(5880) |>
    st_buffer(snap_dist) |>
    st_transform(terra::crs(streams_rast))
  
  # --------------------------------------------------
  # 2. Recorta e mascara a drenagem
  # --------------------------------------------------
  # O parâmetro mask = TRUE já limpa o que está fora do buffer circular
  streams_crop <- terra::crop(streams_rast, vect(buffer_busca), mask = TRUE)
  
  # Se o crop retornar um raster vazio (sem canais no raio)
  if (all(is.na(minmax(streams_crop)))) {
    print(10)
    return(NULL)
  }
  
  # --------------------------------------------------
  # 3. Converte pixels do raster para pontos sf
  # --------------------------------------------------
  pts <- as.points(streams_crop, values = TRUE) |>
    st_as_sf()
  
  # Captura o nome dinâmico da coluna (evita erro caso o raster mude de nome)
  col_stream <- names(pts)[1]
  
  # Mantém apenas onde é canal (valor == 1)
  pts <- pts[pts[[col_stream]] == 1, ]
  
  if (nrow(pts) == 0) {
    return(NULL)
  }
  
  # --------------------------------------------------
  # 4. Calcula distância do ponto original até cada pixel
  # --------------------------------------------------
  d <- st_distance(
    st_transform(ponto_sf, 5880),
    st_transform(pts, 5880)
  )
  pts$dist_m <- as.numeric(d)
  
  # --------------------------------------------------
  # 5. Extrai acumulação
  # --------------------------------------------------
  acc <- terra::extract(accum_rast, vect(pts))
  pts$accum_snap <- acc[[2]] # A 2ª coluna tem os valores do raster
  
  # --------------------------------------------------
  # 6. Ordena e seleciona o melhor pixel
  #    (1º Menor distância, 2º Maior acumulação)
  # --------------------------------------------------
  pts <- pts |> arrange(dist_m, desc(accum_snap))
  melhor <- pts[1, ]
  melhor$dist_snap_m <- melhor$dist_m
  
  # --------------------------------------------------
  # 7. A MÁGICA: Captura o ID linear da célula global
  # --------------------------------------------------
  # Extrai as coordenadas XY do ponto vencedor
  xy_coords <- st_coordinates(melhor)
  
  # Descobre qual é o ID dessa célula no raster de acumulação original
  melhor$snap_cell <- terra::cellFromXY(streams_rast, xy_coords)
  # --------------------------------------------------
  # 8. Limpa colunas desnecessárias e retorna
  # --------------------------------------------------
  melhor <- melhor |> 
    select(dist_snap_m, accum_snap, snap_cell, geometry)
  
  return(melhor)
}

snap_estacoes_pipeline <- function(estacoes_sf, streams_rast, accum_rast, snap_dist) {
  # Cria uma lista para armazenar os pontos ajustados
  snapped_list <- vector("list", nrow(estacoes_sf))
  
  for (i in seq_len(nrow(estacoes_sf))) {
    message("[", i, "/", nrow(estacoes_sf), "] Efetuando snap da estação: ", estacoes_sf$station_code[i])
    
    ponto <- estacoes_sf[i, ]
    
    # Executa a sua função interna de snap (existente no seu functions.R)
    snap <- snap_para_rede(
      ponto_sf     = ponto,
      streams_rast = streams_rast,
      accum_rast   = accum_rast,
      snap_dist    = snap_dist
    )
    
    if (is.null(snap)) next
    
    snap$station_code <- ponto$station_code
    snapped_list[[i]] <- snap
  }
  
  return(dplyr::bind_rows(snapped_list))
}

delimitar_bacias_estacoes <- function(estacoes_snap_path, d8_pointer_path, output_path) {
  # Orquestra a ferramenta de bacias do WhiteboxTools para as estações corrigidas
  whitebox::wbt_watershed(
    d8_pntr = d8_pointer_path,
    pour_pts = estacoes_snap_path,
    output = output_path
  )
}

construir_grafo_hidrologico <- function(estacoes_sf, rede_drenagem_path) {
  # Lógica existente para ler a conectividade e transformar em igraph
  # [Sua implementação matemática do igraph entra aqui]
  #TODO
}

extrair_atributos_topologicos <- function(grafo, estacoes_sf) {
  # Lógica existente que calcula as métricas de rede (Strahler, distância à foz, etc.)
  # [Sua implementação existente entra aqui]
  #TODO
}