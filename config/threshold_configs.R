THRESHOLDS <- list(
  # ... (limiares da ANA que já estavam aqui) ...
  
  # Fase 4: Hidrologia Computacional e DEM
  dem_crs             = "ESRI:102033", # South America Albers Equal Area
  dem_limiar_accum    = 600000,        # Células para início de canal (~0.9 km²)
  dem_snap_dist_m     = 15000,         # Distância máxima de busca para o snap (15 km)
  dem_min_area_foco_km2 = 10000        # Filtro de corte para estações principais
)