# =============================================================================
# CONFIGURAÇÕES DE VARIÁVEIS HIDROLÓGICAS
# =============================================================================
#
# Autor:   Renan Cassimiro Brito
# Data:    2026-06-01
#
# Descrição:
#   Define os objetos de configuração (strategy objects) para cada tipo de
#   variável hidrológica suportada. Cada objeto descreve completamente como
#   aquela variável deve ser baixada, organizada e analisada.
#
#   O pipeline genérico em main.R consome esses objetos via lapply/map,
#   tratando todas as variáveis da mesma forma.
#
# Uso:
#   source(here("config/variable_configs.R"))
#   cfg <- VARIABLE_CONFIGS[["discharge"]]   # acessa uma configuração
#   cfg <- VARIABLE_CONFIGS                  # ou passa a lista inteira
#
# Adicionando uma nova variável:
#   1. Crie uma nova entrada na lista VARIABLE_CONFIGS abaixo
#   2. Não é necessário modificar nenhum outro arquivo
# =============================================================================

VARIABLE_CONFIGS <- list(
  
  # ---------------------------------------------------------------------------
  # Vazão (m³/s) — estações fluviométricas
  # ---------------------------------------------------------------------------
  discharge = list(
    id           = "discharge",
    label        = "Vazão",                   # Usado em títulos e logs
    station_type = "flu",                     # Parâmetro para hydrobr::inventory()
    water_level  = FALSE,                     # Parâmetro para hydrobr::stationsData()
    value_col    = "stream_flow_m3_s",        # Nome da coluna de valor nos dados brutos
    unit_label   = "m³/s",                    # Usado em eixos de gráficos
    select_stations_params = list(
      mode          = "yearly",
      maxMissing    = 100,
      minYears      = 2,
      month         = 1,
      iniYear       = 2010,
      finYear       = 2026,
      consistedOnly = FALSE
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Nível d'água / cota (cm) — estações fluviométricas
  # ---------------------------------------------------------------------------
  water_level = list(
    id           = "water_level",
    label        = "Cota",
    station_type = "flu",                     # Mesma rede flu, só muda waterLevel
    water_level  = TRUE,
    value_col    = "level_cm",
    unit_label   = "cm",
    select_stations_params = list(
      mode          = "yearly",
      maxMissing    = 100,
      minYears      = 2,
      month         = 1,
      iniYear       = 2010,
      finYear       = 2026,
      consistedOnly = FALSE
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Precipitação (mm) — estações pluviométricas
  # ---------------------------------------------------------------------------
  precipitation = list(
    id           = "precipitation",
    label        = "Precipitação",
    station_type = "plu",                     # Rede pluviométrica
    water_level  = FALSE,
    value_col    = "rainfall_mm",
    unit_label   = "mm",
    select_stations_params = list(
      mode          = "yearly",
      maxMissing    = 100,
      minYears      = 2,
      month         = 1,
      iniYear       = 2010,
      finYear       = 2026,
      consistedOnly = FALSE
    )
  )
  
)