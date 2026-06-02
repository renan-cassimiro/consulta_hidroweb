# =============================================================================
# SAZONALIDADE HIDROLÓGICA (Foco: Jornalismo de Dados)
# =============================================================================
# Adaptado para integrar ao ecossistema do pipeline do Rio Xingu/Iriri
# =============================================================================

#' Define o ano hidrológico de forma simples
definir_ano_hidrologico <- function(data, start_month = 1) {
  ano <- year(data)
  mes <- month(data)
  if_else(mes >= start_month, ano, ano - 1)
}

#' Calcula o dia juliano do Centro de Massa do volume anual
calcular_centro_massa <- function(datas, valores) {
  volume_total <- sum(valores, na.rm = TRUE)
  if (volume_total <= 0) return(NA_real_)
  
  acumulado <- cumsum(valores)
  idx <- which(acumulado >= volume_total / 2)[1]
  yday(datas[idx])
}

#' Calcula as métricas anuais para uma tabela diária
calcular_metricas_anuais <- function(df, value_col, start_month = 1, min_obs = 300) {
  valor_sym <- rlang::sym(value_col)
  
  df |>
    filter(!is.na(!!valor_sym)) |>
    mutate(hydro_year = definir_ano_hidrologico(date, start_month)) |>
    group_by(hydro_year) |>
    filter(n() >= min_obs) |>
    arrange(date, .by_group = TRUE) |>
    summarise(
      n_obs         = n(),
      valor_max     = max(!!valor_sym, na.rm = TRUE),
      valor_min     = min(!!valor_sym, na.rm = TRUE),
      valor_med     = mean(!!valor_sym, na.rm = TRUE),
      amplitude_abs = valor_max - valor_min,
      amplitude_rel = if_else(valor_med > 0, amplitude_abs / valor_med, NA_real_),
      dia_max       = yday(date[which.max(!!valor_sym)]),
      dia_min       = yday(date[which.min(!!valor_sym)]),
      centro_massa  = calcular_centro_massa(date, !!valor_sym),
      .groups = "drop"
    )
}

#' Orquestra o processamento da sazonalidade usando a estrutura do projeto
processar_sazonalidade_pipeline <- function(cfg, var_dirs, min_obs = 300) {
  
  message(sprintf("\n>>> Extraindo Sazonalidade (Jornalismo): %s", cfg$label))
  
  # Captura a coluna correta direto da configuração do projeto
  value_col <- cfg$value_col
  
  # Busca os parquets na pasta organizada oficial do run
  arquivos <- list.files(var_dirs$org_dir, pattern = "\\.parquet$", full.names = TRUE)
  
  if (length(arquivos) == 0) {
    message("Aviso: Nenhum arquivo organizado encontrado para esta variável.")
    return(NULL)
  }
  
  resultados <- vector("list", length(arquivos))
  
  for (i in seq_along(arquivos)) {
    arq <- arquivos[i]
    estacao <- tools::file_path_sans_ext(basename(arq))
    
    dados <- read_parquet(arq)
    
    if (!("date" %in% names(dados)) || !(value_col %in% names(dados))) next
    
    # Chuva usa ano civil (start = 1). Vazão/Cota podem usar 1 ou mês da cheia se preferir.
    metrica <- calcular_metricas_anuais(
      dados, 
      value_col = value_col, 
      start_month = 1, 
      min_obs = min_obs
    )
    
    if (nrow(metrica) == 0) next
    
    metrica$station_code <- estacao
    resultados[[i]] <- metrica
  }
  
  df_sazonalidade <- bind_rows(resultados)
  return(df_sazonalidade)
}

#' Gera a assinatura histórica resumida por estação
gerar_assinatura_sazonal <- function(df_metricas) {
  if (is.null(df_metricas) || nrow(df_metricas) == 0) return(tibble())
  
  df_metricas |>
    group_by(station_code) |>
    summarise(
      amp_abs      = median(amplitude_abs, na.rm = TRUE),
      amp_rel      = median(amplitude_rel, na.rm = TRUE),
      dia_max      = median(dia_max, na.rm = TRUE),
      dia_min      = median(dia_min, na.rm = TRUE),
      centro_massa = median(centro_massa, na.rm = TRUE),
      n_anos       = n(),
      .groups = "drop"
    )
}