# =============================================================================
# SAZONALIDADE HIDROLÓGICA (Foco: Jornalismo de Dados)
# =============================================================================
# Versão Corrigida: Inclui o cálculo estrito da Duração da Seca (Q90)
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

#' Calcula as métricas anuais para uma tabela diária (INCLUI DURAÇÃO DA SECA)
calcular_metricas_anuais <- function(df, value_col, start_month = 1, min_obs = 300) {
  valor_sym <- rlang::sym(value_col)
  
  # Remove NAs para não quebrar os cálculos estatísticos
  df_limpo <- df |> filter(!is.na(!!valor_sym))
  
  if (nrow(df_limpo) < min_obs) return(tibble())
  
  # Calcula o limiar Q90 histórico desta estação específica (o "fundo do poço" dela)
  q90_threshold <- quantile(df_limpo[[value_col]], probs = 0.10, na.rm = TRUE)
  
  df_limpo |>
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
      
      # CORREÇÃO: Conta o maior número de dias consecutivos abaixo do Q90 histórico
      duracao_seca_q90 = {
        is_seca <- !!valor_sym < q90_threshold
        r_le <- rle(is_seca)
        if (any(r_le$values)) max(r_le$lengths[r_le$values]) else 0
      },
      .groups = "drop"
    )
}

#' Orquestra o processamento da sazonalidade usando a estrutura do projeto
processar_sazonalidade_pipeline <- function(cfg, var_dirs, min_obs = 300) {
  
  message(sprintf("\n>>> Extraindo Sazonalidade (Jornalismo): %s", cfg$label))
  
  value_col <- cfg$value_col
  variable <- cfg$id
  arquivos  <- list.files(var_dirs$vars[[variable]]$org_dir, pattern = "\\.parquet$", full.names = TRUE)
  
  if (length(arquivos) == 0) {
    message("Aviso: Nenhum arquivo organizado encontrado para esta variável.")
    return(NULL)
  }
  
  resultados <- vector("list", length(arquivos))
  
  # REGRA JORNALÍSTICA DE ANO HIDROLÓGICO:
  # Para Vazão/Cota no Iriri, a seca bate forte em Setembro/Outubro.
  # Começar o ano em Novembro (11) evita cortar a seca ao meio na virada do ano civil.
  # Para Chuva (precipitation), mantemos o ano civil clássico (1).
  start_month <- if (cfg$id == "precipitation") 1 else 11
  
  for (i in seq_along(arquivos)) {
    arq <- arquivos[i]
    estacao <- tools::file_path_sans_ext(basename(arq))
    
    dados <- read_parquet(arq)
    if (!("date" %in% names(dados)) || !(value_col %in% names(dados))) next
    
    metrica <- calcular_metricas_anuais(
      dados, 
      value_col = value_col, 
      start_month = start_month, 
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
      duracao_seca = median(duracao_seca_q90, na.rm = TRUE), # Incluído na assinatura média
      n_anos       = n(),
      .groups = "drop"
    )
}