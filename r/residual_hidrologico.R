# =============================================================================
# MODELAGEM HIDROLOGICA — ELASTICIDADE E RESIDUAL
# =============================================================================
#
# Descrição:
#   Cruza os dados anuais de chuva (CHIRPS) e vazão (ANA). Ajusta um modelo
#   estatístico para estimar a vazão esperada pela chuva e calcula o residual
#   temporal para identificar perda de resiliência da bacia.
# =============================================================================

library(tidyverse)
library(arrow)
library(fs)
library(lubridate)

#' Calcula o Residual Hidrológico e Elasticidade por Estação
#'
#' @param dir_vazao   Pasta com os parquets de vazão da ANA (ex: do hydrobr)
#' @param col_vazao   Nome exato da coluna de vazão no seu parquet (ex: "flow" ou "consistido")
#' @param dir_chirps  Pasta com os parquets do CHIRPS (gerados na etapa anterior)
#' @param dir_saida   Pasta onde os resultados consolidados serão salvos
#' 
#' @export
analisar_residual_hidrologico <- function(dir_vazao, col_vazao, dir_chirps, dir_saida) {
  
  dir_create(dir_saida)
  
  # Lista as estações disponíveis no CHIRPS
  ficheiros_chirps <- dir_ls(dir_chirps, regexp = "\\.parquet$")
  
  lista_resultados_estacoes <- list()
  
  message("\n====== INICIANDO ANÁLISE DE RESIDUAL HIDROLÓGICO ======")
  
  for (i in seq_along(ficheiros_chirps)) {
    f_chirps <- ficheiros_chirps[i]
    estacao_cod <- path_ext_remove(path_file(f_chirps))
    
    #TODO retirar esse hard code
    f_vazao <- path(dir_vazao, paste0("amacro_", estacao_cod, ".parquet"))
    
    # Só prossegue se tiver os dois dados para a mesma estação
    if (!file_exists(f_vazao)) {
      message(sprintf("[%d/%d] Sem dados de vazão para a estação %s. A saltar...", i, length(ficheiros_chirps), estacao_cod))
      next
    }
    
    message(sprintf("[%d/%d] A modelar balanço hídrico para a estação %s...", i, length(ficheiros_chirps), estacao_cod))
    
    # 1. Carregar e agregar CHIRPS (Anual)
    df_chuva_anual <- read_parquet(f_chirps) |> 
      mutate(ano = year(date)) |> 
      group_by(ano) |> 
      summarise(
        chuva_anual_mm = sum(precipitation_mm, na.rm = TRUE),
        meses_com_chuva = n(),
        .groups = "drop"
      ) |> 
      # Garante que o ano teve os 12 meses preenchidos
      filter(meses_com_chuva == 12)
    
    # 2. Carregar e agregar VAZÃO da ANA (Anual)
    df_vazao_anual <- read_parquet(f_vazao) |> 
      mutate(ano = year(date)) |> 
      rename(vazao_var = all_of(col_vazao)) |> 
      group_by(ano) |> 
      summarise(
        vazao_media_m3s = mean(vazao_var, na.rm = TRUE),
        dias_com_dados = sum(!is.na(vazao_var)),
        .groups = "drop"
      ) |> 
      # Filtro de qualidade jornalística: exige pelo menos 300 dias de dados no ano
      filter(dias_com_dados >= 300)
    
    # 3. Cruzamento dos dados (Inner Join por Ano)
    df_banco_anual <- inner_join(df_chuva_anual, df_vazao_anual, by = "ano")
    
    # Se o histórico comum for muito curto (menos de 7 anos), não dá para fazer regressão segura
    if (nrow(df_banco_anual) < 7) {
      message(sprintf("  -> [Aviso] Histórico comum insuficiente (%d anos).", nrow(df_banco_anual)))
      next
    }
    
    # 4. A REGRESSÃO: Vazão explicada pela Chuva
    modelo <- lm(vazao_media_m3s ~ chuva_anual_mm, data = df_banco_anual)
    
    # 5. Extração dos Residuais e Elasticidade
    df_resultado <- df_banco_anual |> 
      mutate(
        station_code = estacao_cod,
        vazao_predita_m3s = predict(modelo, newdata = df_banco_anual),
        residual_m3s = vazao_media_m3s - vazao_predita_m3s,
        # Residual em percentagem (mais fácil de explicar na matéria)
        residual_pct = (residual_m3s / vazao_predita_m3s) * 100
      )
    
    # 6. Calcular Elasticidade Média da Estação (Medida de sensibilidade)
    # % variação da vazão / % variação da chuva (em relação à média histórica)
    media_p <- mean(df_resultado$chuva_anual_mm)
    media_q <- mean(df_resultado$vazao_media_m3s)
    
    # Coeficiente angular (Beta 1) da regressão
    beta_1 <- coef(modelo)[2]
    elasticidade_climatica <- beta_1 * (media_p / media_q)
    
    df_resultado <- df_resultado |> 
      mutate(elasticidade_estacao = elasticidade_climatica)
    
    lista_resultados_estacoes[[estacao_cod]] <- df_resultado
  }
  
  # Consolida todas as estações numa única tabela
  df_final_projeto <- bind_rows(lista_resultados_estacoes)
  
  # Guarda em formatos fáceis para a produção da reportagem
  write_parquet(df_final_projeto, path(dir_saida, "residual_hidrologico_consolidado.parquet"))
  write_csv(df_final_projeto, path(dir_saida, "residual_hidrologico_consolidado.csv"))
  
  message(sprintf("\nProcesso concluído! Tabela final salva em: %s", dir_saida))
  return(df_final_projeto)
}