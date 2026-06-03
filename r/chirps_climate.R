# =============================================================================
# EXTRAÇÃO DE DADOS CLIMÁTICOS MENSAIS VIA RASTER (CHIRPS)
# =============================================================================
#
# Descrição:
#   Lê ficheiros raster anuais do GEE com bandas mensais (mes_01, mes_02...),
#   calcula a precipitação média espacial para os polígonos das bacias usando
#   {exactextractr} e guarda no padrão Parquet para integração no pipeline.
# =============================================================================

library(terra)
library(sf)
library(exactextractr)
library(tidyverse)
library(arrow)
library(fs)
library(lubridate)

#' Extrai a média espacial de rasters CHIRPS mensais para bacias hidrográficas
#'
#' @param dir_rasters Diretório onde estão os TIFs (ex: chirps_anual_stack_1988.tif)
#' @param bacias_sf   Objeto sf com os polígonos das bacias (deve conter 'station_code')
#' @param dir_saida   Diretório onde os ficheiros .parquet serão guardados
#' @export
processar_rasters_chirps_mensal <- function(dir_rasters, bacias_sf, dir_saida) {
  
  dir_create(dir_saida)
  
  # Garante WGS84 para evitar desalinhamentos com o raster do GEE
  bacias_sf <- st_transform(bacias_sf, 4326) 
  
  arquivos_tif <- dir_ls(dir_rasters, regexp = "\\.tif$")
  if (length(arquivos_tif) == 0) stop("Nenhum ficheiro .tif encontrado na pasta!")
  
  message(sprintf("\n====== A EXTRAIR %d RASTERS CHIRPS (MENSAL) ======", length(arquivos_tif)))
  
  dados_completos <- list()
  
  for (i in seq_along(arquivos_tif)) {
    arquivo <- arquivos_tif[i]
    nome_arquivo <- path_file(arquivo)
    
    # Extrai o ano do nome do ficheiro (ex: 1988)
    ano <- as.numeric(str_extract(nome_arquivo, "\\d{4}"))
    
    message(sprintf("[%d/%d] A processar chuva do ano %d...", i, length(arquivos_tif), ano))
    
    # O pacote terra lê os metadados do TIF instantaneamente
    r <- rast(arquivo)
    
    # exact_extract calcula a proporção exata de cada pixel dentro do polígono
    # O GEE exportou "mes_01", o exactextractr devolve "mean.mes_01"
    extracao <- exact_extract(r, bacias_sf, fun = "mean", progress = FALSE)
    extracao$station_code <- bacias_sf$station_code
    
    # Transforma as colunas em linhas (formato longo)
    df_long <- extracao |>
      pivot_longer(
        cols = starts_with("mean."), 
        names_to = "banda", 
        values_to = "precipitation_mm"
      ) |>
      mutate(
        # Puxa apenas os dígitos da string "mean.mes_01" -> 1
        mes_num = as.numeric(str_extract(banda, "\\d+")),
        # Constrói a data oficial (Dia 1 de cada mês)
        date = make_date(year = ano, month = mes_num, day = 1)
      ) |>
      select(station_code, date, precipitation_mm) |>
      # Remove potenciais NAs (se uma bacia cair fora do raster do GEE)
      filter(!is.na(precipitation_mm))
    
    dados_completos[[i]] <- df_long
  }
  
  message("\n[A consolidar e exportar os ficheiros Parquet...]")
  
  df_final <- bind_rows(dados_completos)
  estacoes <- unique(df_final$station_code)
  
  # Exporta um ficheiro por estação
  walk(estacoes, function(estacao) {
    df_estacao <- df_final |> 
      filter(station_code == estacao) |> 
      arrange(date)
    
    write_parquet(df_estacao, path(dir_saida, paste0(estacao, ".parquet")))
  })
  
  message(sprintf("Sucesso! %d ficheiros .parquet guardados na pasta: %s", length(estacoes), dir_saida))
}