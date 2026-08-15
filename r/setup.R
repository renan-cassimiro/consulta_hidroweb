# =============================================================================
# SETUP — Bibliotecas e estrutura de diretórios
# =============================================================================
#
# Descrição:
#   Carrega todas as dependências e cria a estrutura de pastas do projeto.
#   Deve ser sourced uma única vez no início de main.R.
#
# =============================================================================

# -----------------------------------------------------------------------------
# Dependências
# -----------------------------------------------------------------------------
library(arrow)
library(fs)
library(here)
library(hydrobr)
library(Kendall)
library(lubridate)
library(patchwork)
library(purrr)
library(scales)
library(sf)
library(sfarrow)
library(tidyverse)
library(trend)
library(zyp)
library(zoo)
library(ggtext) # Para subtítulos e formatação de texto avançada
library(whitebox)
library(ggrepel) # Para os nomes das estações não se sobreporem no gráfico
library(terra)
library(rmarkdown)



# -----------------------------------------------------------------------------
# Criação da estrutura de diretórios para um dado run_name
# -----------------------------------------------------------------------------
#
#' Cria e retorna todos os caminhos de saída para um run
#'
#' @param run_name  character. Nome do run (ex: "amacro"). Usado como subpasta
#'                  em output/ e como prefixo nos arquivos gerados.
#' @param base_input  character. Caminho raiz de input (padrão: here("input")).
#' @param base_output character. Caminho raiz de output (padrão: here("output")).
#'
#' @return Lista nomeada com todos os caminhos relevantes do projeto:
#'   $input_dir, $study_area_path, $output_dir, $report_dir, $image_dir,
#'   $data_dir, $consolidated_dir, e subcaminhos por variável em $vars[[id]]
#'
setup_dirs <- function(run_name,
                       base_input  = here("input"),
                       base_output = here("output")) {
  
  input_dir  <- path(base_input,  run_name)
  output_dir <- path(base_output, run_name)
  
  dirs <- list(
    input_dir        = input_dir,
    
    # Camadas de entrada resolvidas dinamicamente:
    study_area_path  = path(input_dir, INPUT_FILES$study_area_filename),
    dem_path         = path(input_dir, INPUT_FILES$dem_filename),
    chirps_dir       = path(input_dir, "chirps_anual_stack"),
    
    # Estrutura de saídas
    output_dir       = output_dir,
    report_dir       = path(output_dir, "report"),
    image_dir        = path(output_dir, "images"),
    data_dir         = path(output_dir, "data"),
    consolidated_dir = path(output_dir, "consolidated"),
    watershed_dir = path(output_dir, "watershed"),
    temp_dir = path(output_dir, "temp")
    
  )
  
  # Cria os diretórios base de saída
  walk(
    c(dirs$report_dir, dirs$image_dir, dirs$data_dir, dirs$consolidated_dir),
    dir_create, recurse = TRUE
  )
  
  # Cria subdiretórios por variável (raw + organized)
  dirs$vars <- map(VARIABLE_CONFIGS, function(cfg) {
    raw_dir <- path(dirs$data_dir, paste0("stations_", cfg$id))
    org_dir <- path(dirs$data_dir, paste0("stations_", cfg$id, "_organized"))
    dir_create(raw_dir, recurse = TRUE)
    dir_create(org_dir, recurse = TRUE)
    list(
      raw_dir      = raw_dir,
      org_dir      = org_dir,
      org_inventario   = path(dirs$data_dir, paste0(run_name, "_organized_inventario_", cfg$id, ".parquet")),
      inventario   = path(dirs$data_dir, paste0(run_name, "_inventario_", cfg$id, ".parquet")),
      resumo       = path(output_dir,    paste0("resumo_disponibilidade_", cfg$id, ".parquet"))
    )
  })
  
  return(dirs)
}