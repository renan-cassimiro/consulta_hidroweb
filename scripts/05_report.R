# 1. Carrega dependências de configuração e módulos de lógica
source("config/session_configs.R")
source("config/threshold_configs.R")
source("config/variable_configs.R")
source("R/setup.R")
source("R/analysis.R")
source("R/summary.R")
source("R/plots.R")
source("R/consolidate.R") # Carrega a nova função desenvolvida
source("R/inventory.R")
source("R/seasonality.R")

# Definições Locais de Execução (Sobrescreve se necessário)
VARIAVEIS_ATIVAS <- c("discharge", "water_level", "precipitation")

dirs <- setup_dirs(run_name = RUN_NAME)
# debugonce(ler_consolidado)
# 2. Executa a leitura robusta mapeada diretamente do Cache
dados_cache <- ler_consolidado(
  run_name                 = RUN_NAME,
  active_vars              = VARIAVEIS_ATIVAS,
  gerar_graficos_tendencia = TRUE,
  dirs                     = dirs
)

purrr::map(
  VARIABLE_CONFIGS[VARIAVEIS_ATIVAS],
  function(cfg) {
    # 1. Executa o processamento matemático por estação/ano
    df_sazonal <- processar_sazonalidade_pipeline(cfg, dirs)
    var_id <- cfg$id
    
    if (!is.null(df_sazonal) && nrow(df_sazonal) > 0) {
      
      # 2. Gera a assinatura histórica resumida (médias históricas)
      df_assinatura <- gerar_assinatura_sazonal(df_sazonal)
      
      # 3. Salva os dados brutos consolidados na pasta 'consolidated/'
      path_sazonal    <- path(dirs$consolidated_dir, paste0(RUN_NAME, "_sazonalidade_anual_", var_id, ".parquet"))
        path_assinatura <- path(dirs$consolidated_dir, paste0(RUN_NAME, "_assinatura_sazonal_media_", var_id, ".parquet"))
      
      write_parquet(df_sazonal,    path_sazonal)
      write_parquet(df_assinatura, path_assinatura)
      
      # 4. GERAÇÃO E SALVAMENTO DAS VISUALIZAÇÕES JORNALÍSTICAS
      message(sprintf("  [%s] Renderizando gráficos de sazonalidade...", var_id))
      lista_plots <- gerar_graficos_sazonalidade(df_sazonal, cfg)
      
      # Salva na pasta oficial de imagens do projeto (ex: output/xingu_river/images/)
      salvar_graficos_sazonalidade(lista_plots, dirs$image_dir, var_id)
      
      message(sprintf("  [%s] Processo concluído com sucesso!", var_id))
    }
  }
)

# print(count(filter(dados_cache$consolidado$spatial, station_code in )))
print(length(filter(dados_cache$consolidado$spatial , !is.null(tau_mk_discharge))$station_code))
print(length(filter(dados_cache$consolidado$spatial , !is.na(tau_mk_discharge))$station_code))
print(length(filter(dados_cache$consolidado$spatial , !is.null(tau_mk_discharge))$station_code))

# 3. Preparação de ambiente de saída e Renderização do PDF
message("\n====== [Compilação] Renderizando relatório integrado via Rmd ======")

caminho_template <- here("templates/relatorio_integrado.Rmd")
dir_saida        <- fs::path(dirs$output_dir, "results")
fs::dir_create(dir_saida)
arquivo_pdf      <- paste0("relatorio_integrado_", RUN_NAME, ".pdf")

rmarkdown::render(
  input       = caminho_template,
  output_file = arquivo_pdf,
  output_dir  = dir_saida,
  params      = list(
    resultados_por_variavel = dados_cache$resultados_por_variavel,
    consolidado             = dados_cache$consolidado,
    df_sazonalidade         = dados_cache$df_sazonalidade_discharge,
    estacoes_snap           = estacoes_snap,
    area_estudo             = dados_cache$area_estudo,
    rios                    = dados_cache$rios,
    run_name                = RUN_NAME
  ),
  envir  = new.env(parent = globalenv()),
  quiet  = FALSE
)

# Carrega dados gerados no pipeline anterior (ANA / Fases 1-3)
estacoes_snap <- st_read(PATHS_DEM$estacoes_snap)

View( estacoes_snap |> dplyr::filter(!is.na(area_contrib_km2)))

message("\n>>> Success! Relatório gerado dinamicamente em: ", dir_saida, "/", arquivo_pdf)

# resumos_list     <- map(dados_cache$resultados_por_variavel, "tabela_resumo")
# inventarios_list <- map(dados_cache$resultados_por_variavel, "inventario")
# 
# consolidado <- consolidar_resultados(
#   resumos_list    = resumos_list,
#   inventario_list = inventarios_list)
# 
# salvar_consolidado(dados_cache$consolidado, dirs$consolidated_dir, RUN_NAME)
# 