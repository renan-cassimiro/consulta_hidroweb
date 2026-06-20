# No início do script 01_ana_pipeline.R
source("config/session_configs.R")
source("config/variable_configs.R")
source("R/setup.R")
source("R/inventory.R")
source("R/analysis.R")
source("R/summary.R")
source("R/seasonality.R")
source("R/plots.R")
source("R/consolidate.R")

# Controla se gera e salva os gráficos de tendência (pode ser lento para muitas estações)
GERAR_GRAFICOS_TENDENCIA <- FALSE

dirs <- setup_dirs(run_name = RUN_NAME)

# Lendo a área de estudo de forma 100% dinâmica
area_estudo <- sf::st_read(dirs$study_area_path)

# 2. Execução por Variável (Vazão, Cota, Chuva)
resultados_por_variavel <- list()

for (var_id in names(VARIABLE_CONFIGS)) {
  cfg <- VARIABLE_CONFIGS[[var_id]]
  
  message(sprintf("\n====== %s ======", toupper(var_id)))
  
  # Fase 1: Ingestão
  inv <- obter_inventario(cfg, area_estudo)
  
  
  df_bruto <- baixar_e_organizar(
    cfg        = cfg,
    inventario = inv,
    run_name   = RUN_NAME,
    raw_dir    = dirs$var_dirs$raw_dir,
    org_dir    = dirs$var_dirs$org_dir
  )
  
  df_limpo <- selecionar_estacoes(cfg, df_bruto)
    
  
  # Salvar parquets intermediários
  # arrow::write_parquet(df_limpo, paste0(dirs$data, "/", var_id, "_limpo.parquet"))
  
  # Fase 2: Análise
  resultados <- analisar_todas_estacoes(cfg, df_limpo)
  
  # -- 4e. Tabela resumo + séries empilhadas --------------------------------
  tabela_resumo    <- build_tabela_resumo(resultados, cfg)
  dados_empilhados <- build_dados_empilhados(resultados)
  # write_parquet(tabela_resumo, cfg$var_dirs$resumo)
  # message(sprintf("[%s] Resumo salvo: %d estações", cfg$label, nrow(tabela_resumo)))

  # 1. Executa o processamento matemático por estação/ano
  df_sazonal <- processar_sazonalidade_pipeline(cfg, var_dirs)
  
  
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
  
  # -- 4f. Gráficos de disponibilidade --------------------------------------
  img_disp <- path(dirs$image_dir, paste0("disponibilidade_", cfg$id, ".png"))
  ggsave(img_disp, df_limpo$plot, width = 14, height = 8, dpi = 150)
  message(sprintf("[%s] Disponibilidade salva: %s", cfg$label, img_disp))
  
  # -- 4g. Gráficos de tendência (opcional) ---------------------------------
  graficos <- NULL
  
  if (GERAR_GRAFICOS_TENDENCIA) {
    message(sprintf("[%s] Gerando gráficos de tendência...", cfg$label))
    
    graficos <- gerar_graficos(
      tabela_resumo    = tabela_resumo,
      dados_empilhados = dados_empilhados,
      inventario       = inv,
      area_estudo      = area_estudo,
      cfg              = cfg,
      rios             = rios,
      amacro           = amacro
    )
    
    # Adiciona plot de disponibilidade à lista de gráficos
    graficos$disponibilidade <- estacoes_sel$plot
    
    salvar_graficos(graficos, dirs$image_dir, cfg$id)
  }
  
  resultados_por_variavel[[var_id]] <- list(
    cfg              = cfg,
    inventario       = inv,
    dados_selecionados = df_limpo,
    resultados       = resultados,
    tabela_resumo    = tabela_resumo,
    dados_empilhados = dados_empilhados,
    graficos         = graficos)
    
}

resumos_list     <- map(resultados_por_variavel, "tabela_resumo")
inventarios_list <- map(resultados_por_variavel, "inventario")

consolidado <- consolidar_resultados(
  resumos_list    = resumos_list,
  inventario_list = inventarios_list)

salvar_consolidado(consolidado, dirs$consolidated_dir, RUN_NAME)
