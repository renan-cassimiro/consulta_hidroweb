# =============================================================================
# ANÁLISE DE DISPONIBILIDADE E TENDÊNCIAS HIDROLÓGICAS — PIPELINE PRINCIPAL
# =============================================================================
#
# Autor:   Renan Cassimiro Brito
# Data:    2026-05-13
#
# Descrição:
#   Orquestrador do pipeline de análise hidrológica da ANA.
#   Executa as três variáveis (vazão, cota, precipitação) em sequência usando
#   funções genéricas e consolida os resultados num único artefato espacial.
#
# Sequência de execução:
#   [x] 1. Setup de diretórios e configurações
#   [x] 2. Leitura da área de estudo e camadas geográficas
#   [x] 3. Para cada variável (discharge / water_level / precipitation):
#          a. Inventário de estações via API HidroWeb
#          b. Download + organização das séries
#          c. Seleção por critérios de qualidade
#          d. Análise estatística (STL, Mann-Kendall, Sen, Pettitt)
#          e. Tabela resumo + gráficos de tendência
#          f. Exportação dos gráficos de disponibilidade
#   [x] 4. Consolidação: long + wide + spatial (camada analítica final)
#   [ ] 5. Conectividade hidrológica (DEM → rede → igraph) — etapa futura
#   [ ] 6. Moran global + LISA                             — etapa futura
#   [ ] 7. Regionalização (PCA + clustering)               — etapa futura
#   [ ] 8. Sazonalidade hidrológica                        — etapa futura
#   [ ] 9. CHIRPS (tendência + anomalias precipitação)     — etapa futura
#   [ ] 10. Chuva × vazão (elasticidade + residual)        — etapa futura
#   [ ] 11. Pressão territorial (desmatamento + fogo)      — etapa futura
#
# Inputs:
#   input/{RUN_NAME}/
#   ├── {RUN_NAME}_area_estudo.gpkg
#   ├── ne_10m_rivers_lake_centerlines_bacias_amacro.gpkg  (opcional)
#   └── AMACRO/AMACRO.shp                                  (opcional)
#
# Outputs:
#   output/{RUN_NAME}/
#   ├── data/
#   │   ├── {RUN_NAME}_inventario_{variable}.parquet
#   │   ├── stations_{variable}/           — séries brutas por estação
#   │   └── stations_{variable}_organized/ — séries organizadas
#   ├── images/
#   │   ├── disponibilidade_{variable}.png
#   │   ├── tendencias_estacoes_{variable}.png
#   │   ├── mapa_tendencias_classificadas_{variable}.png
#   │   └── ...
#   ├── consolidated/
#   │   ├── {RUN_NAME}_disponibilidade_long.parquet
#   │   ├── {RUN_NAME}_disponibilidade_wide.parquet
#   │   └── {RUN_NAME}_disponibilidade_spatial.parquet
#   └── resumo_disponibilidade_{variable}.parquet
#
# Dependências:
#   CRAN: arrow, fs, here, hydrobr, Kendall, lubridate, patchwork, purrr,
#         scales, sf, sfarrow, tidyverse, trend, zyp, zoo
#   GitHub: devtools::install_github("hydroversebr/hydrobr", build_vignettes = FALSE)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. MÓDULOS DO PROJETO
# -----------------------------------------------------------------------------
source(here::here("config/variable_configs.R"))
source(here::here("R/setup.R"))        # setup_dirs() + bibliotecas
source(here::here("R/inventory.R"))    # obter_inventario(), baixar_e_organizar(), selecionar_estacoes()
source(here::here("R/analysis.R"))     # analisar_estacao(), analisar_todas_estacoes()
source(here::here("R/summary.R"))      # build_tabela_resumo(), build_dados_empilhados()
source(here::here("R/plots.R"))        # gerar_graficos(), salvar_graficos()
source(here::here("R/consolidate.R"))  # consolidar_resultados(), salvar_consolidado()
source(here::here("R/report.R"))          # renderizar_todos()
source(here::here("R/seasonality.R"))
source(here::here("R/chirps_climate.R"))
source(here::here("R/residual_hidrologico.R"))

# -----------------------------------------------------------------------------
# 1. CONFIGURAÇÃO DO RUN
# -----------------------------------------------------------------------------
RUN_NAME <- "amacro"
USAR_CACHE_LOCAL <- TRUE  # TRUE = Lê ficheiros Parquet gravados; FALSE = Força download da ANA

# Variáveis a processar — subconjunto de VARIABLE_CONFIGS.
# Para rodar só precipitação: VARIAVEIS_ATIVAS <- c("precipitation")
VARIAVEIS_ATIVAS <- c("discharge", "water_level", "precipitation")

# Filtro de estações: NULL usa todas do inventário.
# Para filtrar: estacoes_filtradas <- st_read(path(dirs$input_dir, "estacoes.gpkg"))
ESTACOES_FILTRADAS <- NULL

# Controla se gera e salva os gráficos de tendência (pode ser lento para muitas estações)
GERAR_GRAFICOS_TENDENCIA <- TRUE

# -----------------------------------------------------------------------------
# 2. SETUP DE DIRETÓRIOS
# -----------------------------------------------------------------------------
dirs <- setup_dirs(RUN_NAME)

# -----------------------------------------------------------------------------
# 3. LEITURA DAS CAMADAS GEOGRÁFICAS
# -----------------------------------------------------------------------------
area_estudo <- st_read(dirs$study_area_path, quiet = TRUE)

# Camadas opcionais — se ausentes, os mapas são gerados sem elas
rios_path   <- path(dirs$input_dir, "ne_10m_rivers_lake_centerlines_bacias_amacro.gpkg")
amacro_path <- path(dirs$input_dir, "AMACRO", "AMACRO.shp")

rios   <- if (file_exists(rios_path))   st_read(rios_path,   quiet = TRUE) else NULL
amacro <- if (file_exists(amacro_path)) st_read(amacro_path, quiet = TRUE) else NULL

if (is.null(rios))   message("Aviso: camada de rios não encontrada — mapas gerados sem hidrografia.")
if (is.null(amacro)) message("Aviso: camada AMACRO não encontrada — G5 gerado sem polígono AMACRO.")

# Mapa rápido de conferência da área de estudo
ggplot() +
  geom_sf(data = area_estudo, fill = NA, color = "red", linewidth = 1) +
  { if (!is.null(rios)) geom_sf(data = rios, color = "steelblue", linewidth = 0.4) } +
  theme_classic() +
  labs(title = paste("Área de estudo —", RUN_NAME))

# -----------------------------------------------------------------------------
# 4. PIPELINE POR VARIÁVEL
# -----------------------------------------------------------------------------
# Cada elemento de `resultados_por_variavel` contém:
#   $cfg, $inventario, $dados_selecionados, $resultados,
#   $tabela_resumo, $dados_empilhados, $graficos (se GERAR_GRAFICOS_TENDENCIA)
resultados_por_variavel <- map(
  VARIABLE_CONFIGS[VARIAVEIS_ATIVAS],
  function(cfg) {
    
    message(sprintf("\n====== %s ======", toupper(cfg$label)))
    
    var_dirs <- dirs$vars[[cfg$id]]
    
    # -- 4a. Inventário --------------------------------------------------------
    inventario <- obter_inventario(
      cfg                = cfg,
      area_estudo        = area_estudo,
      output_path        = var_dirs$inventario,
      estacoes_filtradas = ESTACOES_FILTRADAS
    )
    
    # -- 4b. Download + organização -------------------------------------------
    ficheiros_existentes <- list.files(var_dirs$org_dir, pattern = "\\.parquet$", full.names = TRUE)
    
    if (USAR_CACHE_LOCAL && length(ficheiros_existentes) > 0) {
      message(sprintf("  [%s] A carregar %d estações do disco local (Download ignorado)", 
                      cfg$label, length(ficheiros_existentes)))
      
      # Carrega os ficheiros Parquet gravados na sessão anterior
      dados_org <- purrr::map(ficheiros_existentes, arrow::read_parquet)
      # Nomeia a lista com o código da estação (extraído do nome do ficheiro)
      names(dados_org) <- tools::file_path_sans_ext(basename(ficheiros_existentes))
      
    } else {
      message(sprintf("  [%s] A descarregar dados da API da ANA...", cfg$label))
      dados_org <- baixar_e_organizar(
        cfg        = cfg,
        inventario = inventario,
        run_name   = RUN_NAME,
        raw_dir    = var_dirs$raw_dir,
        org_dir    = var_dirs$org_dir
      )
    }
    
    # -- 4c. Seleção por qualidade --------------------------------------------
    estacoes_sel <- selecionar_estacoes(cfg, dados_org)
    
    # -- 4d. Análise estatística ----------------------------------------------
    resultados <- analisar_todas_estacoes(estacoes_sel, cfg)
    
    # -- 4e. Tabela resumo + séries empilhadas --------------------------------
    tabela_resumo    <- build_tabela_resumo(resultados, cfg)
    dados_empilhados <- build_dados_empilhados(resultados)
    
    write_parquet(tabela_resumo, var_dirs$resumo)
    message(sprintf("[%s] Resumo salvo: %d estações", cfg$label, nrow(tabela_resumo)))
    
    # -- 4f. Gráficos de disponibilidade --------------------------------------
    img_disp <- path(dirs$image_dir, paste0("disponibilidade_", cfg$id, ".png"))
    ggsave(img_disp, estacoes_sel$plot, width = 14, height = 8, dpi = 150)
    message(sprintf("[%s] Disponibilidade salva: %s", cfg$label, img_disp))
    
    # -- 4g. Gráficos de tendência (opcional) ---------------------------------
    graficos <- NULL
    
    if (GERAR_GRAFICOS_TENDENCIA) {
      message(sprintf("[%s] Gerando gráficos de tendência...", cfg$label))
      
      graficos <- gerar_graficos(
        tabela_resumo    = tabela_resumo,
        dados_empilhados = dados_empilhados,
        inventario       = inventario,
        area_estudo      = area_estudo,
        cfg              = cfg,
        rios             = rios,
        amacro           = amacro
      )
      
      # Adiciona plot de disponibilidade à lista de gráficos
      graficos$disponibilidade <- estacoes_sel$plot
      
      salvar_graficos(graficos, dirs$image_dir, cfg$id)
    }
    
    list(
      cfg              = cfg,
      inventario       = inventario,
      dados_selecionados = estacoes_sel,
      resultados       = resultados,
      tabela_resumo    = tabela_resumo,
      dados_empilhados = dados_empilhados,
      graficos         = graficos
    )
  }
)

# -----------------------------------------------------------------------------
# 5. CONSOLIDAÇÃO DOS RESULTADOS
# -----------------------------------------------------------------------------
message("\n====== CONSOLIDANDO RESULTADOS ======")

resumos_list     <- map(resultados_por_variavel, "tabela_resumo")
inventarios_list <- map(resultados_por_variavel, "inventario")

consolidado <- consolidar_resultados(
  resumos_list    = resumos_list,
  inventario_list = inventarios_list
)

salvar_consolidado(consolidado, dirs$consolidated_dir, RUN_NAME)
#TODO implementar uma funação para recuperar os dados baixados que foram gravados
# -----------------------------------------------------------------------------
# 6. CAMADA ANALÍTICA ESPACIAL (analysed_stations)
# -----------------------------------------------------------------------------
# O objeto `consolidado$spatial` já é o sf consolidado (wide + geometria).
# Esta etapa apenas reporta e salva uma cópia nomeada para uso nas etapas
# futuras (Moran, regionalização, conectividade, etc.)

analysed_stations <- consolidado$spatial

if (!is.null(analysed_stations)) {
  glimpse(analysed_stations)
  cat(sprintf("\nEstações na camada analítica: %d\n", nrow(analysed_stations)))
  cat(sprintf("CRS: %s\n", st_crs(analysed_stations)$input))
  cat(sprintf("Variáveis consolidadas: %s\n",
              paste(VARIAVEIS_ATIVAS, collapse = ", ")))
}


# -----------------------------------------------------------------------------
# 7. RELATÓRIOS PDF
# -----------------------------------------------------------------------------
# Requer LaTeX. Para instalar: tinytex::install_tinytex()
# Gera: report/relatorio_{variavel}.pdf  (um por variável)
#        report/relatorio_consolidado_{run_name}.pdf
GERAR_RELATORIOS <- TRUE

if (GERAR_RELATORIOS) {
  message("\n====== GERANDO RELATÓRIOS PDF ======")
  renderizar_todos(
    resultados_por_variavel = resultados_por_variavel,
    consolidado             = consolidado,
    run_name                = RUN_NAME,
    dirs                    = dirs
  )
}

# -----------------------------------------------------------------------------
# 8. VERIFICAÇÃO FINAL
# -----------------------------------------------------------------------------
message("\n====== PIPELINE CONCLUÍDO ======")
message(sprintf("Run:       %s", RUN_NAME))
message(sprintf("Variáveis: %s", paste(VARIAVEIS_ATIVAS, collapse = ", ")))

walk(VARIAVEIS_ATIVAS, function(v) {
  n <- nrow(resultados_por_variavel[[v]]$tabela_resumo)
  message(sprintf("  %-15s → %d estações com análise", v, n))
})

message(sprintf(
  "Consolidado — long: %d linhas | wide: %d colunas | spatial: %s",
  nrow(consolidado$long),
  ncol(consolidado$wide),
  if (!is.null(consolidado$spatial)) paste(nrow(consolidado$spatial), "feições") else "não gerado"
))

# -----------------------------------------------------------------------------
# 9. ANÁLISE DE SAZONALIDADE HIDROLÓGICA 
# -----------------------------------------------------------------------------
#TODO acho que tá faltando um mapa aqui
message("\n====== PROCESSANDO SAZONALIDADE ======")

# Loop dinâmico pelas variáveis ativas (vazão, cota, chuva)
walk(VARIAVEIS_ATIVAS, function(v) {
  cfg      <- VARIABLE_CONFIGS[[v]]
  var_dirs <- dirs$vars[[v]]
  
  # 1. Executa o processamento matemático por estação/ano
  df_sazonal <- processar_sazonalidade_pipeline(cfg, var_dirs)
  
  if (!is.null(df_sazonal) && nrow(df_sazonal) > 0) {
    
    # 2. Gera a assinatura histórica resumida (médias históricas)
    df_assinatura <- gerar_assinatura_sazonal(df_sazonal)
    
    # 3. Salva os dados brutos consolidados na pasta 'consolidated/'
    path_sazonal    <- path(dirs$consolidated_dir, paste0(RUN_NAME, "_sazonalidade_anual_", v, ".parquet"))
    path_assinatura <- path(dirs$consolidated_dir, paste0(RUN_NAME, "_assinatura_sazonal_media_", v, ".parquet"))
    
    write_parquet(df_sazonal,    path_sazonal)
    write_parquet(df_assinatura, path_assinatura)
    
    # 4. GERAÇÃO E SALVAMENTO DAS VISUALIZAÇÕES JORNALÍSTICAS
    message(sprintf("  [%s] Renderizando gráficos de sazonalidade...", v))
    lista_plots <- gerar_graficos_sazonalidade(df_sazonal, cfg)
    
    # Salva na pasta oficial de imagens do projeto (ex: output/xingu_river/images/)
    salvar_graficos_sazonalidade(lista_plots, dirs$image_dir, v)
    
    message(sprintf("  [%s] Processo concluído com sucesso!", v))
  }
})

#TODO
# -----------------------------------------------------------------------------
# 10. ANÁLISE DE SAZONALIDADE HIDROLÓGICA 
# -----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# 1. CAMINHOS BASE
# ----------------------------------------------------------------------------
OUTPUT_DIR <- here("output", RUN_NAME)
DATA_DIR <- path(OUTPUT_DIR, "data")
DEM_DIR <- path(DATA_DIR, "dem")
d8_pointer_raster <- path(DEM_DIR, paste0(RUN_NAME, "_flow_direction.tif"))
dir_tmp           <- here("output/xingu_river/data/watershed/temp")
dir_create(dir_tmp) # Pasta temporária para os rasters de cada estação

# Carrega os pontos que já sofreram o SNAP (devem estar na mesma projeção do DEM, ex: 5880)
#Rodar rede hidrografica quando chegar aqui
estacoes_snap <- st_read(here("output/amacro/data/amacro_snapped_stations.gpkg"))
codigos_estacoes <- unique(estacoes_snap$station_code)

message(sprintf("\n====== INICIANDO DELIMITAÇÃO INDIVIDUAL PARA %d ESTAÇÕES ======", length(codigos_estacoes)))

# Lista para armazenar os polígonos de cada bacia
lista_bacias <- list()

# ----------------------------------------------------------------------------
# 2. LOOP POR ESTAÇÃO (Garante o acúmulo real a montante)
# ----------------------------------------------------------------------------
for (i in seq_along(codigos_estacoes)) {
  cod <- codigos_estacoes[i]
  message(sprintf("[%d/%d] Delimitando bacia da estação: %s", i, length(codigos_estacoes), cod))
  
  # Caminhos específicos desta estação
  pt_shp  <- path(dir_tmp, paste0("pt_", cod, ".shp"))
  out_tif <- path(dir_tmp, paste0("wsh_", cod, ".tif"))
  
  # 1. Filtra e isola apenas o ponto desta estação
  ponto_individual <- estacoes_snap |> filter(station_code == cod) |> select(station_code)
  st_write(ponto_individual, pt_shp, delete_dsn = TRUE, quiet = TRUE)
  
  # 2. Executa o Watershed do Whitebox apenas para este ponto
  wbt_watershed(
    d8_pntr  = d8_pointer_raster,
    pour_pts = pt_shp,
    output   = out_tif
  )
  
  # 3. Lê o raster gerado e transforma em polígono se ele existir e for válido
  # if (file_exists(out_tif)) {
    r_bacia <- rast(out_tif)
    
    # Verifica se o raster não está vazio (pode acontecer se o snap falhou drasticamente)
    # if (global(r_bacia, "not_na")$not_na > 0) {
      poligono_sf <- as.polygons(r_bacia) |> 
        st_as_sf() |> 
        st_transform(4326) |>             # Converte para WGS84 para o CHIRPS
        mutate(station_code = cod) |>     # Injeta o ID correto
        select(station_code, geometry)
      
      lista_bacias[[cod]] <- poligono_sf
    # } else {
      # warning(sprintf("A bacia da estação %s gerou um raster vazio. Verifique o snap.", cod))
    # }
  # }
}

# ----------------------------------------------------------------------------
# 3. CONSOLIDANDO AS GEOMETRIAS SOBREPOSTAS
# ----------------------------------------------------------------------------
message("\n[Consolidando polígonos e salvando produto final...]")

bacias_cumulativas_todas <- bind_rows(lista_bacias)

# Salva o arquivo final com todas as bacias cumulativas
st_write(
  bacias_cumulativas_todas, 
  here::here("output/amacro/data/bacias_contribuicao_cumulativa.gpkg"),
  delete_dsn = TRUE
)

# Limpa a pasta temporária para poupar espaço em disco
dir_delete(dir_tmp)

message("Sucesso! O arquivo 'bacias_contribuicao_cumulativa.gpkg' foi gerado com bacias sobrepostas reais.")


# 2. Definir a pasta onde o CHIRPS vai ficar guardado
# Sugestão: crie uma "variável falsa" na sua arquitetura chamada "chirps"
pasta_chirps_organizado <- here::here("output/amacro/data/stations_chirps/organized")

# # 3. Disparar o download
# obter_chirps_para_bacias(bacias_sf = bacias_sf, dir_saida = pasta_chirps_organizado, 
#   data_inicio = "2000-01-01", data_fim = "2025-12-31")

# 3. Roda a extração
processar_rasters_chirps_mensal(
  dir_rasters = "input/amacro/chirps_anual_stack",
  bacias_sf   = bacias_cumulativas_todas,
  dir_saida   = pasta_chirps_organizado
)


# ----------------------------------------------------------------------------
# 3. CONSOLIDANDO AS GEOMETRIAS SOBREPOSTAS
# ----------------------------------------------------------------------------

# Configuração dos caminhos
pasta_vazao_ana  <- here::here("output/amacro/data/stations_discharge_organized") # Ajuste para a sua pasta da ANA
pasta_chuva_gee  <- here::here("output/amacro/data/stations_chirps/organized")
pasta_resultados <- here::here("output/amacro/results")

# Executa o modelo
tabela_reportagem <- analisar_residual_hidrologico(
  dir_vazao  = pasta_vazao_ana,
  col_vazao  = "stream_flow_m3_s", # Insira o nome correto da coluna do seu parquet da ANA
  dir_chirps = pasta_chuva_gee,
  dir_saida  = pasta_resultados
)

# Dispara a geração
gerar_graficos_reportagem(
  caminho_resultado = here::here("output/amacro/results/residual_hidrologico_consolidado.parquet"),
  dir_saida         = here::here("output/amacro/results/plots")
)


# Caminhos dos arquivos
caminho_parquet <- here::here("output/amacro/results/residual_hidrologico_consolidado.parquet")
caminho_gpkg    <- here::here("output/amacro/data/estacoes_analise.gpkg")
dir_graficos    <- here::here("output/amacro/results/plots_consolidados")

# (Opcional) Carrega hidrografia para enfeitar o mapa
# rios <- st_read(here::here("output/nomedorun/data/rede_drenagem.gpkg"))

gerar_visao_consolidada(
  caminho_resultado   = caminho_parquet,
  caminho_estacoes_sf = estacoes_snap,
  dir_saida           = dir_graficos
  # rios_sf           = rios  # Descomente se for usar o fundo dos rios
)

# -----------------------------------------------------------------------------
# 10. VERIFICAÇÃO FINAL
# -----------------------------------------------------------------------------
message("\n====== PIPELINE CONCLUÍDO ======")
message(sprintf("Run:       %s", RUN_NAME))
message(sprintf("Variáveis: %s", paste(VARIAVEIS_ATIVAS, collapse = ", \\")))
