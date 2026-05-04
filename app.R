#
# Shiny App - Série Histórica de Cotas - HidroWebService (ANA)
# Migrado de HidroWeb REST (snirh.gov.br) para HidroWebService (ana.gov.br)
#
# A API antiga retornava arquivos .zip com CSVs de formato legado.
# A nova API retorna JSON estruturado diretamente, sem necessidade de
# descompactar ou parsear colunas posicionais.
#
# Endpoint principal: /EstacoesTelemetricas/HidroSerieCotas/v1
#
# IMPORTANTE: A API limita cada requisição a 366 dias. Para séries longas
# (décadas), o app quebra automaticamente o intervalo em janelas anuais
# e faz múltiplas chamadas, concatenando os resultados ao final.
#
# Autenticação: OAuth com token Bearer de validade de 60 minutos.
# O app reutiliza o token enquanto válido para evitar bloqueio de IP
# por excesso de requisições de autenticação (conforme alerta da ANA).
#

library(shiny)
library(httr)            # Chamadas HTTP à API REST
library(jsonlite)        # Parse do JSON retornado pela API
library(data.table)      # Manipulação eficiente de tabelas
library(dplyr)           # Verbos de transformação de dados
library(lubridate)       # Manipulação de datas (years(), days(), floor_date())
library(leaflet)         # Mapa interativo das estações
library(sf)              # Leitura do GeoJSON da rede hidrometeorológica
library(shinythemes)     # Tema visual do app (flatly)
library(dygraphs)        # Gráficos de séries temporais interativos
library(shinyalert)      # Popups de alerta/erro para o usuário
library(shinycssloaders) # Spinner de carregamento nos gráficos

# -------------------------------------------------------------------------
# URLs da API HidroWebService (ANA)
#
# ATENÇÃO: Os nomes dos parâmetros de query usam os labels exatos do Swagger
# da ANA, incluindo espaços, acentos e parênteses — por isso os backticks
# no R ao montar as listas de query. O httr faz o URL-encoding automaticamente.
# Exemplo da URL que funciona no Swagger:
#   ...?Código da Estação=83900000&Tipo Filtro Data=DATA_LEITURA&...
# Usando camelCase (ex: CodigoDaEstacao) a API retorna erro 406.
# -------------------------------------------------------------------------
BASE_URL       <- "https://www.ana.gov.br/hidrowebservice/EstacoesTelemetricas"
URL_TOKEN      <- paste0(BASE_URL, "/OAUth/v1")                  # Autenticação OAuth
URL_COTAS      <- paste0(BASE_URL, "/HidroSerieCotas/v1")        # Série histórica de cotas
URL_INVENTARIO <- paste0(BASE_URL, "/HidroInventarioEstacoes/v1") # Metadados da estação

# -------------------------------------------------------------------------
# Cache do token OAuth
#
# Usamos um environment separado (fora do servidor Shiny) para que o token
# persista entre sessões no mesmo processo R — evitando autenticações
# repetidas quando múltiplos usuários acessam o app ao mesmo tempo.
# -------------------------------------------------------------------------
token_cache            <- new.env(parent = emptyenv())
token_cache$token      <- NULL  # Valor do token Bearer
token_cache$emitido_em <- NULL  # Horário em que o token foi emitido

# -------------------------------------------------------------------------
# obter_token()
#
# Autentica no HidroWebService e retorna o token Bearer.
# Reutiliza o token em cache se ele ainda tiver mais de 5 minutos de vida
# (o token dura 60 min; renovamos com folga de 5 min = limite de 55 min).
#
# Parâmetros:
#   identificador — CPF ou CNPJ cadastrado na ANA
#   senha         — senha recebida por e-mail após aprovação do cadastro
# -------------------------------------------------------------------------
obter_token <- function(identificador, senha) {
  agora <- Sys.time()

  # Verifica se o token em cache ainda é válido (menos de 55 min de uso)
  if (!is.null(token_cache$token) &&
      !is.null(token_cache$emitido_em) &&
      difftime(agora, token_cache$emitido_em, units = "mins") < 55) {
    return(token_cache$token)
  }

  # Faz a requisição de autenticação enviando credenciais nos headers HTTP
  resp <- tryCatch(
    GET(URL_TOKEN,
        add_headers(Identificador = identificador, Senha = senha),
        timeout(30)),
    error = function(e) NULL
  )

  if (is.null(resp) || status_code(resp) != 200) {
    stop("Falha na autenticação. Verifique seu identificador e senha.")
  }

  # Extrai o token do campo 'tokenautenticacao' no JSON de resposta
  corpo <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  tk    <- corpo$items$tokenautenticacao

  if (is.null(tk) || tk == "") {
    stop("Token não retornado pela API. Verifique suas credenciais.")
  }

  # Salva token e horário de emissão no cache para reutilização
  token_cache$token      <- tk
  token_cache$emitido_em <- agora
  return(tk)
}

# -------------------------------------------------------------------------
# buscar_cotas_janela()
#
# Faz UMA requisição ao endpoint HidroSerieCotas para uma janela de até
# 366 dias. Retorna um data.frame com os registros, ou NULL em caso de erro.
#
# Parâmetros:
#   token          — token Bearer obtido via obter_token()
#   codigo_estacao — código ANA da estação (ex: "83900000")
#   data_inicio    — data inicial da janela (objeto Date do R)
#   data_fim       — data final da janela (objeto Date do R)
# -------------------------------------------------------------------------
buscar_cotas_janela <- function(token, codigo_estacao, data_inicio, data_fim) {
  resp <- tryCatch(
    GET(URL_COTAS,
        add_headers(
          Authorization = paste("Bearer", token), # Token Bearer no header
          Accept        = "application/json"       # Evita erro 406 (Not Acceptable)
        ),
        query = list(
          # Nomes dos parâmetros exatamente como no Swagger da ANA (com espaços e acentos)
          `Código da Estação`         = as.character(codigo_estacao),
          `Tipo Filtro Data`          = "DATA_LEITURA",         # Filtra pela data de leitura do sensor
          `Data Inicial (yyyy-MM-dd)` = format(data_inicio, "%Y-%m-%d"),
          `Data Final (yyyy-MM-dd)`   = format(data_fim,    "%Y-%m-%d")
        ),
        timeout(60)),
    error = function(e) NULL
  )

  # Retorna NULL silenciosamente em caso de erro HTTP (ex: janela sem dados)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)

  # Parse do JSON e extração do array 'items' com os registros
  corpo <- tryCatch(
    fromJSON(content(resp, as = "text", encoding = "UTF-8")),
    error = function(e) NULL
  )

  if (is.null(corpo) || is.null(corpo$items) || length(corpo$items) == 0) return(NULL)

  as.data.frame(corpo$items)
}

# -------------------------------------------------------------------------
# buscar_serie_completa()
#
# Orquestra múltiplas chamadas a buscar_cotas_janela() para cobrir todo o
# período histórico disponível da estação, contornando o limite de 366 dias
# por requisição da API.
#
# Fluxo:
#   1. Consulta o inventário da estação para descobrir a data de início real
#      dos dados (evita janelas vazias no começo da série)
#   2. Gera uma sequência de janelas anuais entre data_inicio e data_fim
#   3. Chama buscar_cotas_janela() para cada janela com atualização do progress bar
#   4. Concatena todos os resultados em um único data.frame
#
# Parâmetros:
#   token           — token Bearer obtido via obter_token()
#   codigo_estacao  — código ANA da estação (ex: "83900000")
#   data_inicio     — data inicial padrão (será ajustada pelo inventário)
#   data_fim        — data final (padrão: hoje)
#   progress        — lista com função $set(value, message) para o progress bar do Shiny
# -------------------------------------------------------------------------
buscar_serie_completa <- function(token, codigo_estacao,
                                  data_inicio = as.Date("1900-01-01"),
                                  data_fim    = Sys.Date(),
                                  progress    = NULL) {

  # --- Passo 1: Inventário ---
  # Consulta os metadados da estação para obter a data real de início da série,
  # evitando fazer chamadas desnecessárias para períodos sem dados.
  resp_inv <- tryCatch(
    GET(URL_INVENTARIO,
        add_headers(
          Authorization = paste("Bearer", token),
          Accept        = "application/json"
        ),
        query = list(`Código da Estação` = as.character(codigo_estacao)),
        timeout(30)),
    error = function(e) NULL
  )

  if (!is.null(resp_inv) && status_code(resp_inv) == 200) {
    inv <- tryCatch(
      fromJSON(content(resp_inv, as = "text", encoding = "UTF-8")),
      error = function(e) NULL
    )
    if (!is.null(inv$items) && length(inv$items) > 0) {
      # Campo que indica o início da série de descarga líquida (cotas/vazões)
      ini_str <- inv$items$Data_Periodo_Desc_liquida_Inicio[1]
      if (!is.null(ini_str) && !is.na(ini_str) && ini_str != "") {
        data_inicio_inv <- as.Date(substr(ini_str, 1, 10))
        # Usa a maior data entre o padrão (1900) e o que o inventário informa
        if (!is.na(data_inicio_inv)) data_inicio <- max(data_inicio, data_inicio_inv)
      }
    }
  }

  # --- Passo 2: Janelas anuais ---
  # Divide o período total em intervalos de 1 ano para respeitar o limite da API.
  # Ex: 2000-01-01 a 2003-06-30 vira 4 janelas:
  #   [2000-01-01, 2000-12-31], [2001-01-01, 2001-12-31],
  #   [2002-01-01, 2002-12-31], [2003-01-01, 2003-06-30]
  janelas_inicio <- seq(data_inicio, data_fim, by = "year")
  janelas_fim    <- pmin(janelas_inicio + years(1) - days(1), data_fim)

  resultados <- list()
  n          <- length(janelas_inicio)

  # --- Passo 3: Loop de requisições ---
  for (i in seq_len(n)) {

    # Atualiza o progress bar do Shiny a cada janela processada
    if (!is.null(progress)) {
      progress$set(
        value   = i / n,
        message = paste0("Buscando dados: ", format(janelas_inicio[i], "%Y"),
                         " (", i, "/", n, ")")
      )
    }

    # Avisa se o token está prestes a expirar durante um loop longo.
    # Séries de décadas podem demorar mais de 55 min para baixar completamente.
    if (!is.null(token_cache$emitido_em) &&
        difftime(Sys.time(), token_cache$emitido_em, units = "mins") >= 55) {
      warning("Token próximo do vencimento. Considere renovar as credenciais.")
    }

    df <- buscar_cotas_janela(token, codigo_estacao, janelas_inicio[i], janelas_fim[i])
    if (!is.null(df) && nrow(df) > 0) resultados[[i]] <- df
  }

  if (length(resultados) == 0) return(NULL)

  # Empilha todos os data.frames das janelas em um único data.frame
  bind_rows(resultados)
}

# -------------------------------------------------------------------------
# processar_serie()
#
# Recebe o data.frame bruto da API (registros horários ou subdiários) e
# gera três saídas:
#   - bruta:   dados originais com data/hora e cota, sem agregação
#   - diaria:  média diária das leituras de cota
#   - mensal:  média mensal calculada a partir da série diária
#
# A detecção das colunas de data e cota usa grep() para ser tolerante a
# variações nos nomes de campos retornados pela API entre versões.
# -------------------------------------------------------------------------
processar_serie <- function(df_raw) {

  # Normaliza os nomes de colunas para minúsculas (evita variações de case)
  nomes         <- tolower(names(df_raw))
  names(df_raw) <- nomes

  # Detecta a coluna de data/hora (ex: "data_hora_medicao", "datahora")
  col_data <- grep("data.*hora|data.*medicao|datahora", nomes, value = TRUE)[1]
  # Detecta a coluna de cota adotada (ex: "cota_adotada")
  col_cota <- grep("cota.*adotada|cota_adotada|media", nomes, value = TRUE)[1]

  # Fallback: se os padrões não baterem, usa a primeira coluna com formato
  # de data e a primeira coluna numérica encontradas
  if (is.na(col_data) || is.na(col_cota)) {
    col_data <- nomes[sapply(df_raw, function(x) any(grepl("-|/", x[!is.na(x)][1])))][1]
    col_cota <- nomes[sapply(df_raw, is.numeric)][1]
  }

  # Renomeia colunas detectadas e converte tipos
  df <- df_raw %>%
    rename(data = all_of(col_data), cota = all_of(col_cota)) %>%
    mutate(
      # Trunca o timestamp para 19 chars (yyyy-mm-dd HH:MM:SS) antes de converter,
      # pois a API pode retornar milissegundos ou sufixo de fuso horário
      data      = as.Date(data),                       # Extrai só a data para agregações
      cota      = suppressWarnings(as.numeric(cota))        # API retorna cota como string
    ) %>%
    filter(!is.na(data), !is.na(cota)) %>% # Remove registros sem data ou cota válidos
    distinct(data, .keep_all = TRUE) # Mantém a primeira ocorrência de cada data
  
  # Série mensal: agrupa por mês (primeiro dia do mês via floor_date) e
  # calcula a média das médias diárias
  serie_mensal <- df %>%
    mutate(mes = floor_date(data, "month")) %>%  # Arredonda para o 1º do mês
    group_by(mes) %>%
    summarise(cota_media_mensal = mean(cota, na.rm = TRUE), .groups = "drop") %>%
    rename(data = mes) %>%
    arrange(data)
 
  View(serie_mensal)
  
   list(mensal = serie_mensal, bruta = df)
}

# =========================================================================
# UI
# =========================================================================
ui <- navbarPage(
  "Série Histórica de Cotas - HidroWebService",
  id    = "inTabset",
  theme = shinytheme("flatly"),

  # -----------------------------------------------------------------------
  # Aba 1: Mapa das estações
  # O usuário seleciona a estação clicando no mapa ou digitando o código.
  # As credenciais da ANA também são inseridas aqui antes de buscar dados.
  # -----------------------------------------------------------------------
  tabPanel("Mapa das Estações",
    div(class = "outer",
      includeCSS("resources/style.css"),
      leafletOutput("mapahidroweb", width = "100%", height = "100%"),

      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 80, left = "auto", right = 20, bottom = "auto",
        width = 360, height = "auto",

        # h4("Autenticação HidroWebService"),
        # # CPF ou CNPJ cadastrado na ANA (enviado como header 'Identificador')
        # textInput("identificador", "Identificador (CPF/CNPJ):", ""),
        # # Senha recebida por e-mail após aprovação do cadastro na ANA
        # passwordInput("senha", "Senha:", ""),
        # hr(),
        h4("Selecione a estação no mapa ou digite o código"),
        textInput("codEstacao",  "Código:", ""),
        textInput("nomeEstacao", "Estação:", ""),
        actionButton("serie_historica", "Gerar Série Histórica",
                     icon = icon("chart-area"), class = "btn-primary btn-block")
      ),

      tags$div(id = "cite",
        "Dados de ", tags$em("HidroWebService: Agência Nacional de Águas e Saneamento Básico (ANA)")
      )
    )
  ),

  # -----------------------------------------------------------------------
  # Aba 2: Gráficos
  # Exibe a série mensal completa com seletor de intervalo (dyRangeSelector).
  # O usuário ajusta o período no gráfico mensal e então clica para gerar
  # a série diária filtrada para aquele intervalo (sem nova chamada à API).
  # -----------------------------------------------------------------------
  tabPanel(value = "panel2", "Gráficos",
    fluidRow(align = "center",
      uiOutput("tituloNomeEstacao"),
      # Mostra o intervalo atualmente selecionado no gráfico mensal
      div(
        strong("De: "), textOutput("from", inline = TRUE),
        strong(" Até: "), textOutput("to", inline = TRUE)
      ),
      br(),
      # Gráfico da série mensal — renderizado após o download completo
      withSpinner(dygraphOutput("dygraph_serie_historica")),
      # br(),
      # # Filtra e exibe a série diária para o intervalo selecionado acima
      # actionButton("gerar_diaria", "Ver Série Diária no Período Selecionado",
      #              icon = icon("chart-area"), class = "btn-info"),
      # br(), br(),
      # withSpinner(dygraphOutput("dygraph_serie_historica_diaria")),
      br(),
      actionButton("preparar_download", "Preparar Download",
                   icon = icon("file-export"), class = "btn-success")
    )
  ),

  # -----------------------------------------------------------------------
  # Aba 3: Download
  # Disponibiliza três arquivos CSV:
  #   - Mensal:  média mensal de cota (colunas: mes, cota_media)
  #   - Diária:  média diária de cota (colunas: data, cota_media)
  #   - Bruta:   todos os registros originais da API, sem agregação
  # -----------------------------------------------------------------------
  tabPanel(value = "panel3", "Download",
    fluidRow(align = "center",
      h1("Dados prontos para download"),
      br(),
      downloadButton("downloadSerieMensal",  "Série Mensal (.csv)",  icon = icon("download")),
      # downloadButton("downloadSerieDiaria",  "Série Diária (.csv)",  icon = icon("download")),
      downloadButton("downloadSerieCompleta","Série Bruta (.csv)",   icon = icon("download")),
      br(), br(),
      tags$div(id = "cite",
        "Dados de ", tags$em("HidroWebService - ANA")
      )
    )
  )
)

# =========================================================================
# Server
# =========================================================================
server <- function(input, output, session) {

  # Carrega a rede hidrometeorológica nacional do GeoJSON local.
  # Filtra apenas estações com registro de nível (registnive == "Sim"),
  # que são as fluviométricas com dados de cota disponíveis.
  # rede <- read_sf("resources/rede_hidrometeorologica_nacional.geojson") %>%
  #   filter(registnive == "Sim")
  rede <- read_sf("resources/rede_hidrometeorologica_nacional.geojson") 

  # reactiveValues armazena o estado da sessão entre eventos do Shiny.
  # Substitui as variáveis globais (<<-) do código original, que causavam
  # comportamento imprevisível com múltiplos usuários simultâneos.
  rv <- reactiveValues(
    serie_mensal  = NULL,
    serie_diaria  = NULL,
    serie_bruta   = NULL,
    nome_estacao  = NULL
  )

  # -----------------------------------------------------------------------
  # Mapa base com marcadores das estações.
  # Azul escuro = fluviométrica | vermelho = pluviométrica
  # O layerId de cada marcador é o código da estação, capturado no clique.
  # -----------------------------------------------------------------------
  output$mapahidroweb <- renderLeaflet({
    leaflet() %>%
      addTiles(options = providerTileOptions(
        updateWhenZooming = FALSE,  # Não recarrega tiles durante o zoom
        updateWhenIdle    = TRUE    # Só carrega tiles quando o mapa para de mover
      )) %>%
      addCircleMarkers(
        data        = rede,
        radius      = 6,
        stroke      = FALSE,
        fillOpacity = 0.7,
        layerId     = ~Codigo,  # ID do marcador = código da estação
        popup       = rede$Estacao
      )
  })

  # Ao clicar em um marcador do mapa, preenche automaticamente os campos
  # de código e nome da estação no painel lateral.
  observe({
    ev   <- input$mapahidroweb_marker_click
    req(ev)
    nome <- rede$Estacao[rede$Codigo == ev$id]
    updateTextInput(session, "codEstacao",  value = ev$id)
    updateTextInput(session, "nomeEstacao", value = nome)
  })

  # -----------------------------------------------------------------------
  # Evento principal: botão "Gerar Série Histórica"
  #
  # Fluxo:
  #   1. Valida credenciais e código da estação
  #   2. Autentica na API e obtém token Bearer
  #   3. Baixa a série completa em janelas anuais (com progress bar)
  #   4. Processa os dados brutos em séries diária e mensal
  #   5. Renderiza o gráfico mensal e navega para a aba de gráficos
  # -----------------------------------------------------------------------
  observeEvent(input$serie_historica, {

    req(input$codEstacao)
    
    # # Garante que as credenciais foram preenchidas antes de tentar autenticar
    # if (input$identificador == "" || input$senha == "") {
    #   shinyalert(
    #     title = "Credenciais necessárias",
    #     text  = "Preencha seu Identificador (CPF/CNPJ) e Senha da ANA para acessar o HidroWebService.",
    #     type  = "warning"
    #   )
    #   return()
    # }

    # withProgress exibe uma barra de progresso durante operações longas
    withProgress(message = "Conectando à API...", value = 0, {

      # --- 1. Autenticação ---
      token <- tryCatch(
        obter_token("40954711823", "jir5ys5s"),
        error = function(e) {
          shinyalert(title = "Erro de autenticação", text = e$message, type = "error")
          NULL
        }
      )
      req(token)  # Interrompe a execução se o token for NULL (erro já exibido)

      # --- 2. Download da série completa ---
      # O progress callback repassa o andamento das janelas anuais para o
      # withProgress do Shiny, escalando entre 10% e 95% da barra total.
      setProgress(value = 0.05, message = "Buscando série histórica...")

      df_raw <- tryCatch(
        buscar_serie_completa(token, input$codEstacao, progress = list(
          set = function(value, message) setProgress(value = value * 0.85 + 0.1, message = message)
        )),
        error = function(e) {
          shinyalert(title = "Erro na consulta", text = e$message, type = "error")
          NULL
        }
      )

      if (is.null(df_raw) || nrow(df_raw) == 0) {
        shinyalert(
          title = "Sem dados",
          text  = "Não foram encontrados dados de cota para esta estação. Verifique o código ou consulte o site da ANA.",
          type  = "warning"
        )
        return()
      }

      # --- 3. Processamento das séries ---
      setProgress(value = 0.97, message = "Processando séries...")
      series <- processar_serie(df_raw)

      # Salva no estado reativo para uso pelos outros eventos (gráfico diário, downloads)
      rv$serie_mensal  <- series$mensal
      # rv$serie_diaria  <- series$diaria
      rv$serie_bruta   <- series$bruta
      rv$nome_estacao  <- rede$Estacao[rede$Codigo == input$codEstacao]
      # Fallback para o código caso a estação não esteja no GeoJSON local
      if (length(rv$nome_estacao) == 0) rv$nome_estacao <- paste("Estação", input$codEstacao)
    })

    # --- 4. Renderização do gráfico mensal ---
    output$tituloNomeEstacao <- renderUI({ h2(rv$nome_estacao) })

    output$dygraph_serie_historica <- renderDygraph({
      req(rv$serie_mensal)
      # Converte para objeto xts (formato esperado pelo dygraphs)
      ts_mensal <- xts::xts(rv$serie_mensal$cota_media_mensal,
                            order.by = rv$serie_mensal$data)
      names(ts_mensal) <- "Cota Média Mensal (cm)"
      dygraph(ts_mensal, main = "Série Mensal de Cota") %>%
        dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha = 0.15,
                  drawGrid = FALSE, colors = "#1a5276") %>%
        dyRangeSelector() %>%           # Seletor de intervalo na base do gráfico
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 4, hideOnMouseOut = FALSE) %>%
        dyRoller(rollPeriod = 1) %>%    # Média móvel ajustável pelo usuário
        dyAxis("y", label = "Cota (cm)")
    })

    # Textos "De: ... Até: ..." sincronizados com o range selecionado no gráfico
    output$from <- renderText({
      strftime(req(input$dygraph_serie_historica_date_window[[1]]), "%d %b %Y")
    })
    output$to <- renderText({
      strftime(req(input$dygraph_serie_historica_date_window[[2]]), "%d %b %Y")
    })

    updateTabsetPanel(session, "inTabset", selected = "panel2")
  })

  # -----------------------------------------------------------------------
  # Evento: botão "Ver Série Diária no Período Selecionado"
  #
  # Filtra a série diária completa (já em memória em rv$serie_diaria) pelo
  # intervalo de datas selecionado no dyRangeSelector do gráfico mensal.
  # Não realiza nenhuma nova chamada à API.
  # -----------------------------------------------------------------------
  # observeEvent(input$gerar_diaria, {
  #   req(rv$serie_diaria)
  # 
  #   # Lê o intervalo atualmente selecionado no gráfico mensal
  #   data_ini <- as.Date(input$dygraph_serie_historica_date_window[[1]])
  #   data_fim <- as.Date(input$dygraph_serie_historica_date_window[[2]])
  # 
  #   diaria_filtrada <- rv$serie_diaria %>%
  #     filter(data >= data_ini, data <= data_fim)
  # 
  #   if (nrow(diaria_filtrada) == 0) {
  #     shinyalert(title = "Sem dados", text = "Nenhum dado diário no período selecionado.", type = "info")
  #     return()
  #   }
  # 
  #   output$dygraph_serie_historica_diaria <- renderDygraph({
  #     ts_diaria <- xts::xts(diaria_filtrada$cota_media_diaria,
  #                           order.by = diaria_filtrada$data)
  #     names(ts_diaria) <- "Cota Média Diária (cm)"
  #     dygraph(ts_diaria, main = "Série Diária de Cota") %>%
  #       dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha = 0.15,
  #                 drawGrid = FALSE, colors = "#D8AE5A") %>%
  #       dyCrosshair(direction = "vertical") %>%
  #       dyHighlight(highlightCircleSize = 4, hideOnMouseOut = FALSE) %>%
  #       dyAxis("y", label = "Cota (cm)")
  #   })
  # })

  # -----------------------------------------------------------------------
  # Evento: botão "Preparar Download"
  #
  # Registra os três downloadHandlers com os dados já processados em memória.
  # Os arquivos CSV só são gerados no momento em que o usuário clica no
  # botão de download correspondente (comportamento padrão do downloadHandler).
  #
  # Formatos de saída:
  #   - Mensal:  colunas mes (mm/yyyy) e cota_media
  #   - Diária:  colunas data (dd/mm/yyyy) e cota_media
  #   - Bruta:   todas as colunas originais retornadas pela API, sem agregação
  # -----------------------------------------------------------------------
  observeEvent(input$preparar_download, {
    req(rv$serie_mensal, rv$serie_bruta)

    output$downloadSerieMensal <- downloadHandler(
      filename = function() paste0(input$codEstacao, "_mensal.csv"),
      content  = function(file) {
        write.csv2(
          transmute(rv$serie_mensal,
                    mes        = format(data, "%m/%Y"),
                    cota_media = round(cota_media_mensal, 2)),
          file, row.names = FALSE
        )
      }
    )


    # Série bruta: exporta os dados originais da API sem nenhuma agregação
    output$downloadSerieCompleta <- downloadHandler(
      filename = function() paste0(input$codEstacao, "_bruta.csv"),
      content  = function(file) {
        write.csv2(rv$serie_bruta, file, row.names = FALSE)
      }
    )

    updateTabsetPanel(session, "inTabset", selected = "panel3")
  })
}

shinyApp(ui = ui, server = server)
