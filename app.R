#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(httr)
library(data.table)
library(dplyr)
library(lubridate)
library(datasets)
library(leaflet)
library(sf)
library(rmapshaper)
library(shinythemes)
library(dygraphs)
library(rstudioapi)
library(shinyalert)

# Define UI for application that draws a histogram
ui <- navbarPage("Dados da cota do rio da HIDROWEB v3.2.0", id="inTabset", theme = shinytheme("flatly"),
        tabPanel("Mapa das Estações",
            div(class="outer",
                includeCSS("resources/style.css"),
                leafletOutput("mapahidroweb", width="100%", height="100%"),
                # Shiny versions prior to 0.11 should use class = "modal" instead.
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = FALSE, top = 80, left = "auto", right = 20, bottom = "auto",
                        width = 330, height = "auto",
                                
                    h3("Selecione a estação no mapa, ou digite o código"),
                    textInput("codEstacao", "Código:", ""),
                    textInput("nomeEstacao", "Estação:", ""),
                    actionButton("serie_historica", "Gerar Série Histórica", icon=icon("chart-area"))
                                
                ),
                tags$div(id="cite",
                    'Dados compilador de ', 
                    tags$em('HIDROWEB v3.2.6: Agência Nacional das Águas'), 
                    ' por Renan Cassimiro Brito, 2022.'
                )
            )# mainPanel
        ), # Navbar 1, tabPanel
        
        tabPanel(value="panel2", "Gráficos",
            fluidRow(align = "center",
                uiOutput("tituloNomeEstacao"),
                div(strong("De: "), textOutput("from", inline = TRUE), strong(" Até: "), textOutput("to", inline = TRUE)),
                shinycssloaders::withSpinner(
                    dygraphOutput("dygraph_serie_historica")
                ),
                br(),
                
                actionButton("serie_historica_diaria", "Gerar Série Histórica Diária", icon=icon("chart-area")),
                br(),br(),
                
                shinycssloaders::withSpinner(
                    dygraphOutput("dygraph_serie_historica_diaria")
                ),
                br(),
                
                actionButton("preparar_download", "Preparar Download", icon=icon("file-export"))
            )
        ),
        
        tabPanel(value="panel3", "Download",
            fluidRow(align = "center", 
                h1("Obrigado!"),
                h2("Seus arquivos estão prontos para download..."),
                br(),
                br(),
                downloadButton("downloadSerieMensal", "Baixar Série Mensal", icon=icon("download")),
                downloadButton("downloadSerieDiaria", "Baixar Série Diária", icon=icon("download"))
            ),
            fluidRow(align = "center", 
                 tags$div(id="cite",
                          'Dados compilador de ', 
                          tags$em('HIDROWEB v3.2.6: Agência Nacional das Águas'), 
                          ' por Renan Cassimiro Brito, 2022.'
                 )
            )
        )
    ) # navbarPage

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    rede_hidrometeorologica_nacional <- read_sf("resources/rede_hidrometeorologica_nacional.geojson")    
    rede_hidrometeorologica_nacional <- filter(rede_hidrometeorologica_nacional, registnive == 'Sim')
    tabela_original <- NULL
    serie_mensal_download <- NULL
    serie_diaria_download <- NULL
    
    output$mapahidroweb <- renderLeaflet({
        leaflet() %>%
            addTiles(options = providerTileOptions(
                updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                updateWhenIdle = TRUE,           # map won't load new tiles when panning
                leafletOptions(preferCanvas = TRUE)
            ))  %>%
            addCircleMarkers(
                data = rede_hidrometeorologica_nacional,
                radius = 6, stroke = FALSE,
                color = ~ifelse(Tipo == "Fluviométrica", "#053061", "red"),
                fillOpacity = 0.7,
                layerId = ~Codigo,
                popup = rede_hidrometeorologica_nacional$Estacao)
    })
    
    observeEvent(input$serie_historica, {
        output$dygraph_serie_historica <- NULL
        output$dygraph_serie_historica_diaria <- NULL
        download_folder = "download"
        
        if (!file.exists(download_folder)){
            dir.create(download_folder)            
        }
        
        setwd(download_folder)
        
        estacao = input$codEstacao
        arquivo_estacao = paste0(estacao, ".zip")
        data_arquivo_estacao = as.Date(as.POSIXct(file.mtime(arquivo_estacao), 'GMT'))
        
        if(!file.exists(arquivo_estacao) || Sys.Date() != data_arquivo_estacao){
            baseurl = "http://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=&documentos="
            
            # tipo=3 arquivo excel  *.csv
            tipo = 3
            
            #substituindo o tipo
            baseurl = gsub("tipo=",paste0("tipo=",tipo),baseurl)
            baseurl_est = paste0(baseurl,estacao[1])
            
            # #Conexao
            r = POST(url = baseurl_est, body = list(cboTipoReg = "8"), encode = "form")
            if (r$status_code == 405) {
                cont = content(r, as = "text", encoding="ISO-8859-1")
                download.file(baseurl_est, paste0(estacao[1], ".zip"), mode = "wb")
            }
        }
        
        tryCatch({
            unzip(paste0(estacao, ".zip"))
            zip_estacao <- paste0("cotas_C_", estacao, ".zip")
            
            if(file.exists(zip_estacao)){
                unzip(zip_estacao)
                
                tabela_original <<-
                    read.csv2(paste0("cotas_C_", estacao, ".csv"), header=FALSE, sep = ";", skip = 14, skipNul=TRUE)
                
                updateTabsetPanel(session, "inTabset", selected = "panel2")
                
                serie_mensal <- setDT(tabela_original)[, .(
                    data = dmy(V3),
                    # maxima_mensal = V7,
                    # minima_mensal = V8,
                    media_mensal = V9
                )]
                
                serie_mensal <- na.omit(distinct(serie_mensal, data, .keep_all= TRUE))
                serie_mensal_download <<- transmute(serie_mensal, 
                                                 mes=format(data, "%m/%y"),
                                                 cota_media=media_mensal)
                
                output$dygraph_serie_historica <- renderDygraph({
                    dygraph(serie_mensal)%>%
                        dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
                        dyRangeSelector() %>%
                        dyCrosshair(direction = "vertical") %>%
                        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
                        dyRoller(rollPeriod = 1)
                })
                
                nomeestacao <- rede_hidrometeorologica_nacional$Estacao[rede_hidrometeorologica_nacional$Codigo == estacao]
                
                output$tituloNomeEstacao <- renderUI({
                    h2(nomeestacao)
                })
                
            }
            
            output$from <- renderText({
                strftime(req(input$dygraph_serie_historica_date_window[[1]]), "%d %b %Y")
            })
            
            output$to <- renderText({
                strftime(req(input$dygraph_serie_historica_date_window[[2]]), "%d %b %Y")
            })
        }, warning = function(w){
            print(w)
            shinyalert(title = "Erro!", text = "Não foram encontrados dados de nível do rio para esta estação. Consulte o site oficial da Hidroweb.", type = "error")
        })
        
        setwd('../')
        unlink("download",recursive=TRUE)
    
    })
    
    observe({
        
        event <- input$mapahidroweb_marker_click
        
        nomeestacao <- rede_hidrometeorologica_nacional$Estacao[rede_hidrometeorologica_nacional$Codigo == event$id]
        
        updateTextInput(session, "codEstacao", value=event$id)
        updateTextInput(session, "nomeEstacao", value=nomeestacao)
        
        
    })
    
    observeEvent(input$serie_historica_diaria, {
        
        data_inicio <- as.Date(input$dygraph_serie_historica_date_window[[1]])
        data_fim <- as.Date(input$dygraph_serie_historica_date_window[[2]])

        serie_diaria <- distinct(tabela_original, V3, .keep_all= TRUE)
        serie_diaria <- mutate(serie_diaria,
                               V3=dmy(V3))
        
        serie_diaria <- subset(serie_diaria, V3 > data_inicio & V3 < data_fim)
        serie_diaria_concatenada <- data.frame(matrix(ncol = 2, nrow=0))
        
        for(i in 1:nrow(serie_diaria)) {
            linha <- data.frame(serie_diaria[i, c(17:47)])
            for(j in 1:ncol(linha)){
                data <- serie_diaria[i, 3]
                data <- as.Date(paste0(j, "/", format(data, "%m/%y")), format='%d/%m/%y')
                cota_dia <- data.frame(data = data, cota = linha[,j])
                serie_diaria_concatenada <- rbind(serie_diaria_concatenada, cota_dia)
            }
        }
        
        
        serie_diaria_concatenada <- na.omit(serie_diaria_concatenada)
        serie_diaria_download <<- as.data.frame(serie_diaria_concatenada)%>%
                                    transmute(data=format(data, "%d/%m/%y"),
                                            cota_media_diaria=cota)
        
        serie_diaria_concatenada <- xts::xts(serie_diaria_concatenada[, -1], order.by=as.Date(serie_diaria_concatenada$data))
        
        output$dygraph_serie_historica_diaria <- renderDygraph({
            dygraph(serie_diaria_concatenada)%>%
                dyOptions(labelsUTC = TRUE, disableZoom=TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
                dyCrosshair(direction = "vertical") %>%
                dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  
        })
    
    })
    
    
    observeEvent(input$preparar_download, {
        
        output$downloadSerieMensal <- downloadHandler(
            filename = function() {
                paste(input$codEstacao, "_mensal.csv", sep="")
            },
            content = function(file) {
                write.csv(serie_mensal_download, file, row.names = FALSE)
            }
        )
        
        output$downloadSerieDiaria <- downloadHandler(
            filename = function() {
                paste(input$codEstacao, "_diaria.csv", sep="")
            },
            content = function(file) {
                write.csv(serie_diaria_download, file, row.names = FALSE)
            }
        )
        updateTabsetPanel(session, "inTabset", selected = "panel3")
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
