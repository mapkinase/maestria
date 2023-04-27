# Importar librerías ----


## Instalar los paquetes necesarios para correr la palciación 

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
# if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
# if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
# if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("pzfx", quietly = TRUE)) install.packages("pzfx")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("ggthemes", quietly = TRUE)) install.packages("ggthemes")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")

## Importar las librerias 

library(shiny) # Web application framework
# library(ggplot2)
# library(dplyr)
library(shinythemes) # tema para la aplicación de shiny
library(plotly)
library(tidyverse) # hacer gráficas interactivas
# library(tidyr)
library(openxlsx) # crear archivos para Escel
library(pzfx) # Crear archivos para prism
library(stringr) # make working with strings as easy as possible
library(ggthemes)
library(scales)
library(hrbrthemes)
library(ggdark)

# UI ----

ui <- fluidPage(
  ## Tema  ----
  theme = shinytheme("cyborg"),
  ## CSS y JavaScript ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), 
    # tags$script(src = "script.js")
  ),
  ## title panel ----
  titlePanel(
    ### Logo ----
    tags$img(src = "logo.png", height = "220px", alt = "Logo"),
    "Water maze app"
  ),
  ## Layout ----
  ### sidebarLayout ----
  sidebarLayout(
    sidebarPanel(
      #### subir arcivos raw ---- 
      fileInput("file", "Subir datos de experimentos de AnyMaze", buttonLabel = "Buscar archivos"),
      downloadButton("download", "Descargar archivos modificados"),
      br(),
      br(),
      #### Merger ----
      textInput("folder", "Seleccionar carpeta con datos modiificados para unirlos en un archivo único"),
      actionButton("loadFiles", "Cargar archivos"),
      downloadButton("downloadFile", "descargar archivos unidos", enabled = FALSE),
      br(),
      br(),
      #### análisis de datos ----
      fileInput("analysisFile", "Seleccionar CVS con datos para analizar", accept = c(".csv"))
    ),
    ### mainpanel ----
    mainPanel(
      #### heading archivos raw ----
      tags$h3("Datos RAW"),
      ##### dataframe prcoesado ----
      tableOutput("table"),
      #### Tabla merged -----
      tags$h3("Merged Data"),
      tableOutput("mergedTable"),
      #### Analisis de datos ----
      tabsetPanel(
        id = "main_tab",
        tabPanel("Analisis de datos",
                 plotlyOutput("entr_lat_plot"),
                 downloadButton("entr_lat_csv", "Download CSV"),
                 downloadButton("entr_lat_xlsx", "Download XLSX"),
                 downloadButton("entr_lat_pzfx", "Download PZFX"))
      ),
      tabPanel("Cudrante Blanco",
               plotlyOutput("cudrante_blanco_plot")
    )
  )
))

# Server ----
server <- function(input, output, session) {
  ## renombrar archivos raw ----
  data <- reactive({
    if (is.null(input$file)) return(NULL)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    df %>% rename(
      # tratamiento = Tratamiento, 
      latencia = Duration,
      distancia = Distance,
      velocidad = Mean.speed,
      # eficiencia = Path.efficiency,
      TiempoSE = SE.cuadrante...time,
      TiempoSO = SWcuadrante...time,
      TiempoNO = NW.Cuadrante...time,
      TiempoNE = NE.cuadrante...time,
      EntradasNE = Annulus.NE...entries,
      PromedioNE = Annulus.NE...mean.distance.from,
      EficienciaNE = Annulus.NE...path.efficiency.to.entry,
      EntradasSO = Annulus.SW...entries,
      PromedioSO = Annulus.SW...mean.distance.from,
      EficianciaSO = Annulus.SW...path.efficiency.to.entry 
    )
  })
  
  ### Tabla con datos renombrados ----

  output$table <- renderTable({
    req(data())
    data()
  })
  
  ### Descargar datos modificados ----
  output$download <- downloadHandler(
    filename = function() {
      paste("modified_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  
  ## Merged data ----
  mergedDF <- reactiveVal()
  
  ### Seleccionar columnas ----  
  selectedColumns <- c("Test", 
                       "Animal", 
                       "tratamiento", 
                       "Stage", 
                       "Trial", 
                       "Date", 
                       "Time", 
                       "Sexo",
                       "latencia", 
                       "distancia", 
                       "velocidad", 
                       "TiempoSE", 
                       "TiempoSO", 
                       "TiempoNO", 
                       "TiempoNE", 
                       "EntradasNE", 
                       "PromedioNE", 
                       "EficienciaNE", 
                       "EntradasSO", 
                       "PromedioSO", 
                       "EficianciaSO",
                       "peso",
                       "mouseline",
                       "CUMS",
                       "experimento")
  
  ### función (?) que mergea datos ----
  observeEvent(input$loadFiles, {
    if (dir.exists(input$folder)) {
      fileNames <- list.files(input$folder, pattern = "*.csv", full.names = TRUE)
      
      if (length(fileNames) > 0) {
        dataFrames <- lapply(fileNames, function(file) {
          df <- read_csv(file)
          df <- df %>% select(all_of(intersect(colnames(df), selectedColumns)))
          df
        })
        
        mergedData <- bind_rows(dataFrames)
        mergedDF(mergedData)
      } else {
        mergedDF(NULL)
      }
      ### descargar merged data ----
      output$downloadFile <- downloadHandler(
        filename = "merged_data.csv",
        content = function(file) {
          write_csv(mergedDF(), file)
        }
      )
      
      shinyjs::enable("downloadFile")
    } else {
      showNotification("Invalid directory path. Please enter a valid path.", type = "error")
    }
  })
  
  #### ver merged data ----
  output$mergedTable <- renderTable({
    req(mergedDF())
    mergedDF()
  })
  
  
  ## Análisis de datos ----
  df_analysis <- reactive({
    req(input$analysisFile)
    read.csv(input$analysisFile$datapath)
  })
  
  
  ### Pruebas ----
  #### Cuadrante blanco ----
  ##### mutar dataframe -----
  
  cuadrante_blanco <- reactive({
    req(df_analysis())
    df_analysis()%>%
      group_by(tratamiento, Stage) %>%
      filter(str_detect(Stage, "prueba")) %>% 
      select(tratamiento, 
             Stage,
             Animal,
             Sexo,
             Experimento,
             TiempoSE,
             TiempoSO,
             TiempoNO,
             TiempoNE) %>%
      mutate(
        cuadrante_blanco = case_when(
          Stage == "prueba_1" ~ (TiempoNE * 100) / 60,
          Stage == "prueba_2" ~ (TiempoNE * 100) / 60,
          Stage == "prueba_rev" ~ (TiempoSO * 100) / 60
        )
      )
  })
  
  ##### gráfico cuad blanco -----

  output$cudrante_blanco_plot <- renderPlotly({
    req(cuadrante_blanco())
    dodge_width <- 1
    plotly_cuadrante_blanco <- ggplot(cuadrante_blanco(), 
                                      aes(x = Stage, 
                                          y = cuadrante_blanco, 
                                          colour = tratamiento,
                                          shape = Sexo,
                                          # fill = tratamiento,
                                          group = tratamiento)) +
      
      # geom_line(aes(group = Animal),
      #           position = position_dodge(width = dodge_width)) +
      
      #geom_line(aes(group = tratamiento)) +
      
      stat_summary(fun.data = mean_se, 
                   geom = "errorbar",
                   width = 0.4) +
      
      stat_summary(geom = "point", 
                   fun.y="mean",
                   shape = 18,
                   size = 2,
                   aes(text = "Media")) +
      
      stat_summary(geom = "line", 
                   fun.y="mean",
                   linewidth = 0.35,
                   alpha = 1,
                   linetype = 3) +
      
      geom_jitter(alpha = 0.3,
                  position = position_dodge(width = dodge_width),
                  aes(text = paste("Animal", Animal))) +
      # Dibujar línea en eje y
      geom_hline(yintercept=50,
                 linetype="dashed", 
                 color = "black", 
                 size= 0.1) + 
      
      facet_grid(~ Experimento) + # tratamiento ~ Experimento / wrap??
      # margins = TRUE
      # scales='free'
      # facet_grid(dose ~ supp, labeller=label_both)
    #'   # Change facet text font. Possible values for the font style:
    #'   #'plain', 'italic', 'bold', 'bold.italic'.
    #'   bp + facet_grid(dose ~ supp)+
    #'   theme(strip.text.x = element_text(size=12, color="red",
    #'                                     face="bold.italic"),
    #'         strip.text.y = element_text(size=12, color="red",
    #'                                     face="bold.italic"))
    #' # Change the apperance of the rectangle around facet label
    #' bp + facet_grid(dose ~ supp)+
    #'   theme(strip.background = element_rect(colour="black", fill="white", 
    #'                                         size=1.5, linetype="solid"))

      dark_theme_classic() +
      scale_colour_solarized() +
    # theme_classic() +
    # theme_few() +
    
      scale_shape_manual(values=c(3, 0, 10)) + # Formas diferentes a Shape
  
      guides(shape = FALSE)  # quitar este aes de la leyenda del gráfico
    
    
                        # scale_color_manual(values=c("#e7298a", "#1b9e77", "#66a61e", "#d95f02", "#7570b3"))
     
 

    
    ggplotly(plotly_cuadrante_blanco)
  })
        

    
  
  
  ### latencias ----
  entrneamientos_latencias <- reactive({
    req(df_analysis())
    df_analysis() %>%
      group_by(tratamiento, Stage) %>%
      filter(str_detect(Stage, "entrenamiento")) %>%
      filter(!str_detect(Stage, "pre")) %>%
      select(tratamiento,
             Stage,
             latencia,
             Test,
             Animal)
  })
  
  
  #### plot latencias ----
  output$entr_lat_plot <- renderPlotly({
    req(entrneamientos_latencias())
    p <- ggplot(entrneamientos_latencias(), aes(x = Stage, y = latencia, fill = tratamiento)) +
      geom_violin(trim = FALSE, position = position_dodge(1)) +
      geom_boxplot(position = position_dodge(1)) +
      stat_summary(fun.data = mean_se, geom = "pointrange", position = position_dodge(1), colour = "red") +
      geom_jitter(shape = 16, position = position_dodge(1), colour = "black", alpha = 0.9) +
      theme_classic()
    
    ggplotly(p)
  })
  
  
  #### Download buttons for CSV, XLSX, and PZFX files ----

  output$entr_lat_csv <- downloadHandler(
    filename = function() {"entrneamientos_latencias.csv"},
    content = function(file) {write.csv(entrneamientos_latencias(), file)}
  )
  
  output$entr_lat_xlsx <- downloadHandler(
    filename = function() {"entrneamientos_latencias.xlsx"},
    content = function(file) {write.xlsx(entrneamientos_latencias(), file)}
  )
  
  output$entr_lat_pzfx <- downloadHandler(
    filename = function() {"entrneamientos_latencias.pzfx"},
    content = function(file) {write_pzfx(entrneamientos_latencias(), file)}
  )
  
}


# Run the app ----
shinyApp(ui, server)
