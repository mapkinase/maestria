# Campo Abierto ----

library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyr)
library(openxlsx)
library(pzfx)
library(stringr)
library(shinydashboard)
library(viridis)
library(purrr)
library(readr)

# UI ----

# Define the UI
ui <- fluidPage(
  # Add a theme
  theme = shinytheme("superhero"),

  #Define the layout
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "sidebar_tab",
        tabPanel("Data Processing and Merging",
                 fileInput("file", "Upload the data file for processing", buttonLabel = "Browse..."),
                 downloadButton("download", "Download the modified data"),
                 br(),
                 br(),
                 textInput("folder", "Enter directory path:"),
                 actionButton("loadFiles", "Load Files"),
                 downloadButton("downloadFile", "Download Merged CSV", enabled = FALSE)
        ),
        tabPanel("Data Analysis",
                 fileInput("analysisFile", "Choose CSV file for analysis", accept = c(".csv"))
        ),
        tags$hr(),
        tabPanel("Archivos CVS para latencia",
                 fileInput("file_latencia", "Choose CSV file", accept = c(".csv")),
                 tags$hr()
        ),
        tags$hr(),
        tabPanel("Mapa de Calor",
                 id = "sidebar_tab",
                 tabPanel("Mapa de Calor",
                          textInput("folder_path", "Seleccionar carpeta con coordenadas"),
                          actionButton("load_data", "Subir Coordenadas")),
                 tags$hr()
        )
      )),
    mainPanel(
      tabsetPanel(
        id = "main_tab",
        tabPanel("Processed Data",
                 tags$h3("Processed Data"),
                 tableOutput("table")
        ),
        tabPanel("Merged Data",
                 tags$h3("Merged Data"),
                 tableOutput("mergedTable")
        ),
        tabPanel("Entrneamientos Latencias",
                 plotlyOutput("plot_distancia"),
                 downloadButton("entr_lat_csv", "Download CSV"),
                 downloadButton("entr_lat_xlsx", "Download XLSX"),
                 downloadButton("entr_lat_pzfx", "Download PZFX"),
                 plotlyOutput("plot_tiempo_periferia"),
                 plotlyOutput("plot_TiempoMedio"),
                 plotlyOutput("plot_TiempoCentro"),
                 plotlyOutput("plot_CrucesCentro"),
                 plotlyOutput("plotVelocidadPromedio")
        ),
        tabPanel("grafico_latencias", 
                 tags$h3("lATENCIA"),
                 plotlyOutput("grafico_lat_plot"), 
        ),
        tabPanel("Mapa de Calor",
                 tags$h3("Mapa de Calor"),
                 plotOutput("heatmap"))
      )
    )
  )
)





# Servidor ----

server <- function(input, output, session) {
  
  ## Renombrar Columnas ----
  
  
  # Cambiar nombre de columnas
  # por default, anymaze da nombres con espacios
  # tuve problema con esto, asi que le cambié el nombre por unos sin espacios
  data <- reactive({
    if (is.null(input$file)) return(NULL)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    df %>% rename(
      distancia = Distance,
      TiempoPeriferiferia = Periferia...time, 
      TiempoMedio = Medio...time,
      TiempoCentro = Centro...time,
      # CrucesCentro = Centro...Number.line.crossings,
      # TiempoInmobil = Time.mobile,
      VelocidadPromedio = Mean.speed
    )
  })
  
  output$table <- renderTable({
    req(data())
    data()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("modified_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  ## Merge Data ----
  
  mergedDF <- reactiveVal()
  
  # Seleccionamos solo las variables que nos van a interesar
  # en un futuro, podríamos agregar más variables, como peso, sexo
  selectedColumns <- c("Test", 
                       "Animal", 
                       "Tratamiento", 
                       "Stage", 
                       "Trial", 
                       "Date", 
                       "Time", 
                       "distancia", 
                       "TiempoPeriferiferia", 
                       "TiempoMedio", 
                       "TiempoCentro", 
                       "CrucesCentro", 
                       "TiempoInmobil", 
                       "VelocidadPromedio")
  
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
  
  output$mergedTable <- renderTable({
    req(mergedDF())
    mergedDF()
  })
  
  
  ## Analisis exploratorio de datos ----
  
  df_analysis <- reactive({
    req(input$analysisFile)
    read.csv(input$analysisFile$datapath)
  })
  
  
  ### distancia ---- 
  
  #### dataframe -----

  
  df_campo_abierto <- reactive({
    req(df_analysis())
    df_analysis() %>%
      group_by(Tratamiento, Stage) %>% 
      select(Tratamiento,
             Stage,
             Test, 
             Animal, 
             distancia, 
             TiempoPeriferiferia, 
             TiempoMedio, 
             TiempoCentro, 
             CrucesCentro, 
             TiempoInmobil, 
             VelocidadPromedio)
  })
  

  
  ##### Plot ----
  
  ##### plot distancia ----
  
  output$plot_distancia <- renderPlotly({
    req(df_campo_abierto())
    dodge_width <- 0.9  # dodge para conservar posiciones en todos los plots
    # boxplot_width <- 0.5
    p <- ggplot(df_campo_abierto(), aes(x = reorder(Stage, desc(Stage)), 
                                        y = distancia,
                                        group = Tratamiento,
                                        colour = Tratamiento)) +
      # Error estándar
      stat_summary(fun.data = mean_se,
                   geom = "errorbar") +
      stat_summary(geom = "point", 
                   fun.y="mean",
                   shape = 18,
                   size = 2,
                   aes(text = "Media")) +
      stat_summary(geom = "line", 
                   fun.y="mean",
                   size = 1) +
      # Puntos individuales
      # geom_jitter(shape = 1, 
      #             position = position_dodge(width = dodge_width),
      #             alpha = 0.5, 
      #             aes(colour = Sexo,
      #                 group = Tratamiento,
      #                 text = paste ("Número de prueba:", Test))) + # Funciona para que diga qué número de prueba es al seleccionar el dato
      # # geom_boxplot(outlier.shape = 4, 
      #              position = position_dodge(width = dodge_width),
      #              alpha = 0.5) +
      # Nombre de ejes en X
      # scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa')) +
      # Nombre de otros ejes
      # labs(
      #   title = "Latencias entrenamientos",
      #   x = "Número de entrenamiento",
      #   y = "Latencia de escape (s)") +
      # Tema
      theme_classic()
    
    ggplotly(p)%>%layout(boxmode = "group")
  })
  
  ###### plot timepo periferia ----
  
  output$plot_tiempo_periferia <- renderPlotly({
    req(df_campo_abierto())
    dodge_width <- 0.9  # dodge para conservar posiciones en todos los plots
    # boxplot_width <- 0.5
    p <- ggplot(df_campo_abierto(), aes(x = reorder(Stage, desc(Stage)), 
                                        y = TiempoPeriferiferia,
                                        group = Tratamiento,
                                        colour = Tratamiento)) +
      # Error estándar
      stat_summary(fun.data = mean_se,
                   geom = "errorbar") +
      stat_summary(geom = "point", 
                   fun.y="mean",
                   shape = 18,
                   size = 2,
                   aes(text = "Media")) +
      stat_summary(geom = "line", 
                   fun.y="mean",
                   size = 1) +
    theme_classic()
    

  })
  
  ###### plot TiempoMedio----
  
  output$plot_TiempoMedio <- renderPlotly({
    req(df_campo_abierto())
    dodge_width <- 0.9  # dodge para conservar posiciones en todos los plots
    # boxplot_width <- 0.5
    p <- ggplot(df_campo_abierto(), aes(x = reorder(Stage, desc(Stage)), 
                                        y = TiempoMedio,
                                        group = Tratamiento,
                                        colour = Tratamiento)) +
      # Error estándar
      stat_summary(fun.data = mean_se,
                   geom = "errorbar") +
      stat_summary(geom = "point", 
                   fun.y="mean",
                   shape = 18,
                   size = 2,
                   aes(text = "Media")) +
      stat_summary(geom = "line", 
                   fun.y="mean",
                   size = 1) +
      theme_classic()
    
    
  })
  
  
  ###### plot TiempoCentroa ----
  
  output$plot_TiempoCentro <- renderPlotly({
    req(df_campo_abierto())
    dodge_width <- 0.9  # dodge para conservar posiciones en todos los plots
    # boxplot_width <- 0.5
    p <- ggplot(df_campo_abierto(), aes(x = reorder(Stage, desc(Stage)), 
                                        y = TiempoCentro,
                                        group = Tratamiento,
                                        colour = Tratamiento)) +
      # Error estándar
      stat_summary(fun.data = mean_se,
                   geom = "errorbar") +
      stat_summary(geom = "point", 
                   fun.y="mean",
                   shape = 18,
                   size = 2,
                   aes(text = "Media")) +
      stat_summary(geom = "line", 
                   fun.y="mean",
                   size = 1) +
      theme_classic()
    
    
  })
  
  
  
  ###### plot CrucesCentro ----
  
  output$plot_CrucesCentro <- renderPlotly({
    req(df_campo_abierto())
    dodge_width <- 0.9  # dodge para conservar posiciones en todos los plots
    # boxplot_width <- 0.5
    p <- ggplot(df_campo_abierto(), aes(x = reorder(Stage, desc(Stage)), 
                                        y = CrucesCentro,
                                        group = Tratamiento,
                                        colour = Tratamiento)) +
      # Error estándar
      stat_summary(fun.data = mean_se,
                   geom = "errorbar") +
      stat_summary(geom = "point", 
                   fun.y="mean",
                   shape = 18,
                   size = 2,
                   aes(text = "Media")) +
      stat_summary(geom = "line", 
                   fun.y="mean",
                   size = 1) +
      theme_classic()
    
    
  })
  
  
  
  ###### plot VelocidadPromedio ----
  
  output$plotVelocidadPromedio <- renderPlotly({
    req(df_campo_abierto())
    dodge_width <- 0.9  # dodge para conservar posiciones en todos los plots
    # boxplot_width <- 0.5
    p <- ggplot(df_campo_abierto(), aes(x = reorder(Stage, desc(Stage)), 
                                        y = VelocidadPromedio,
                                        group = Tratamiento,
                                        colour = Tratamiento)) +
      # Error estándar
      stat_summary(fun.data = mean_se,
                   geom = "errorbar") +
      stat_summary(geom = "point", 
                   fun.y="mean",
                   shape = 18,
                   size = 2,
                   aes(text = "Media")) +
      stat_summary(geom = "line", 
                   fun.y="mean",
                   size = 1) +
      theme_classic()
    
    
  })
  
  

  
  ### Descargas ----
  
  # Download buttons for CSV, XLSX, and PZFX files
  output$entr_lat_csv <- downloadHandler(
    filename = function() {"df_campo_abierto.csv"},
    content = function(file) {write.csv(df_campo_abierto(), file)}
  )
  
  output$entr_lat_xlsx <- downloadHandler(
    filename = function() {"df_campo_abierto.xlsx"},
    content = function(file) {write.xlsx(entrenamientos_latencias_prisma(), file)}
  )
  
  output$entr_lat_pzfx <- downloadHandler(
    filename = function() {"df_campo_abierto.pzfx"},
    content = function(file) {write_pzfx(entrenamientos_latencias_prisma(),
                                         # row_names=FALSE,
                                         x_col = "Stage",
                                         file)}
  )
  
  # Gráficos limpios ----
  
  ## dataframe ----
  
  
  
  df_entrenamientos <- reactive({
    req(input$file_latencia)
    read.csv(input$file_latencia$datapath)
  })
  
  latencias_plot <- reactive({
    req(df_entrenamientos())
    df_entrenamientos() %>%
      group_by(Tratamiento, Stage) %>%
      select(Tratamiento,
             Stage,
             Latencia,
             Test,
             Animal)
  })
  
  latencia_graficos <- reactive({
    req(df_entrenamientos())
    df_entrenamientos() %>%
      group_by(Tratamiento, Stage) %>%
      select(Tratamiento,
             Stage,
             Latencia)
  })
  
  ## gráficos ----
  
  output$grafico_lat_plot <- renderPlotly({
    req(df_entrenamientos())
    # dodge_width_lat <- 0.9
    q <- ggplot(df_entrenamientos(), aes(x = Stage,
                                         y = Latencia,
                                         colour = Tratamiento)) +
      stat_summary(fun.data = mean_se,
                   geom = "errorbar",
                   aes(text = "SEM")) +
      stat_summary(fun.y="mean", 
                   geom = "point", 
                   shape = 18,
                   size = 2,
                   aes(text = "Media")) +
      geom_line(stat = "summary",
                fun.y = "mean",
                aes(group = Tratamiento)) +
      scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa')) +
      labs(
        title = "Latencias entrenamientos",
        x = "Número de entrenamiento",
        y = "Latencia de escape (s)") +
      # Tema
      theme_classic()
    
    ggplotly(q)
    
  })
  
  # Mapa de Calor ----
  
  folder_data <- eventReactive(input$load_data, {
    req(input$folder_path)
    folder_path <- input$folder_path
    
    csv_files <- list.files(path = folder_path, pattern = "*.csv")
    
    data <- map_dfr(csv_files, ~ read_csv(file.path(folder_path, .x)))
    data
  }, ignoreNULL = TRUE)
  
  output$heatmap <- renderPlot({
    data <- folder_data()
    req(data)
    
    # Resample and aggregate data
    resolution <- .7
    aggregated_data <- data %>%
      mutate(X = round(X / resolution) * resolution,
             Y = round(Y / resolution) * resolution) %>%
      group_by(X, Y) %>%
      summarise(mean_time = mean(Time, na.rm = TRUE))
    
    # Use geom_raster to plot the heatmap
    ggplot(aggregated_data, aes(x = X, y = Y, fill = mean_time)) +
      geom_raster(interpolate = TRUE) +
      scale_fill_viridis_c(option = "viridis", direction = 1) +
      theme_minimal() +
      labs(x = "X", y = "Y", fill = "Time Density", title = "Time Density Heatmap")
  })
  
}





# Run the app
shinyApp(ui, server)








