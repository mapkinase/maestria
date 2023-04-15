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

# Define the UI
ui <- fluidPage(
  # Add a theme
  theme = shinytheme("cerulean"),
  # Include custom CSS and JavaScript files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js")
  ),
  # Define the title panel
  titlePanel(
    # Add logo image
    tags$img(src = "logo.png", height = "200px", alt = "Logo"),
    "CSV Files Processor and Merger"
  ),
  #Define the layout
  sidebarLayout(
    sidebarPanel(
      # File input control for processing and merging CSV files
      fileInput("file", "Upload the data file for processing", buttonLabel = "Browse..."),
      downloadButton("download", "Download the modified data"),
      br(),
      br(),
      textInput("folder", "Enter directory path:"),
      actionButton("loadFiles", "Load Files"),
      downloadButton("downloadFile", "Download Merged CSV", enabled = FALSE),
      # File input control for data analysis
      fileInput("analysisFile", "Choose CSV file for analysis", accept = c(".csv")),
      tags$hr()
    ),
    mainPanel(
      # Heading for processed data table
      tags$h3("Processed Data"),
      # Display the processed data table
      tableOutput("table"),
      # Heading for merged data table
      tags$h3("Merged Data"),
      # display the merged data table
      tableOutput("mergedTable"),
      tabsetPanel(
        id = "main_tab",
        tabPanel("Entrneamientos Latencias",
                 plotlyOutput("entr_lat_plot"),
                 downloadButton("entr_lat_csv", "Download CSV"),
                 downloadButton("entr_lat_xlsx", "Download XLSX"),
                 downloadButton("entr_lat_pzfx", "Download PZFX"))
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # ... (the same server code as before) ...
  data <- reactive({
    if (is.null(input$file)) return(NULL)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    df %>% rename(
      Velocidad = Mean.speed,
      TiempoSE = SE.cuadrante...time,
      TiempoSO = SWcuadrante...time,
      TiempoNO = NW.Cuadrante...time,
      TiempoNE = NE.cuadrante...time
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
  
  mergedDF <- reactiveVal()
  
  selectedColumns <- c("Test", "Animal", "Treatment", "Stage", "Trial", "Date", "Time", "Duration", "Distance", "Velocidad", "TiempoSE", "TiempoSO", "TiempoNO", "TiempoNE", "EntradasNE", "PromedioNE", "EficienciaNE", "EntradasSO", "PromedioSO", "EficianciaSO")
  
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
  
  
  
  df_analysis <- reactive({
    req(input$analysisFile)
    read.csv(input$analysisFile$datapath)
  })
  
  entrneamientos_latencias <- reactive({
    req(df_analysis())
    df_analysis() %>%
      group_by(Treatment, Stage) %>%
      filter(str_detect(Stage, "entrenamiento")) %>%
      filter(!str_detect(Stage, "pre")) %>%
      select(Treatment,
             Stage,
             Duration,
             Test,
             Animal)
  })
  
  output$entr_lat_plot <- renderPlotly({
    req(entrneamientos_latencias())
    p <- ggplot(entrneamientos_latencias(), aes(x = Stage, y = Duration, fill = Treatment)) +
      geom_violin(trim = FALSE, position = position_dodge(1)) +
      geom_boxplot(position = position_dodge(1)) +
      stat_summary(fun.data = mean_se, geom = "pointrange", position = position_dodge(1), colour = "red") +
      geom_jitter(shape = 16, position = position_dodge(1), colour = "black", alpha = 0.9) +
      theme_classic()
    
    ggplotly(p)
  })
  
  
  # Download buttons for CSV, XLSX, and PZFX files
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


# Run the app
shinyApp(ui, server)
