# App funcional doble merge 2

library(shiny)
library(dplyr)
library(tidyverse)
library(fs)

# Define the UI
ui <- fluidPage(
  titlePanel("CSV Files Processor and Merger"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload the data file"),
      downloadButton("download", "Download the modified data"),
      textInput("folder", "Enter directory path:"),
      actionButton("loadFiles", "Load Files"),
      downloadButton("downloadFile", "Download Merged CSV", enabled = FALSE)
    ),
    mainPanel(
      tableOutput("table"),
      tableOutput("mergedTable")
    )
  )
)

# Define the server
server <- function(input, output, session) {
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
}

# Run the app
shinyApp(ui, server)
