library(shiny)
library(tidyverse)
library(fs)

ui <- fluidPage(
  titlePanel("CSV Files Merger"),
  sidebarLayout(
    sidebarPanel(
      textInput("folder", "Enter directory path:"),
      actionButton("loadFiles", "Load Files"),
      downloadButton("downloadFile", "Download Merged CSV", enabled = FALSE)
    ),
    mainPanel(
      tableOutput("mergedTable")
    )
  )
)

server <- function(input, output, session) {
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

shinyApp(ui = ui, server = server)
