library(shiny)
library(dplyr)

# Define the UI
ui <- fluidPage(
  # Define the file input
  fileInput("file", "Upload the data file"),
  
  # Define the download button
  downloadButton("download", "Download the modified data"),
  
  # Define the table output
  tableOutput("table")
)

# Define the server
server <- function(input, output) {
  # Define the reactive dataframe based on the uploaded file
  data <- reactive({
    if(is.null(input$file)) return(NULL)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    df %>% rename(
      Velocidad = Mean.speed,
      TiempoSE = SE.cuadrante...time,
      TiempoSO = SWcuadrante...time,
      TiempoNO = NW.Cuadrante...time,
      TiempoNE = NE.cuadrante...time
    )
  })
  
  # Render the table output
  output$table <- renderTable({
    req(data())
    data()
  })
  
  # Define the download button
  output$download <- downloadHandler(
    filename = function() {
      paste("modified_data", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)
