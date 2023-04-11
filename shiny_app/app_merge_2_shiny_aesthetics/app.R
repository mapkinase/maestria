# app.R

library(shiny)
library(dplyr)
library(tidyverse)
library(fs)
library(shinythemes)

# Define the UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js")
  ),
  titlePanel(
    tags$img(src = "logo.png", height = "40px", alt = "Logo"),
    "CSV Files Processor and Merger"
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload the data file", buttonLabel = "Browse..."),
      downloadButton("download", "Download the modified data"),
      br(),
      br(),
      textInput("folder", "Enter directory path:"),
      actionButton("loadFiles", "Load Files"),
      downloadButton("downloadFile", "Download Merged CSV", enabled = FALSE)
    ),
    mainPanel(
      tags$h3("Processed Data"),
      tableOutput("table"),
      tags$h3("Merged Data"),
      tableOutput("mergedTable")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # ... (the same server code as before) ...
}

# Run the app
shinyApp(ui, server)
