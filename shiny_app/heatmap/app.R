# Install necessary packages if you haven't already
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinydashboard", quietly = TRUE)) install.packages("shinydashboard")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "CSV Time Density Heatmap"),
  dashboardSidebar(),
  dashboardBody(
    box(width = 12, plotOutput("heatmap"))
  )
)

# Define server logic
server <- function(input, output) {
  observe({
    # Read all CSV files in the "data" folder
    csv_files <- list.files(path = "data", pattern = "*.csv")
    
    # Read CSV files and combine them into a single dataframe
    data <- map_dfr(csv_files, ~ read_csv(file.path("data", .x)))
    
    # Convert the Time column to numeric
    data$Time <- as.numeric(data$Time)
    
    # Create a heatmap with time densities
    output$heatmap <- renderPlot({
      ggplot(data, aes(x = X, y = Y, weight = Time)) +
        geom_bin2d(bins = 30) +
        scale_fill_viridis_c(option = "viridis", direction = 1) +
        theme_minimal() +
        labs(x = "X", y = "Y", fill = "Time Density", title = "Time Density Heatmap")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js")
  ),
  titlePanel(
    tags$img(src = "logo.png", height = "200px", alt = "Logo"),
    "CSV Files Processor and Merger"
  ),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "sidebar_tab",
        tabPanel("Mapa de Calor",
                 fileInput("archivos_coordenadas", "Seleccionar carpeta con coordenadas", multiple = TRUE, accept = c(".csv")),
                 actionButton("subir_coordenadas", "Subir Coordenadas")),
        tags$hr()
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tab",
        tabPanel("Mapa de Calor",
                 tags$h3("Mapa de Calor"),
                 plotOutput("heatmap"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$subir_coordenadas, {
    if (is.null(input$archivos_coordenadas)) {
      return(NULL)
    }
    
    folder_path <- input$archivos_coordenadas$datapath
    csv_files <- list.files(path = folder_path, pattern = "*.csv")
    data <- map_dfr(csv_files, ~ read_csv(file.path(folder_path, .x), col_names = c("Time", "X", "Y")))
    data$Time <- as.numeric(data$Time)
    
    output$heatmap <- renderPlot({
      ggplot(data, aes(x = X, y = Y, z = Time)) +
        stat_summary_2d(fun = "sum", geom = "raster", aes(fill = ..value..), bins = 30) +
        scale_fill_viridis_c(option = "viridis", direction = 1) +
        theme_minimal() +
        labs(x = "X", y = "Y", fill = "Time Density", title = "Time Density Heatmap")
    })
  })
}

shinyApp(ui, server)


server <- function(input, output, session) {
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
    
    ggplot(data, aes(x = X, y = Y, weight = Time)) +
      geom_bin2d(bins = 30) +
      scale_fill_viridis_c(option = "viridis", direction = 1) +
      theme_minimal() +
      labs(x = "X", y = "Y", fill = "Time Density", title = "Time Density Heatmap")
  })
}

----
  
  observeEvent(input$subir_coordenadas, {
    req(input$archivos_coordenadas)
    
    data <- map_dfr(input$archivos_coordenadas$datapath, ~ read_csv(.x, col_names = c("Time", "X", "Y")))
    data$Time <- as.numeric(data$Time)
    
    output$heatmap <- renderPlot({
      ggplot(data, aes(x = X, y = Y, z = Time)) +
        stat_summary_2d(fun = "sum", geom = "raster", aes(fill = after_stat(value)), bins = 30) +
        scale_fill_viridis_c(option = "viridis", direction = 1) +
        theme_minimal() +
        labs(x = "X", y = "Y", fill = "Time Density", title = "Time Density Heatmap")
    })
  })