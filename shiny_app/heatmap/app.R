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
