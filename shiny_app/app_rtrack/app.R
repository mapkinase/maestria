# Load required libraries
# Load required libraries
library(shiny)
library(Rtrack)

# Define UI
ui <- fluidPage(
  titlePanel("Rtrack Shiny App"),
  mainPanel(
    tabsetPanel(
      tabPanel("Density Maps All", plotOutput("density_maps_all")),
      tabPanel("Density Maps Individual", plotOutput("density_maps_individual")),
      tabPanel("Strategy Plots", plotOutput("strategy_plots"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Read experiment data
  experiment <- reactive({
    file_path <- "ketamina_y_fluoxetina.xlsx"
    data_dir <- getwd()
    Rtrack::read_experiment(file_path, format = "Excel", data.dir = data_dir)
  })
  
  # Process experiment data
  strategies <- reactive({
    Rtrack::call_strategy(experiment()$metrics)
  })
  
  # Generate plots
  output$density_maps_all <- renderPlot({
    Rtrack::plot_density(experiment()$metrics)
  })
  
  output$density_maps_individual <- renderPlot({
    group_1.reversal.metrics <- experiment()$metrics[experiment()$factors$Tratamiento == "Fluoxetina" &
                                                       (experiment()$factors$`_Day` == 33 | experiment()$factors$`_Day` == 33)]
    group_2.reversal.metrics <- experiment()$metrics[experiment()$factors$Tratamiento == "ketamina" &
                                                       (experiment()$factors$`_Day` == 33 | experiment()$factors$`_Day` == 33)]
    group_3.reversal.metrics <- experiment()$metrics[experiment()$factors$Tratamiento == "control" &
                                                       (experiment()$factors$`_Day` == 33 | experiment()$factors$`_Day` == 33)]
    
    par(mfrow = c(1, 3))
    Rtrack::plot_density(group_1.reversal.metrics, title = "Fluoxetina reversa")
    Rtrack::plot_density(group_2.reversal.metrics, title = "control reversal")
    Rtrack::plot_density(group_3.reversal.metrics, title = "ketamina reversal")
    par(mfrow = c(1, 1))
  })
  
  output$strategy_plots <- renderPlot({
    Rtrack::plot_strategies(strategies(), experiment = experiment(), factor = "Tratamiento", exclude.probe = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
