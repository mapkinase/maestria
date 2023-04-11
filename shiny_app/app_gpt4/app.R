#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("DT")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("emmeans")
#install.packages("multcompView")
#install.packages("afex")

# Load required libraries ----
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readr)
library(emmeans)
library(multcompView)
library(afex)


# User Interface ----
ui <- fluidPage(
  titlePanel("Data Exploration and ANOVA App"),
  sidebarLayout(
    sidebarPanel(
      ##  File input for uploading CSV ----
      fileInput("file", "Upload your CSV file",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      ## Checkbox for indicating if the CSV has a header ----
      checkboxInput("header", "Header", TRUE),
      tags$hr(),
      ## Dropdown menus for selecting x and y variables ----
      selectInput("x_var", "X Variable", choices = NULL),
      selectInput("y_var", "Y Variable", choices = NULL),
      ## Dropdown menu for selecting plot type ----
      selectInput("plot_type", "Plot Type",
                  choices = c("Scatter Plot", "Box Plot", "Density Plot", "Violin Plot", "Bar Graph"))
    ),
    mainPanel(
      ## Tabs for Data, Data Exploration, Graphs, and ANOVA ----
      tabsetPanel(
        id = "tabs",
        tabPanel("Data", DTOutput("table")),
        tabPanel("Data Exploration", verbatimTextOutput("summary")),
        tabPanel("Graphs", plotOutput("plot")),
        tabPanel("ANOVA", verbatimTextOutput("anova"))
      )
    )
  )
)


# Server logic ----
server <- function(input, output, session) {
  ## Reactive expression for reading CSV data ----
  data <- reactive({
    ### Wait for the input$file to be available ----
    req(input$file)
    
    ### Read the CSV file ----
    inFile <- input$file
    read_csv(inFile$datapath, col_names = input$header)
  })

  ## Display data in the Data tab ----  
  output$table <- renderDT({
    ### Display the data as a DataTable ----
    data()
  })
  
  ## Display summary statistics in the Data Exploration tab ----
  output$summary <- renderPrint({
    ### Wait for the data to be available ----
    req(data())
    ### Calculate summary statistics for numeric columns only ----
    describe_data <- data() %>%
      select_if(is.numeric) %>%
      summarise_all(
        list(
          mean = ~mean(., na.rm = TRUE),
          median = ~median(., na.rm = TRUE),
          sd = ~sd(., na.rm = TRUE),
          range = ~max(., na.rm = TRUE) - min(., na.rm = TRUE),
          min = ~min(., na.rm = TRUE),
          max = ~max(., na.rm = TRUE),
          Q1 = ~quantile(., 0.25, na.rm = TRUE),
          Q3 = ~quantile(., 0.75, na.rm = TRUE)
        )
      )
    ### Print the summary statistics ----
    print(describe_data)
  })
  
  
  ## Display plots in the Graphs tab ----
  output$plot <- renderPlot({
    ### Wait for the data and input selections to be available ----
    req(data(), input$x_var, input$y_var, input$plot_type)
    
    x_var <- sym(input$x_var)
    y_var <- sym(input$y_var)
    
    plot <- ggplot(data(), aes(x = !!x_var, y = !!y_var))
    
    ### Add the appropriate geom layer based on the plot_type input ----
    if (input$plot_type == "Scatter Plot") {
      plot <- plot + geom_point()
    } else if (input$plot_type == "Box Plot") {
      plot <- plot + geom_boxplot()
    } else if (input$plot_type == "Density Plot") {
      plot <- plot + geom_density(aes(y = ..scaled..))
    } else if (input$plot_type == "Violin Plot") {
      plot <- plot + geom_violin()
    } else if (input$plot_type == "Bar Graph") {
      plot <- plot + geom_col()
    }
    
    plot <- plot + labs(x = input$x_var, y = input$y_var)
    plot
  })
 
  ## Update the x_var and y_var dropdown menu choices when new data is uploaded ----
  observeEvent(data(), {
    updateSelectInput(session, "x_var", choices = names(data()))
    updateSelectInput(session, "y_var", choices = names(data()))
  })
  
  ## Display ANOVA results in the ANOVA tab ----
  output$anova <- renderPrint({
    ### Wait for the data to be available ----
    req(data())
    ### Perform a repeated measures two-way ANOVA using the afex package ----
    model <- aov_ez("Subject", "DependentVar", data(), within = c("Factor1", "Factor2"), na.rm = TRUE)
    anova_table <- summary(model)
    print(anova_table)
    
    ### Perform a Tukey post hoc test using the emmeans package ----
    tukey <- emmeans(model, pairwise ~ Factor1 * Factor2, adjust = "tukey")
    print(tukey)
  })
}  
    
# Run the app ----
shinyApp(ui = ui, server = server)
