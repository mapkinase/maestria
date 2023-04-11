library(shiny)
library(tidyverse)
library(stringr)

create_summary_table <- function(data) {
  data %>%
    group_by(Treatment) %>%
    summarise(
      across(
        starts_with("variable"),
        list(
          N = ~ sum(!is.na(.x)),
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
        ),
        .names = "{col}_{fn}"
      )
    )
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("data", "Choose CSV File", accept = ".csv"),
      textInput("filter", "Filter by Stage", "some_string_example"),
      selectInput("x_var", "X-axis Variable", choices = NULL),
      selectInput("y_var", "Y-axis Variable", choices = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Summary Table", tableOutput("summary_table"), downloadButton("download_table", "Download Table"))
      )
    )
  )
)

server <- function(input, output, session) {
  reactive_dataframe <- reactive({
    req(input$data)
    read.csv(input$data$datapath)
  })
  
  observeEvent(reactive_dataframe(), {
    updateSelectInput(session, "x_var", choices = colnames(reactive_dataframe()))
    updateSelectInput(session, "y_var", choices = colnames(reactive_dataframe()))
  })
  
  output$plot <- renderPlot({
    req(input$x_var, input$y_var)
    data <- reactive_dataframe()
    
    # Filter data based on user input
    data_filtered <- data %>%
      filter(str_detect(Stage, input$filter))
    
    p <- ggplot(data = data_filtered) +
      labs(
        title = paste(input$y_var, "Title"),
        subtitle = paste(input$y_var, "subtitle"),
        caption = paste(input$y_var, "caption"),
        x = input$x_var,
        y = input$y_var
      ) +
      theme(
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_blank(),
        axis.line = element_line(color = "#1209c8"),
        axis.title = element_text(size = 13, color = "black", face = "bold"),
        axis.text = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = c(.9, 1.2),
        legend.text = element_text(size = 10),
        legend.background = element_rect(color = "black")
      )
    
    p <- p + aes_string(x = input$x_var, y = input$y_var) +
      geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
      scale_color_manual(values = c("#ff1493", "#4c00ff"))
    
    p
  })
  
  output$summary_table <- renderTable({
    req(input$data)
    data <- reactive_dataframe()
    
    # Filter data based on user input
    data_filtered <- data %>%
      filter(str_detect(Stage, input$filter))
    
    # Create summary table
    summary_table <- create_summary_table(data_filtered)
    
    summary_table
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste("summary_table", ".csv", sep = "")
    },
    content = function(file) {
      req(input$data)
      data <- reactive_dataframe()
      
      # Filter data based on user input
      data_filtered <- data %>%
        filter(str_detect(Stage, input$filter))
      
      # Create summary table
      summary_table <- create_summary_table(data_filtered)
      
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
