Now, please help me combine this two ahiny apps: 

# First App: 



library(shiny)
library(dplyr)
library(tidyverse)
library(fs)
library(shinythemes)



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
      # File input control
      fileInput("file", "Upload the data file", buttonLabel = "Browse..."),
      # Download buttib for processed data
      downloadButton("download", "Download the modified data"),
      # Spacing
      br(),
      br(),
      # Text input for directory path
      textInput("folder", "Enter directory path:"),
      # Button to load files from directory
      actionButton("loadFiles", "Load Files"),
      # Download button for merged CSV
      downloadButton("downloadFile", "Download Merged CSV", enabled = FALSE)
    ),
    mainPanel(
      # Heading for processed data table
      tags$h3("Processed Data"),
      # Display the processed data table
      tableOutput("table"),
      # Heading for merged data table
      tags$h3("Merged Data"),
      # display the merged data table
      tableOutput("mergedTable")

    )
  )
))

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
  

      
    })
    
  })
  
  }

# Run the app
shinyApp(ui, server)


# Second App: 

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(openxlsx)
library(pzfx)
library(stringr)



ui <- fluidPage(
  titlePanel("Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV file", accept = c(".csv")),
      tags$hr()
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tab",
        tabPanel("Entrneamientos Latencias", 
                 plotOutput("entr_lat_plot"),
                 downloadButton("entr_lat_csv", "Download CSV"),
                 downloadButton("entr_lat_xlsx", "Download XLSX"),
                 downloadButton("entr_lat_pzfx", "Download PZFX"))
      )
    )
  )
)

server <- function(input, output, session) {
  df <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
 
    
    entrneamientos_latencias <- reactive({
      req(df())
      df() %>%
        group_by(Treatment, Stage) %>%
        filter(str_detect(Stage, "entrenamiento")) %>%
        filter(!str_detect(Stage, "pre")) %>%
        select(Treatment,
               Stage,
               Duration,
               Test,
               Animal)
    })

    
    output$entr_lat_plot <- renderPlot({
      req(entrneamientos_latencias())
      ggplot(entrneamientos_latencias(), aes(x = Stage, y = Duration, fill = Treatment)) +
        geom_violin(trim = FALSE, position = position_dodge(1)) +
        geom_boxplot(position = position_dodge(1) ) +
        stat_summary(fun.data = mean_se, geom = "pointrange", position = position_dodge(1), colour = "red") + # geom = "errorbar"
        # geom_dotplot(binaxis='y', stackdir='center', dotsize=1, position = position_dodge(1)) +
        geom_jitter(shape=16, position=position_dodge(1), colour = "black" , alpha=0.9) +
        theme_classic()

    })
    

    
    # Download buttons for CSV, XLSX, and PZFX files
    # Replace `entrneamientos_latencias` with the corresponding sub-dataframe name for each tab
    
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
    
    # Add the corresponding download handlers for each sub-dataframe and tab
    # Replace the `entr_lat_` prefix with the appropriate prefix for each tab

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
    
    ####
    
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
    
    ####
    
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
    
    
    ####
    
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
    
    ###
    
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
    
    ###
    
    
  }
  
  shinyApp(ui = ui, server = server)