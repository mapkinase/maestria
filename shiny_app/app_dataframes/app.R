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
                 downloadButton("entr_lat_pzfx", "Download PZFX")),
        tabPanel("Entrenamientos Distancia", 
                 plotOutput("entr_dist_plot"),
                 downloadButton("entr_dist_csv", "Download CSV"),
                 downloadButton("entr_dist_xlsx", "Download XLSX"),
                 downloadButton("entr_dist_pzfx", "Download PZFX")),
        tabPanel("Entrenamientos Velocidad", 
                 plotOutput("entr_vel_plot"),
                 downloadButton("entr_vel_csv", "Download CSV"),
                 downloadButton("entr_vel_xlsx", "Download XLSX"),
                 downloadButton("entr_vel_pzfx", "Download PZFX")),
        tabPanel("Pruebas Porcentaje Cuadrante Blanco", 
                 plotOutput("pruebas_pct_plot"),
                 downloadButton("pruebas_pct_csv", "Download CSV"),
                 downloadButton("pruebas_pct_xlsx", "Download XLSX"),
                 downloadButton("pruebas_pct_pzfx", "Download PZFX")),
        tabPanel("Pruebas Numero Cruces Cuadrante Blanco", 
                 plotOutput("pruebas_num_plot"),
                 downloadButton("pruebas_num_csv", "Download CSV"),
                 downloadButton("pruebas_num_xlsx", "Download XLSX"),
                 downloadButton("pruebas_num_pzfx", "Download PZFX")),
        tabPanel("Pruebas Distancia Promedio Blanco", 
                 plotOutput("pruebas_dist_blanco_plot"),
                 downloadButton("pruebas_dist_blanco_csv", "Download CSV"),
                 downloadButton("pruebas_dist_blanco_xlsx", "Download XLSX"),
                 downloadButton("pruebas_dist_blanco_pzfx", "Download PZFX")),
        tabPanel("Pruebas Distancia Promedio Antiguo", 
                 plotOutput("pruebas_dist_antiguo_plot"),
                 downloadButton("pruebas_dist_antiguo_csv", "Download CSV"),
                 downloadButton("pruebas_dist_antiguo_xlsx", "Download XLSX"),
                 downloadButton("pruebas_dist_antiguo_pzfx", "Download PZFX"))
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
  
    entrenamientos_distancia <- reactive({
      req(df())
      df() %>%
        group_by(Treatment, Stage) %>%
        filter(str_detect(Stage, "entrenamiento")) %>%
        filter(!str_detect(Stage, "pre")) %>%
        select(Treatment,
               Stage,
               Distance)
    })
    
    entrenamientos_velocidad <- reactive({
      req(df())
      df() %>%
        group_by(Treatment, Stage) %>%
        filter(str_detect(Stage, "entrenamiento")) %>% 
        filter(!str_detect(Stage, "pre")) %>% 
        select(Treatment, 
               Stage, 
               Mean.speed)
    })
    
    pruebas_porcentaje_cuadrante_blanco <- reactive({
      req(df())
      df() %>%
        group_by(Treatment, Stage) %>%
        filter(str_detect(Stage, "prueba")) %>% 
        select(Treatment, 
               Stage, 
               SE.cuadrante...time,
               SWcuadrante...time,
               NW.Cuadrante...time,
               NE.cuadrante...time) %>%
        rename(
          NE_cuadrante_time = `NE.cuadrante...time`,
          SE_cuadrante_time = `SE.cuadrante...time`,
          SW_cuadrante_time = `SWcuadrante...time`,
          NW_cuadrante_time = `NW.Cuadrante...time`
        ) %>%
        mutate(
          cuadrante_blanco = case_when(
            Stage == "prueba_1" ~ (NE_cuadrante_time * 100) / 60,
            Stage == "prueba_2" ~ (NE_cuadrante_time * 100) / 60,
            Stage == "prueba_rev" ~ (SW_cuadrante_time * 100) / 60
          )
        )
    })
    
    pruebas_numero_cruces_cuadrante_blanco <- reactive({
      req(df())
      df() %>%
        group_by(Treatment, Stage) %>%
        filter(str_detect(Stage, "prueba")) %>% 
        select(Treatment, 
               Stage, 
               Annulus.NE...entries,
               Annulus.SW...entries) %>%
        mutate(
          cruces_blanco = case_when(
            Stage == "prueba_1" ~ (Annulus.NE...entries),
            Stage == "prueba_2" ~ (Annulus.NE...entries),
            Stage == "prueba_rev" ~ (Annulus.SW...entries)
          )
        )
    })
    
    pruebas_distancia_promedio_blanco <- reactive({
      req(df())
      df() %>%
        group_by(Treatment, Stage) %>%
        filter(str_detect(Stage, "prueba")) %>% 
        select(Treatment, 
               Stage, 
               Annulus.NE...mean.distance.from,
               Annulus.SW...mean.distance.from) %>%
        mutate(
          distancia_promedio_blanco = case_when(
            Stage == "prueba_1" ~ (Annulus.NE...mean.distance.from),
            Stage == "prueba_2" ~ (Annulus.NE...mean.distance.from),
            Stage == "prueba_rev" ~ (Annulus.SW...mean.distance.from)
          )
        )
    })
    
    pruebas_distancia_promedio_antiguo <- reactive({
      req(df())
      df() %>%
        group_by(Treatment, Stage) %>%
        filter(str_detect(Stage, "prueba")) %>% 
        select(Treatment, 
               Stage, 
               Annulus.NE...mean.distance.from,
               Annulus.SW...mean.distance.from) %>%
        mutate(
          distancia_promedio_antiguo = case_when(
            Stage == "prueba_rev" ~ (Annulus.NE...mean.distance.from)
          )
        )
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
    
    output$entr_dist_plot <- renderPlot({
      req(entrenamientos_distancia())
      ggplot(entrenamientos_distancia(), aes(x = Stage, y = Distance, fill = Treatment)) +
        stat_summary(fun.data = mean_se, geom = "pointrange", position = position_dodge(1)) +
        stat_summary(fun.y=mean, geom="line", group=1, aes(colour = Treatment)) 
        theme_classic()
    })
    
    output$entr_vel_plot <- renderPlot({
      req(entrenamientos_velocidad())
      ggplot(entrenamientos_velocidad(), aes(x = Stage, y = Mean.speed, fill = Treatment)) +
        geom_violin() +
        theme_minimal()
    })
    
    output$pruebas_pct_plot <- renderPlot({
      req(pruebas_porcentaje_cuadrante_blanco())
      ggplot(pruebas_porcentaje_cuadrante_blanco(), aes(x = Stage, y = cuadrante_blanco, fill = Treatment)) +
        geom_violin() +
        theme_minimal()
    })
    
    output$pruebas_num_plot <- renderPlot({
      req(pruebas_numero_cruces_cuadrante_blanco())
      ggplot(pruebas_numero_cruces_cuadrante_blanco(), aes(x = Stage, y = cruces_blanco, fill = Treatment)) +
        geom_violin() +
        theme_minimal()
    })
    
    output$pruebas_dist_blanco_plot <- renderPlot({
      req(pruebas_distancia_promedio_blanco())
      ggplot(pruebas_distancia_promedio_blanco(), aes(x = Stage, y = distancia_promedio_blanco, fill = Treatment)) +
        geom_violin() +
        theme_minimal()
    })
    
    output$pruebas_dist_antiguo_plot <- renderPlot({
      req(pruebas_distancia_promedio_antiguo())
      ggplot(pruebas_distancia_promedio_antiguo(), aes(x = Stage, y = distancia_promedio_antiguo, fill = Treatment)) +
        geom_violin() +
        theme_minimal()
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
  
              
    