

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}

library(shiny)
library(tidyverse)
library(stringr)
library(DT)

ui <- fluidPage(
  titlePanel("Pruebas de Water Maze"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c(".csv"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Datos", DT::dataTableOutput("table")),
        tabPanel("Porcentaje cuadrantes", plotOutput("porcentaje_cuadrantes_plot")),
        tabPanel("Número de cruces en cuadrante blanco", plotOutput("numero_cruces_plot")),
        tabPanel("Distancia promedio a annulus blanco", plotOutput("distancia_promedio_plot"))
      )
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data())
  })
  
  pruebas <- reactive({
    df <- data()
    
    # Assuming the data has columns: grupo, entrenamiento, latencia, distancia, velocidad, eficiencia_NE, eficiencia_SO
    # Group the data by grupo and entrenamiento
    df_grouped <- df %>%
      group_by(Treatment, Stage) %>%
      filter(str_detect(Stage, "prueba")) %>% 
      select(Treatment, # selecciona columnas con variables de interes
             Stage, 
             Duration, 
             Distance,
             SE.cuadrante...time,
             SWcuadrante...time,
             NW.Cuadrante...time,
             NE.cuadrante...time,
             Annulus.NE...entries,
             Annulus.NE...mean.distance.from,
             Annulus.SW...entries,
             Annulus.SW...mean.distance.from) %>%
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
      ) %>% 
      mutate(
        cruces_blanco = case_when(
          Stage == "prueba_1" ~ (Annulus.NE...entries),
          Stage == "prueba_2" ~ (Annulus.NE...entries),
          Stage == "prueba_rev" ~ (Annulus.SW...entries)
        )
      ) %>%
      mutate(
        distancia_promedio_blanco = case_when(
          Stage == "prueba_1" ~ (Annulus.NE...mean.distance.from),
          Stage == "prueba_2" ~ (Annulus.NE...mean.distance.from),
          Stage == "prueba_rev" ~ (Annulus.SW...mean.distance.from)
        ),
        distancia_promedio_antiguo = case_when(
          Stage == "prueba_rev" ~ (Annulus.NE...mean.distance.from)
        )
      ) %>%
      group_by(Treatment, Stage) %>%
      summarise(
        # cuadrante_blanco
        N_cuadrante_blanco  = sum(!is.na(cuadrante_blanco)),
        media_cuadrante_blanco = mean(cuadrante_blanco, na.rm=TRUE),
        sd_cuadrante_blanco = sd(cuadrante_blanco, na.rm=TRUE),
        se_cuadrante_blanco = sd_cuadrante_blanco / sqrt(N_cuadrante_blanco),
        # cuadrante_opuestos
        # N_cuadrantes_opuestos  = sum(!is.na(cuadrantes_opuestos)),
        # media_cuadrantes_opuestos = mean(cuadrantes_opuestos, na.rm=TRUE),
        # sd_cuadrantes_opuestos = sd(cuadrantes_opuestos, na.rm=TRUE),
        # se_cuadrantes_opuestos = sd_cuadrantes_opuestos / sqrt(N_cuadrantes_opuestos),
        # annulus_cruces_blanco
        N_annulus_cruces_blanco  = sum(!is.na(cruces_blanco)),
        media_annulus_cruces_blanco = mean(cruces_blanco, na.rm=TRUE),
        sd_annulus_cruces_blanco = sd(cruces_blanco, na.rm=TRUE),
        se_annulus_cruces_blanco = sd_annulus_cruces_blanco / sqrt(N_annulus_cruces_blanco),
        # annulus_dist_promedio_blanco
        N_annulus_dist_promedio_blanco  = sum(!is.na(distancia_promedio_blanco)),
        media_annulus_dist_promedio_blanco = mean(distancia_promedio_blanco, na.rm=TRUE),
        sd_annulus_dist_promedio_blanco = sd(distancia_promedio_blanco, na.rm=TRUE),
        se_annulus_dist_promedio_blanco = sd_annulus_dist_promedio_blanco / sqrt(N_annulus_dist_promedio_blanco),
        # annulus dist promedio antiguo
        N_annulus_dist_promedio_antiguo  = sum(!is.na(distancia_promedio_antiguo)),
        media_annulus_dist_promedio_antiguo = mean(distancia_promedio_antiguo, na.rm=TRUE),
        sd_annulus_dist_promedio_antiguo = sd(distancia_promedio_antiguo, na.rm=TRUE),
        se_annulus_dist_promedio_antiguo = sd_annulus_dist_promedio_antiguo / sqrt(N_annulus_dist_promedio_antiguo)
      ) 
  })
  
  
  output$porcentaje_cuadrantes_plot <- renderPlot({
    ggplot(
      data = pruebas(), 
      aes(
        x = Stage, 
        y = media_cuadrante_blanco, 
        fill = Treatment)) +
      geom_col(position = position_dodge()) +
      # geom_point(aes(
      #   colour = Treatment), 
      #   alpha = 1, 
      #   size = 4) +
      # geom_line(aes(
      #   group = Treatment)) +
      # scale_color_manual(
      #   values = c("#ff1493", "#4c00ff")) +
      geom_errorbar(aes(ymin = media_cuadrante_blanco - se_cuadrante_blanco, 
                        ymax = media_cuadrante_blanco + se_cuadrante_blanco), 
                    size = .1, width = .3, position = position_dodge(width = 0.9)) 
      # labs(
      #   title = "Latencia en entrenamienos de Water Maze",
      #   subtitle = "Fluoxetina Grupo 1",
      #   caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n \n Se muestra con SEM \n \n línea roja muestra 'cut off'de aprendizaje (17 segundos)" ,
      #   x = "Día de entrenamiento",
      #   y = "Latencia promedio (s)"
      # ) +
      # theme(
      #   plot.margin = unit(c(1,1,1,1), "cm"),
      #   panel.background = element_blank(),
      #   axis.line = element_line(color = "#1209c8"),
      #   axis.title = element_text(size=13, color="black", 
      #                             face="bold"),
      #   axis.text = element_text(size = 7, color = "black"),
      #   axis.text.y = element_text(size=10), 
      #   axis.ticks.x = element_blank(),
      #   axis.text.x = element_text(margin = margin(t=5)),
      #   axis.title.y=element_text(margin = margin(r=10)),
      #   legend.position = c(.9,1.2),
      #   legend.text = element_text(size=10),
      #   legend.background = element_rect(color="black")) +
      # geom_hline(yintercept = 17, linetype = "dotted", color = "red", size = .6) +
      # scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa'))
    
  })
  
  output$numero_cruces_plot <- renderPlot({
    ggplot(
      data = pruebas(), 
      aes(
        x = Stage, 
        y = media_annulus_cruces_blanco, 
        fill = Treatment)) +
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(ymin = media_annulus_cruces_blanco - se_annulus_cruces_blanco, 
                        ymax = media_annulus_cruces_blanco + se_annulus_cruces_blanco), 
                    size = .1, width = .3, position = position_dodge(width = 0.9)) 

  })
  
  output$distancia_promedio_plot <- renderPlot({
    ggplot(data = pruebas(), 
           aes(x = Stage, y = media_annulus_dist_promedio_blanco, colour = Treatment)) +
      geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
      geom_line(aes(group = Treatment)) +
      geom_errorbar(aes(ymin = media_annulus_dist_promedio_blanco - se_annulus_dist_promedio_blanco, 
                        ymax = media_annulus_dist_promedio_blanco + se_annulus_dist_promedio_blanco), width = .1, size = .5) +
      geom_point(aes(x = Stage, 
                     y = media_annulus_dist_promedio_antiguo, # annulus_antiguo_dentro_mismo_plot
                     shape = Treatment 
                     ), 
                 size = 5) +
      geom_errorbar(aes(ymin = media_annulus_dist_promedio_antiguo - se_annulus_dist_promedio_antiguo, 
                        ymax = media_annulus_dist_promedio_antiguo + se_annulus_dist_promedio_antiguo), width = .1, size = .5)
  })
  
}

shinyApp(ui = ui, server = server)



