# Load required libraries
library(shiny)
library(tidyverse)
library(stringr)
library(ggplot2)

# User Interface
ui <- fluidPage(
  titlePanel("Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", dataTableOutput("dataTable")),
        tabPanel("Latencia", plotOutput("latencia")),
        tabPanel("Distancia", plotOutput("distancia")),
        tabPanel("Velocidad", plotOutput("velocidad")),
        tabPanel("Eficiencia_NE", plotOutput("eficiencia_NE")),
        tabPanel("Eficiencia_SO", plotOutput("eficiencia_SO"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  datos_crudos <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  entrenamientos <- reactive({
    df <- datos_crudos() %>%
      group_by(Treatment, Stage) %>%
      filter(str_detect(Stage, "entrenamiento")) %>%
      filter(!str_detect(Stage, "pre")) %>%
      select(Treatment,
             Stage,
             Duration,
             Distance,
             Mean.speed,
             Annulus.NE...path.efficiency.to.entry,
             Annulus.NE...CIPL,
             Annulus.SW...latency.to.first.entry,
             Annulus.SW...CIPL)
    return(df)
  })
  
  entrenamientos_estadisticos <- reactive({
    df <- entrenamientos() %>%
      group_by(Treatment, Stage) %>%
      dplyr::summarise(
        # Latencia
        N_latencia  = sum(!is.na(Duration)), # Calcula la N
        media_latencia = mean(Duration, na.rm=TRUE), # calcula la media
        sd_latencia = sd(Duration, na.rm=TRUE), # calcula la SD
        se_latencia = sd_latencia / sqrt(N_latencia), # calcula SE
        # Distancia
        N_distancia  = sum(!is.na(Distance)),
        media_distancia = mean(Distance, na.rm=TRUE),
        sd_distancia = sd(Distance, na.rm=TRUE),
        se_distancia = sd_distancia / sqrt(N_distancia),
        # velocidad promedio
        N_velocidad  = sum(!is.na(Mean.speed)),
        media_velocidad = mean(Mean.speed, na.rm=TRUE),
        sd_velocidad = sd(Mean.speed, na.rm=TRUE),
        se_velocidad = sd_velocidad / sqrt(N_velocidad),
        # eficiencia annulus NE
        N_eficiencia_NE  = sum(!is.na(Annulus.NE...path.efficiency.to.entry)),
        media_eficiencia_NE = mean(Annulus.NE...path.efficiency.to.entry, na.rm=TRUE),
        sd_eficiencia_NE = sd(Annulus.NE...path.efficiency.to.entry, na.rm=TRUE),
        se_eficiencia_NE = sd_eficiencia_NE / sqrt(N_eficiencia_NE),
        # CIPL annulus NE
        N_cipl_NE  = sum(!is.na(Annulus.NE...CIPL)),
        media_cipl_NE = mean(Annulus.NE...CIPL, na.rm=TRUE),
        sd_cipl_NE = sd(Annulus.NE...CIPL, na.rm=TRUE),
        se_cipl_NE = sd_cipl_NE / sqrt(N_cipl_NE),
        # eficiencia annulus SO
        N_eficiencia_SO  = sum(!is.na(Annulus.SW...CIPL)),
        media_eficiencia_SO = mean(Annulus.SW...CIPL, na.rm=TRUE),
        sd_eficiencia_SO = sd(Annulus.SW...CIPL, na.rm=TRUE),
        se_eficiencia_SO = sd_eficiencia_SO / sqrt(N_eficiencia_SO),
        # CIPL annulus SO
        N_cipl_SO  = sum(!is.na(Annulus.SW...CIPL)),
        media_cipl_SO = mean(Annulus.SW...CIPL, na.rm=TRUE),
        sd_cipl_SO = sd(Annulus.SW...CIPL, na.rm=TRUE),
        se_cipl_SO = sd_cipl_SO / sqrt(N_cipl_SO))
    
    cipl_NE_plot <- ggplot(data = entrenamientos_estadisticos, 
                           aes(x = Stage, 
                               y = media_cipl_NE, 
                               colour = Treatment)) +
      geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
      geom_line(aes(group = Treatment)) +
      scale_color_manual(values = c("#ff1493", "#4c00ff")) +
      geom_errorbar(aes(ymin = media_cipl_NE - se_cipl_NE, 
                        ymax = media_cipl_NE + se_cipl_NE), width = .1, size = 1) +
      labs(
        title = "CIPL Annulus NE",
        subtitle = "Fluoxetina Grupo 1",
        caption = "n = 7 [CUMS_ctrl] y n = 8 [CUMS_fluox] \n \n Se muestra con SEM
\n \n NOTA: Ignorar Entrenamientos reversa",
x = "Día de entrenamiento",
y = "CIPL"
      ) +
      theme(
        plot.margin = unit(c(2,1,1,1), "cm"),
        panel.background = element_blank(),
        axis.line = element_line(color = "#1209c8"),
        axis.title = element_text(size=13, color="black",
                                  face="bold"),
        axis.text = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(margin = margin(t=5)),
        axis.title.y=element_text(margin = margin(r=10)),
        legend.position = c(.9,1.3),
        legend.text = element_text(size=10),
        legend.background = element_rect(color="black")) +
      scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa'))
    
    cipl_SO_plot <- ggplot(data = entrenamientos_estadisticos, aes(x = Stage, y = media_cipl_SO, colour = Treatment)) +
      geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
      geom_line(aes(group = Treatment)) +
      scale_color_manual(values = c("#ff1493", "#4c00ff")) +
      geom_errorbar(aes(ymin = media_cipl_SO - se_cipl_SO, ymax = media_cipl_SO + se_cipl_SO), width = .1, size = 1) +
      labs(
        title = "CIPL Annulus SO Reversa",
        subtitle = "Fluoxetina Grupo 1",
        caption = "n = 7 [CUMS_ctrl] y n = 8 [CUMS_fluox] \n \n Se muestra con SEM
\n \n NOTA: Ignorar entrenamientos 1 ,2 ,3 y 4",
x = "Día de entrenamiento",
y = "CIPL"
      ) +
      theme(
        plot.margin = unit(c(2,1,1,1), "cm"),
        panel.background = element_blank(),
        axis.line = element_line(color = "#1209c8"),
        axis.title = element_text(size=13, color="black",
                                  face="bold"),
        axis.text = element_text(size = 7, color = "black"),
        axis.text.y = element
        
        .text.y = element_text(size=10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(margin = margin(t=5)),
        axis.title.y=element_text(margin = margin(r=10)),
        legend.position = c(.9,1.3),
        legend.text = element_text(size=10),
        legend.background = element_rect(color="black")) +
      scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa'))
    
    ggsave("cipl_NE_plot.png", cipl_NE_plot, width = 10, height = 6, dpi = 300)
    ggsave("cipl_SO_plot.png", cipl_SO_plot, width = 10, height = 6, dpi = 300)
    
    cipl_NE_plot
    cipl_SO_plot
    
    
    
    
    
                                                   