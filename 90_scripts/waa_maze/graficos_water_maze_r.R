library(tidyverse)
library(plyr)

datos_crudos <- read.csv("datos_crudos_any_maze.csv")

colnames(datos_crudos)



# Crear subset de datos ----
  # Vamos a crear un subset de datos para entrenamientos y pruebas

## prueba reversa ----

 
### Crear columna con porcentaje ----

prueba_reversa <- subset(datos_crudos, (Stage=='prueba_rev')) |> # filtra rows solo de prueba_reversa
  mutate( # este comando de mutate saca el porcentaje de zonas deseadas vs zonas opuestas
    porcentaje_cuadrante_blanco_SO = ((SWcuadrante...time * 100) / 60), 
    porcentaje_cuadrante_antiguo_NE = ((NE.cuadrante...time * 100) / 60),
    porcentaje_cuadrante_opuesto_1 = ((SE.cuadrante...time * 100) / 60),
    porcentaje_cuadrante_opuesto_2 = ((NW.Cuadrante...time * 100) / 60),
    comprobar_porcentajes = porcentaje_cuadrante_blanco_SO + porcentaje_cuadrante_antiguo_NE + porcentaje_cuadrante_opuesto_1 + porcentaje_cuadrante_opuesto_2 # solo para combprobar que la suma sea el 100% (60 segundos)
  ) |>
  pivot_longer( # este comando le da formato a los porcentajes para graficarlo en R
    cols = starts_with("porcentaje_"),
    names_to = "tipo_cuadrante",
    values_to = "porcentaje_en_cuadrante"
  )

colnames(prueba_reversa)

### Crear estadísticos de variables ----
### 
#### porcentaje en zonas ----

prueba_reversa_estadisticos <- prueba_reversa |>
  group_by(Treatment, tipo_cuadrante) %>%
  dplyr::summarise(N_porcentaje_en_cuadrante  = length(porcentaje_en_cuadrante),
                   media_porcentaje_en_cuadrante = mean(porcentaje_en_cuadrante),
                   sd_porcentaje_en_cuadrante   = sd(porcentaje_en_cuadrante),
                   se_porcentaje_en_cuadrante   = sd_porcentaje_en_cuadrante / sqrt(N_porcentaje_en_cuadrante))

colnames(prueba_reversa_estadisticos)

write.csv(prueba_reversa, "prueba_reversa.csv")
### Graficar



ggplot() +               
  geom_jitter(data = prueba_reversa, aes(x = Treatment, y = porcentaje_en_cuadrante, colour = Treatment, fill = Treatment), 
             size = 1, shape = 21,)+
  geom_point(data = prueba_reversa_estadisticos, aes(x = Treatment, y = media_porcentaje_en_cuadrante, colour = Treatment), 
             size = 5, shape = 21)+
  geom_errorbar(data = prueba_reversa_estadisticos, aes(x = Treatment, y = media_porcentaje_en_cuadrante, 
                                                        ymin = media_porcentaje_en_cuadrante - se_porcentaje_en_cuadrante, ymax = media_porcentaje_en_cuadrante + se_porcentaje_en_cuadrante), 
                width = .1, size = 1)+
  labs(x = "X-Data", y = "Y-Data")+
  ggtitle("Combined Plot")  +
  facet_grid(~tipo_cuadrante)


## numero_cruces antiguo vs nuevo ----
colnames(prueba_reversa)


prueba_reversa_cruces <- subset(datos_crudos, (Stage=='prueba_rev')) |> 
  pivot_longer( # este comando le da formato a los porcentajes para graficarlo en R
    cols = c("Annulus.NE...entries","Annulus.SW...entries"),
    names_to = "tipo_annulus",
    values_to = "entradas_annulus"
  )

prueba_reversa_estadisticos_cruces <- prueba_reversa_cruces |>
  group_by(Treatment, tipo_annulus) %>%
  dplyr::summarise(N_entradas_annulus  = length(entradas_annulus),
                   media_entradas_annulus = mean(entradas_annulus),
                   sd_entradas_annulus   = sd(entradas_annulus),
                   se_entradas_annulus   = sd_entradas_annulus / sqrt(N_entradas_annulus))

ggplot() +               
  geom_jitter(data = prueba_reversa_cruces, aes(x = Treatment, y = entradas_annulus, colour = Treatment, fill = Treatment), 
              size = 1, shape = 21,)+
  geom_point(data = prueba_reversa_estadisticos_cruces, aes(x = Treatment, y = media_entradas_annulus, colour = Treatment), 
             size = 5, shape = 21)+
  geom_errorbar(data = prueba_reversa_estadisticos_cruces, aes(x = Treatment, y = media_entradas_annulus, 
                                                        ymin = media_entradas_annulus - se_entradas_annulus, ymax = media_entradas_annulus + se_entradas_annulus), 
                width = .1, size = 1)+
  labs(x = "X-Data", y = "Y-Data")+
  ggtitle("Combined Plot")  +
  facet_grid(~tipo_annulus)

## distancia_media nuevo vs antiguo ----

colnames(prueba_reversa)


prueba_reversa_dist_media <- subset(datos_crudos, (Stage=='prueba_rev')) |> 
  pivot_longer( # este comando le da formato a los porcentajes para graficarlo en R
    cols = c("Annulus.NE...mean.distance.from","Annulus.SW...mean.distance.from"),
    names_to = "tipo_annulus",
    values_to = "distancia_annulus"
  )

prueba_reversa_estadisticos_dist <- prueba_reversa_dist_media |>
  group_by(Treatment, tipo_annulus) %>%
  dplyr::summarise(N_distancia_annulus  = length(distancia_annulus),
                   media_distancia_annulus = mean(distancia_annulus),
                   sd_distancia_annulus   = sd(distancia_annulus),
                   se_distancia_annulus   = sd_distancia_annulus / sqrt(N_distancia_annulus))

ggplot() +               
  geom_jitter(data = prueba_reversa_dist_media, aes(x = Treatment, y = distancia_annulus, colour = Treatment, fill = Treatment), 
              size = 1, shape = 21,)+
  geom_point(data = prueba_reversa_estadisticos_dist, aes(x = Treatment, y = media_distancia_annulus, colour = Treatment), 
             size = 5, shape = 21)+
  geom_errorbar(data = prueba_reversa_estadisticos_dist, aes(x = Treatment, y = media_distancia_annulus, 
                                                               ymin = media_distancia_annulus - se_distancia_annulus, ymax = media_distancia_annulus + se_distancia_annulus), 
                width = .1, size = 1)+
  labs(x = "X-Data", y = "Y-Data")+
  ggtitle("Combined Plot")  +
  facet_grid(~tipo_annulus)


## entropía ----





# Entrenamientos ----


# filter filtra rows , ver: https://sebastiansauer.github.io/dplyr_filter/
# para filtar los entrenamientos, usaremos library(stringr)
# El script filtra los datos por entrenamiento y elimina los de pre-entrenamiento y reversa
# PENDIENTE: hacer que el filter haga todo en un paso   , tal vez con Regex?
#Después filtramos las columas (variables) que queremos con select

library(stringr) 

entrenamientos <- datos_crudos |>
  group_by(Treatment, Stage) |>
  filter(str_detect(Stage, "entrenamiento")) |>
  filter(!str_detect(Stage, "pre")) |>
  select(Treatment, 
         Stage, 
         Duration, 
         Distance, 
         Mean.speed, 
         Annulus.NE...path.efficiency.to.entry, 
         Annulus.NE...CIPL, 
         Annulus.SW...latency.to.first.entry,
         Annulus.SW...CIPL)

### Estadísticos ----

entrenamientos_estadisticos <- entrenamientos |>
  group_by(Treatment, Stage) %>%
  dplyr::summarise(
    # Latencia
    N_latencia  = sum(!is.na(Duration)),
    media_latencia = mean(Duration, na.rm=TRUE),
    sd_latencia = sd(Duration, na.rm=TRUE),
    se_latencia = sd_latencia / sqrt(N_latencia),
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
    se_cipl_SO = sd_cipl_SO / sqrt(N_cipl_SO)
  )


colnames(entrenamientos_estadisticos)




## Entrenamientos semana 1 ----

entrenamientos_1 <- entrenamientos |>
  group_by(Treatment, Stage) |>
  filter(!str_detect(Stage, "rev")) 



### Latencias ----

ggplot(data = entrenamientos_estadisticos, aes(x = Stage, y = media_latencia, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_latencia - se_latencia, ymax = media_latencia + se_latencia), width = .1, size = 1) +
  labs(
    title = "Latencia en entrenamienos de Water Maze",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "Día de entrenamiento",
    y = "Latencia promedio (s)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  geom_hline(yintercept = 17, linetype = "dotted", color = "red", size = .6) +
  scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa'))


### Distancia ----

ggplot(data = entrenamientos_estadisticos, aes(x = Stage, y = media_distancia, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_distancia - se_distancia, ymax = media_distancia + se_distancia), width = .1, size = 1) +
  labs(
    title = "distancia recorrida entrenamienos de Water Maze",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "Día de entrenamiento",
    y = "distancia (m)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa'))

### Velocidad ----

ggplot(data = entrenamientos_estadisticos, aes(x = Stage, y = media_velocidad, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_velocidad - se_velocidad, ymax = media_velocidad + se_velocidad), width = .1, size = 1) +
  labs(
    title = "velocidad recorrida entrenamienos de Water Maze",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "Día de entrenamiento",
    y = "velocidad promedio (m/s)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa'))

### Eficiencia Annulus_NE ----



ggplot(data = entrenamientos_estadisticos, aes(x = Stage, y = media_eficiencia_NE, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_eficiencia_NE - se_eficiencia_NE, ymax = media_eficiencia_NE + se_eficiencia_NE), width = .1, size = 1) +
  labs(
    title = "eficiencia Annulus NE",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "Día de entrenamiento",
    y = "eficiencia"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa'))



### Eficiencia Annulus_SO ----

ggplot(data = entrenamientos_estadisticos, aes(x = Stage, y = media_eficiencia_SO, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_eficiencia_SO - se_eficiencia_SO, ymax = media_eficiencia_SO + se_eficiencia_SO), width = .1, size = 1) +
  labs(
    title = "eficiencia Annulus SO Reversa",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "Día de entrenamiento",
    y = "eficiencia"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  scale_x_discrete(labels=c('1', '2', '3', '4', '1-Reversa', '2-Reversa'))


# Pruebas ----

# NOTA; aqui voy a modificar desde excel el dataframe original para hacer más facil el analisis
  # en lugar de hacer un mutate y case_when para transformar los cuadrantes blanco en un solo valor
  # lo voy a hacer en excel
  # PENDIENTE; hacerlo con case_when y mutate
  # Haré lo mismo con todas las mediciones para tener una sola gráfica
  # Esto porque en el dataframe el cuadrante blanco para las pruebas 1 y 2 no son el mismo que para las reversa
  # Los porcentajes de cuadrante blanco vs otros también se sacaron directo en el excel
    # Otros cuadrantes se sacó el promedio mean (tiempo_cuad_1 + tiempo_cuad 2...)  

pruebas <- read.csv('datos_crudos_any_maze_para_pruebas.csv') 
colnames(pruebas)


## Estadísticos ----
pruebas_estadisticos <- pruebas |>
  group_by(Treatment, Stage) %>%
  dplyr::summarise(
    # cuadrante_blanco
    N_cuadrante_blanco  = sum(!is.na(cuadrante_blanco)),
    media_cuadrante_blanco = mean(cuadrante_blanco, na.rm=TRUE),
    sd_cuadrante_blanco = sd(cuadrante_blanco, na.rm=TRUE),
    se_cuadrante_blanco = sd_cuadrante_blanco / sqrt(N_cuadrante_blanco),
    # cuadrante_opuestos
    N_cuadrante_opuestos  = sum(!is.na(cuadrante_opuestos)),
    media_cuadrante_opuestos = mean(cuadrante_opuestos, na.rm=TRUE),
    sd_cuadrante_opuestos = sd(cuadrante_opuestos, na.rm=TRUE),
    se_cuadrante_opuestos = sd_cuadrante_opuestos / sqrt(N_cuadrante_opuestos),
    # annulus_cruces_blanco
    N_annulus_cruces_blanco  = sum(!is.na(annulus_cruces_blanco)),
    media_annulus_cruces_blanco = mean(annulus_cruces_blanco, na.rm=TRUE),
    sd_annulus_cruces_blanco = sd(annulus_cruces_blanco, na.rm=TRUE),
    se_annulus_cruces_blanco = sd_annulus_cruces_blanco / sqrt(N_annulus_cruces_blanco),
    # annulus_dist_promedio_blanco
    N_annulus_dist_promedio_blanco  = sum(!is.na(annulus_dist_promedio_blanco)),
    media_annulus_dist_promedio_blanco = mean(annulus_dist_promedio_blanco, na.rm=TRUE),
    sd_annulus_dist_promedio_blanco = sd(annulus_dist_promedio_blanco, na.rm=TRUE),
    se_annulus_dist_promedio_blanco = sd_annulus_dist_promedio_blanco / sqrt(N_annulus_dist_promedio_blanco)
  )


colnames(pruebas_estadisticos)


## porcentaje_cuadrante blanco ----


ggplot(data = pruebas_estadisticos, aes(x = Stage, y = media_cuadrante_blanco, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_cuadrante_blanco - se_cuadrante_blanco, ymax = media_cuadrante_blanco + se_cuadrante_blanco), width = .1, size = 1) +
  labs(
    title = "porcentaje en cuadrante blanco en 3 pruebas",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "número de prueba",
    y = "(%)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  scale_x_discrete(labels=c('1', '2', 'reversa'))


## porcentaje_cuadrantes opuestos ----


ggplot(data = pruebas_estadisticos, aes(x = Stage, y = media_cuadrante_opuestos, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_cuadrante_opuestos - se_cuadrante_opuestos, ymax = media_cuadrante_opuestos + se_cuadrante_opuestos), width = .1, size = 1) +
  labs(
    title = "porcentaje en cuadrante opuestos en 3 pruebas",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "número de prueba",
    y = "(%)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  scale_x_discrete(labels=c('1', '2', 'reversa'))

## numero_cruces ----

ggplot(data = pruebas_estadisticos, aes(x = Stage, y = media_annulus_cruces_blanco, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_annulus_cruces_blanco - se_annulus_cruces_blanco, ymax = media_annulus_cruces_blanco + se_annulus_cruces_blanco), width = .1, size = 1) +
  labs(
    title = "cruces",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "número de prueba",
    y = "(%)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  scale_x_discrete(labels=c('1', '2', 'reversa'))

## distancia_promedio_annulus ----

ggplot(data = pruebas_estadisticos, aes(x = Stage, y = media_annulus_dist_promedio_blanco, colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_annulus_dist_promedio_blanco - se_annulus_dist_promedio_blanco, ymax = media_annulus_dist_promedio_blanco + se_annulus_dist_promedio_blanco), width = .1, size = 1) +
  labs(
    title = "dist",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM",
    x = "número de prueba",
    y = "(%)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(1,1),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  scale_x_discrete(labels=c('1', '2', 'reversa'))



## Pendiente ; comparar con annulus antiguo

# Pasar a formato largo

pruebas <- read.csv('datos_crudos_any_maze_para_pruebas.csv') |>
  group_by(Treatment, Stage) |>
  pivot_longer(
    cols = starts_with('cuadrante'),
    names_to = 'tipo_cuadrante',
    values_to = 'porcentaje_cuadrante'
  ) |>
  pivot_longer(
    cols = starts_with('annulus_cruces'),
    names_to = 'tipo_annulus_cruces',
    values_to = 'numero_cruces'
  ) |>
  pivot_longer(
    cols = starts_with('annulus_dist'),
    names_to = 'tipo_annulus_dist',
    values_to = 'distancia_promedio_annulus')


# entropía con datos de rtrack

library(Rtrack)
rtrack <- read.csv("fluoxetina_grupo_1_resultados_crudos_totales.csv")

colnames(rtrack)

entropia_pruebas <- rtrack |>
  group_by(tratamiento, X_Day) |>
  select(X_Day, Probe, tratamiento, roaming.entropy) |>
  filter(str_detect(Probe, "TRUE"))

colnames(entropia_pruebas)

# Graficos
ggplot(data = entropia_pruebas, aes(x = X_Day, y = roaming.entropy, colour = tratamiento)) +
  geom_boxplot()+
  facet_grid(~ X_Day)

# Con estadísticos 
entropia_estadistica <- entropia_pruebas |>
  group_by(tratamiento, X_Day) %>%
  dplyr::summarise(
    N_entropia  = sum(!is.na(roaming.entropy)),
    media_entropia = mean(roaming.entropy, na.rm=TRUE),
    sd_entropia = sd(roaming.entropy, na.rm=TRUE),
    se_entropia = sd_entropia / sqrt(N_entropia)
  )

colnames(entropia_estadistica)
# gráficos con estadística



ggplot(data = entropia_estadistica, aes(x = X_Day , 
                                          y = media_entropia, 
                                          group = tratamiento,
                                          colour = tratamiento)) +
  geom_line(aes(colour = tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(
    ymin = media_entropia - se_entropia, 
    ymax = media_entropia + se_entropia), width = .1, size = 1) +
  #  Labelling as desired
  labs(
    title = "Tiempo en Movimiento",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n SEM",
    x = "Tratamiento",
    y = "Tiempo de movilidad (s)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.7, 1.2),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black")) +
    scale_x_discrete(labels=c('1', '2', 'reversa'))
#   scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))+
#   