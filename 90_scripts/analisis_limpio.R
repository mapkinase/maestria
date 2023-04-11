# Cargar librerias ----

library(tidyverse)

# Leer datos y hacer los dataframes ----

  # Aqui se importan dos dataframes
  # datos_crudos que contiene los datos directos de anymaze
  # resumen_estadistico que da los estadísticos de las variables (media, df, n, se)
    # Con este resumen vamos a hacer las gráficas con barras de errores    
datos_crudos <- read.csv("campo_abierto/resultados_raw_campo_abierto/resultados_raw_campo_abierto.csv")


resumen_estadistico <- datos_crudos |>
  group_by(Treatment, Sexo, Tratamiento) |> # agrupar por variables categóricas
  summarise(media_distancia = mean(Distance), sd_distancia = sd(Distance), n_distancia = n(), se_distancia = (sd_distancia/(sqrt(n_distancia))), # resumir estadísticos de cada variable; repetir para todas las variables deseadas
            media_tiempo_en_movimiento = mean(Time.immobile), sd_tiempo_en_movimiento = sd(Time.immobile), n_tiempo_en_movimiento = n(), se_tiempo_en_movimiento = (sd_tiempo_en_movimiento/(sqrt(n_tiempo_en_movimiento))),
            media_latencia_primera_inmovilidad = mean(Immobility.latency), sd_latencia_primera_inmovilidad = sd(Immobility.latency), n_latencia_primera_inmovilidad = n(), se_latencia_primera_inmovilidad = (sd_latencia_primera_inmovilidad/(sqrt(n_latencia_primera_inmovilidad))),
            media_velocidad_promedio = mean(Mean.speed), sd_velocidad_promedio = sd(Mean.speed), n_velocidad_promedio = n(), se_velocidad_promedio = (sd_velocidad_promedio/(sqrt(n_velocidad_promedio))),
            media_tiempo_periferia = mean(Periferia...time), sd_tiempo_periferia = sd(Periferia...time), n_tiempo_periferia = n(), se_tiempo_periferia = (sd_tiempo_periferia/(sqrt(n_tiempo_periferia))),
            media_tiempo_medio = mean(Medio...time), sd_tiempo_medio = sd(Medio...time), n_tiempo_medio = n(), se_tiempo_medio = (sd_tiempo_medio/(sqrt(n_tiempo_medio))),
            media_tiempo_centro = mean(Centro...time), sd_tiempo_centro = sd(Centro...time), n_tiempo_centro = n(), se_tiempo_centro = (sd_tiempo_centro/(sqrt(n_tiempo_centro))),
            media_cruces_al_centro = mean(Centro...Number.line.crossings), sd_cruces_al_centro = sd(Centro...Number.line.crossings), n_cruces_al_centro = n(), se_cruces_al_centro = (sd_cruces_al_centro/(sqrt(n_cruces_al_centro))),
  )


  # Con este salió mejor
  # Cuando agrupaba junto con sexo, me graficaba 4 valores (porque dividía por sexo además de tratamiento)
  # Se podría comparar sexo vs tratamiento
resumen_estadistico_v2 <- datos_crudos |>
  group_by(Treatment, Tratamiento) |> # agrupar por variables categóricas
  dplyr::summarise(media_distancia = mean(Distance), sd_distancia = sd(Distance), n_distancia = n(), se_distancia = (sd_distancia/(sqrt(n_distancia))), # resumir estadísticos de cada variable; repetir para todas las variables deseadas
            media_tiempo_en_movimiento = mean(Time.immobile), sd_tiempo_en_movimiento = sd(Time.immobile), n_tiempo_en_movimiento = n(), se_tiempo_en_movimiento = (sd_tiempo_en_movimiento/(sqrt(n_tiempo_en_movimiento))),
            media_latencia_primera_inmovilidad = mean(Immobility.latency), sd_latencia_primera_inmovilidad = sd(Immobility.latency), n_latencia_primera_inmovilidad = n(), se_latencia_primera_inmovilidad = (sd_latencia_primera_inmovilidad/(sqrt(n_latencia_primera_inmovilidad))),
            media_velocidad_promedio = mean(Mean.speed), sd_velocidad_promedio = sd(Mean.speed), n_velocidad_promedio = n(), se_velocidad_promedio = (sd_velocidad_promedio/(sqrt(n_velocidad_promedio))),
            media_tiempo_periferia = mean(Periferia...time), sd_tiempo_periferia = sd(Periferia...time), n_tiempo_periferia = n(), se_tiempo_periferia = (sd_tiempo_periferia/(sqrt(n_tiempo_periferia))),
            media_tiempo_medio = mean(Medio...time), sd_tiempo_medio = sd(Medio...time), n_tiempo_medio = n(), se_tiempo_medio = (sd_tiempo_medio/(sqrt(n_tiempo_medio))),
            media_tiempo_centro = mean(Centro...time), sd_tiempo_centro = sd(Centro...time), n_tiempo_centro = n(), se_tiempo_centro = (sd_tiempo_centro/(sqrt(n_tiempo_centro))),
            media_cruces_al_centro = mean(Centro...Number.line.crossings), sd_cruces_al_centro = sd(Centro...Number.line.crossings), n_cruces_al_centro = n(), se_cruces_al_centro = (sd_cruces_al_centro/(sqrt(n_cruces_al_centro))),
  )

  # todo/código - [ ] reusmir_estadístico en una mejor fórmula
  # 
      # Tal vez podría servir el siguiente código (https://r-graph-gallery.com/4-barplot-with-error-bar.html):
      #    # Calculates mean, sd, se and IC
# my_sum <- data %>%
#   group_by(Species) %>%
#   summarise( 
#     n=n(),
#     mean=mean(Sepal.Length),
#     sd=sd(Sepal.Length)
#   ) %>%
#   mutate( se=sd/sqrt(n))  %>%
#   mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
  
# Gráficos ----

  # Estos gráficos se hicieron con un tutorial de youtube
  # para hacer gráficos en formato APA

  # aquí vamos a trabajar con "resumen_estadistico"

  # El primer gráfico lo voy a hacer por partes para ver qué hace cada línea
  # Los otros se van a sacar de forma directa con una pipa

## Gráfico distancia ----

### Definir ejes ----
dist <- resumen_estadistico_v2 |>
  ggplot(aes(x=Tratamiento, # Aqui es CUMS vs Control 
             y=media_distancia,
             fill = reorder(Treatment, desc(Treatment)), # pre-cums vs post-cums ; los agrupa
          ))  #              ymin= media_distancia - se_distancia, ymax= media_distancia + se_distancia
dist

### geom_col ----

dist2 <- dist + geom_col(width = .5, position = position_dodge(.6), color = 'purple') # color = 'color' dibuja un contorno blanco
dist2



### geom_errorbar----

    # Ya me salen las barras, pero salen barras dobles ARREGLAR

# dist + geom_errorbar(width=.1,position = position_dodge(.6))+
dist3 <- dist2 + geom_errorbar(aes(ymin = media_distancia - se_distancia,
                         ymax = media_distancia + se_distancia), 
                      width = 0.2, 
                      position=position_dodge(width=.6))

dist3

### scale_fill_manual ----

  # Anade colores a las barras
dist4 <- dist3 + scale_fill_manual(values=c('#a60df3', '#0ef4ea')) 
dist4

### scale_y_constinous ----

dist5 <- dist4 + scale_y_continuous(expand = expansion(0), # ajusta el centro? de la grafica
                     limits = c(0,80), # ajusta la escala 
                     breaks = seq(0, 100,5)) # Ajusta la escala de y
dist5

### labs ----

dist6 <- dist5 + labs(
    x = 'Grupo Experimental',
    y = 'Distancia (m)',
    title= 'Distancia Recorrida'
  ) 

dist6

### theme ----

  # Ultimos detalles

dist6 +  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.8, 0.9),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black"))

# Gráficos V2 (Fuente: https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html) ----
# 

ggplot(resumen_estadistico, 
       aes(fill=reorder(Treatment, desc(Treatment)), 
           y=media_distancia, 
           x=as.factor(Tratamiento), 
           group=reorder(Treatment, desc(Treatment)))) + 
  geom_bar(position="dodge", stat="identity")


ggp <- ggplot(data=resumen_estadistico, aes(y = media_distancia, x = as.factor(Tratamiento), group=reorder(Treatment, desc(Treatment)))) +
  geom_bar(aes(fill=reorder(Treatment), desc(Treatment), stat="summary", fun.y=mean, position="dodge")) +
  theme_bw() + 
  facet_grid( ~ Treatment)+xlab("treatment") +ylab("Sulfide")+ggtitle("Time")

  ggp

  
###
###
  boxplot(Total.amount.of.tree.species ~ Species, xlab="Tree Species",
          ylab="Amount of Tree Species", data=TreeMean[TreeMean$area == 1, ])
  

  ## Slopegraph con newggslopegraph  
### Slopegraph
### 
### Fuente: https://r-charts.com/evolution/newggslopegraph/
### 

install.packages("CGPfunctions")
library(CGPfunctions)
  
newggslopegraph(dataframe = resumen_estadistico,
                Times = Treatment,
                Measurement = media_distancia,
                Grouping = Tratamiento)   


## slopegraph ----
  
  ## Fuente: https://ibecav.github.io/slopegraph/

require(dplyr)
require(ggplot2)
require(ggrepel)
require(kableExtra)
library(ggnewscale)


### distancia ----

ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_distancia, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_distancia - se_distancia, ymax = media_distancia + se_distancia), width = .1, size = 1) +
  scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))+
  #  Labelling as desired
  labs(
    title = "Distancia Total recorrida",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n SEM",
    x = "Tratamiento",
    y = "Distancia (m)"
  ) +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.8, 0.9),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black")) 

# + geom_signif(comparisons = list(c('pre-cums', 'postcums')), map_signif_level = TRUE)



### tiempo en movimiento media_tiempo_en_movimiento  ----

ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_tiempo_en_movimiento, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_tiempo_en_movimiento - se_tiempo_en_movimiento, ymax = media_tiempo_en_movimiento + se_tiempo_en_movimiento), width = .1, size = 1) +
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
    legend.background = element_rect(color="black")) 
#   scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))+

### latencia_primera_inmovilidad ------

ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_latencia_primera_inmovilidad, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_latencia_primera_inmovilidad - se_latencia_primera_inmovilidad, ymax = media_latencia_primera_inmovilidad + se_latencia_primera_inmovilidad), width = .1, size = 1) +
  #  Labelling as desired
  labs(
    title = "Latencia a primera inmovilidad",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox]",
    x = "Tratamiento",
    y = "Latencia a inmovilidad (s)"
  ) +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.8, 0.9),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black")) 
#   + scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))

### velocidad_promedio -----

ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_velocidad_promedio, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_velocidad_promedio - se_velocidad_promedio, ymax = media_velocidad_promedio + se_velocidad_promedio), width = .1, size = 1) +
  #  Labelling as desired
  labs(
    title = "Velocidad promedio",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n SEM",
    x = "Tratamiento",
    y = "Velocidad (m/s)"
  ) +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.8, 0.9),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black")) 
#   + scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))

### tiempo_periferia -------

ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_tiempo_periferia, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_tiempo_periferia - se_tiempo_periferia, ymax = media_tiempo_periferia + se_tiempo_periferia), width = .1, size = 1) +
  #  Labelling as desired
  labs(
    title = "Tiempo en periferia",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n SEM",
    x = "Tratamiento",
    y = "Tiempo (s)"
  ) +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.8, 1.13),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black")) 
#   + scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))
 
# tiempo_medio


ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_tiempo_medio, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_tiempo_medio - se_tiempo_medio, ymax = media_tiempo_medio + se_tiempo_medio), width = .1, size = 1) +
  #  Labelling as desired
  labs(
    title = "Tiempo en Medio",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n SEM",
    x = "Tratamiento",
    y = "Tiempo (s)"
  ) +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.8, 0.9),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black")) 
#   + scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))

### tiempo_centro -------


ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_tiempo_centro, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_tiempo_centro - se_tiempo_centro, ymax = media_tiempo_centro + se_tiempo_centro), width = .1, size = 1) +
  #  Labelling as desired
  labs(
    title = "Tiempo en Centro",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n SEM",
    x = "Tratamiento",
    y = "Tiempo (s)"
  ) +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.8, 1.12),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black")) 
#   + scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))

### cruces_al_centro --------


ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_cruces_al_centro, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_cruces_al_centro - se_cruces_al_centro, ymax = media_cruces_al_centro + se_cruces_al_centro), width = .1, size = 1) +
  #  Labelling as desired
  labs(
    title = "Cruces al centro",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n SEM",
    x = "Tratamiento",
    y = "Número de Cruces"
  ) +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=22, color="black", 
                              face="bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size=17), 
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(margin = margin(t=10)),
    axis.title.y=element_text(margin = margin(r=10)),
    legend.position = c(.8, 1.13),
    legend.text = element_text(size=20),
    legend.background = element_rect(color="black")) 
#   + scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))


# Exportar a Prisma ----

resumen_estadistico_v2

datos_prisma <- resumen_estadistico_v2 %>%
  pivot_wider(names_from = c(Tratamiento, Treatment),
              values_from = c(media_distancia, 
                              sd_distancia, 
                              n_distancia,
                              se_distancia,
                              media_tiempo_en_movimiento,
                              sd_tiempo_en_movimiento,
                              n_tiempo_en_movimiento,
                              se_tiempo_en_movimiento,
                              media_latencia_primera_inmovilidad,
                              sd_latencia_primera_inmovilidad,
                              n_latencia_primera_inmovilidad, 
                              se_latencia_primera_inmovilidad, 
                              media_velocidad_promedio,
                              sd_velocidad_promedio,
                              n_velocidad_promedio, 
                              se_velocidad_promedio,
                              media_tiempo_periferia,
                              sd_tiempo_periferia,
                              n_tiempo_periferia,
                              se_tiempo_periferia,
                              media_tiempo_medio,
                              sd_tiempo_medio,
                              n_tiempo_medio,
                              se_tiempo_medio,
                              media_tiempo_centro,
                              sd_tiempo_centro,
                              n_tiempo_centro,
                              se_tiempo_centro,
                              media_cruces_al_centro,
                              sd_cruces_al_centro,
                              n_cruces_al_centro,
                              se_cruces_al_centro))

datos_prisma

write_pzfx(datos_prisma, "campo_abierto_fluox_grupo_1.pzfx", row_names = TRUE, x_col = NA)


