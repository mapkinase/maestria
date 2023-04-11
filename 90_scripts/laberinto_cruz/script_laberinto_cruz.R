library(tidyverse)

datos_crudos <- read.csv("laberinto_cruz_fluox_g1.csv")
colnames(datos_crudos)

ggplot(datos_crudos, 
       aes(x = Tratamiento, y = Distance, group = Tratamiento, colour = Tratamiento )) +
  geom_violin()+
  facet_grid(~Treatment)


ggplot(datos_crudos, 
       aes(x = Tratamiento, y = Brazo.abierto...time, group = Tratamiento, colour = Tratamiento )) +
  geom_violin()+
  facet_grid(~Treatment)

ggplot(datos_crudos, 
       aes(x = Tratamiento, y = Brazo.cerrado...time, group = Tratamiento, colour = Tratamiento )) +
  geom_violin()+
  facet_grid(~Treatment)

ggplot(datos_crudos, 
       aes(x = Tratamiento, y = Centro....time, group = Tratamiento, colour = Tratamiento )) +
  geom_violin()+
  facet_grid(~Treatment)

####


ggplot(data = resumen_estadistico_v2, aes(x = reorder(Treatment, desc(Treatment)), y = media_tiempo_en_movimiento, group = Tratamiento,colour = Tratamiento)) +
  geom_line(aes(colour = Tratamiento), alpha = 1, size = 2) +
  geom_point(aes(colour = Tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = media_tiempo_en_movimiento - se_tiempo_en_movimiento, ymax = media_tiempo_en_movimiento + se_tiempo_en_movimiento), width = .1, size = 1) +
  #  Labelling as desired
  labs(
    title = "Tiempo en Movimiento",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox]",
    x = "Tratamiento",
    y = "Tiempo de movilidad (s)"
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
#   scale_y_continuous(expand = expansion(0), limits = c(30, 70), breaks = seq(30,70,5))+
