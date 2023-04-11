datos_crudos <- read.csv("datos_crudos.csv")
colnames(datos_crudos)

inmobilidad_estadistica <- datos_crudos |>
  group_by(Condicion, Stage) %>%
  dplyr::summarise(
    N_inmobilidad  = sum(!is.na(Time.immobile)),
    media_inmobilidad = mean(Time.immobile, na.rm=TRUE),
    sd_inmobilidad = sd(Time.immobile, na.rm=TRUE),
    se_inmobilidad = sd_inmobilidad / sqrt(N_inmobilidad)
  )



ggplot(inmobilidad_estadistica, 
         aes(x = Condicion, y = media_inmobilidad, group = Condicion, colour = Condicion )) +
    geom_point(aes(colour = Condicion), alpha = 1, size = 4) +
    geom_line(aes(group = Condicion)) +
    facet_grid(~Stage)+
    scale_color_manual(values = c("#ff1493", "#4c00ff")) +
    geom_errorbar(aes(ymin = media_inmobilidad - se_inmobilidad, ymax = media_inmobilidad + se_inmobilidad), width = .1, size = 1) +
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
      axis.text = element_text(size = 9, color = "black"),
      axis.text.y = element_text(size=17), 
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(margin = margin(t=10)),
      axis.title.y=element_text(margin = margin(r=10)),
      legend.position = c(1,1),
      legend.text = element_text(size=10),
      legend.background = element_rect(color="black")) 
  
# Como ya había diferencia en consumo entre cums control vs cum fluox
# voy a grafica el aumento en inmovilidad de cada grupo
# voy a hacer con excel
  # PENDIETNE, hacerlo con código   

write.csv(inmobilidad_estadistica, "inmobilidad_estadistica_de_r.csv")

  



    