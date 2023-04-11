library(tidyverse)

pesos_raw <- read.csv("registro_pesos_fluoxetina_grupo_1.csv")


# Con ddply

library(plyr)
  # Run the functions length, mean, and sd on the value of "change" for each group, 
  # broken down by sex + condition
  # http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
   
cdata <- ddply(pesos_raw, c("tratamiento", "sexo", "semana"), summarise,
               N    = length(peso),
               mean = mean(peso),
               sd   = sd(peso),
               se   = sd / sqrt(N)
)
cdata

  # If there are NA’s in the data, you need to pass the flag na.rm=TRUE 
  # to each of the functions. 
  # length() doesn’t take na.rm as an option, 
  # so one way to work around it is to use sum(!is.na(...)) 
  # to count how many non-NA’s there are.


cdata <- ddply(pesos_raw, c("tratamiento", "sexo", "semana"), summarise,
               N    = sum(!is.na(peso)),
               mean = mean(peso, na.rm=TRUE),
               sd   = sd(peso, na.rm=TRUE),
               se   = sd / sqrt(N)
)
cdata


ggplot(data = cdata, aes(x = semana, y = mean, colour = tratamiento)) +
  geom_line(aes(colour = tratamiento), alpha = 1, size = .5) +
  geom_point(aes(colour = tratamiento), alpha = 1, size = 4) +
  scale_color_manual(values = c("#ff1493", "#4c00ff")) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1, size = 1) +
  facet_grid(~ sexo) + 
  labs(
    title = "Peso de ratones por semana",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n Se muestra con SEM \n líneas punteadas muestran inicio y término de CUMS",
    x = "Semana",
    y = "Peso (g)"
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
    legend.position = c(1,1.3),
    legend.text = element_text(size=10),
    legend.background = element_rect(color="black")) +
  geom_vline(xintercept = 2, linetype = "dotted", color = "red", size = .6) +
  geom_vline(xintercept = 4, linetype = "dotted", color = "blue", size = .6)

    

# Exportar datos

## Prisma

cdata

datos_prisma <- cdata %>%
  pivot_wider(names_from = c(tratamiento, sexo),
              values_from = c(mean, N, sd, se))

datos_prisma

write_pzfx(datos_prisma, "pesos_fluoxetina_grupo_1_clean.pzfx", row_names = TRUE, x_col = NA)















