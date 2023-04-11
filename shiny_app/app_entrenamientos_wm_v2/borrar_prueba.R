# Install and load dplyr
install.packages("dplyr")
library(dplyr)
library(tidyverse)

# Assuming your dataframe is called 'df'
df <- read.csv("datos_wm.csv")

# Apply formulas based on conditions and create new columns

colnames(df)

########
########
df_prueba <- df %>%
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
    ),
    cuadrantes_opuestos = case_when(
      Stage == "prueba_1" ~ (((SE_cuadrante_time + SW_cuadrante_time + NW_cuadrante_time) / 3) * 100) / 60,
      Stage == "prueba_2" ~ (((SE_cuadrante_time + SW_cuadrante_time + NW_cuadrante_time) / 3) * 100) / 60,
      Stage == "prueba_rev" ~ (((SE_cuadrante_time + NE_cuadrante_time + NW_cuadrante_time) / 3) * 100) / 60
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
  ) 

colnames(df_prueba)


ggplot(
  df_prueba,
  aes(
    x = Stage, 
    y = cuadrante_blanco,
    colour = Treatment
  )
) +
  geom_bar(stat = 'identity', position = 'dodge')

ggplot(
  data = pruebas_long, aes(
    x = Variable,
    y = Value,
    fill = Treatment
  )
) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(~ Stage)

#######

pruebas <- df %>%
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



cuadrante_blanco <- ggplot(data = pruebas, aes(
  x = Stage, 
  y = media_cuadrante_blanco, 
  colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  scale_color_manual(values = c("#ff1493", "#4c00ff", "#044400")) +
  geom_errorbar(aes(ymin = media_cuadrante_blanco - se_cuadrante_blanco, ymax = media_cuadrante_blanco + se_cuadrante_blanco), width = .1, size = 1) +
  labs(
    title = "porcentaje en cuadrante blanco en 3 pruebas",
    subtitle = "Fluoxetina Grupo 1",
    caption = "n = 7  [CUMS_ctrl] y n = 8 [CUMS_fluox] \n \n Se muestra con SEM",
    x = "número de prueba",
    y = "porcentaje (%)"
  ) +
  theme(
    plot.margin = unit(c(2,2,2,2), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(color = "#1209c8"),
    axis.title = element_text(size=15, color="black", 
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

cuadrante_blanco
############

ggplot(
  pruebas, aes(
    x = Stage,
    y = media_cuadrante_blanco,
    fill = Treatment
  )
) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_errorbar(aes(ymin = media_cuadrante_blanco - se_cuadrante_blanco, 
                    ymax = media_cuadrante_blanco + se_cuadrante_blanco), 
                size = .1, position = position_dodge(width = 0.7))

ggplot(
  pruebas, aes(
    x = Stage,
    y = media_cuadrante_blanco,
    fill = Treatment
  )
) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = media_cuadrante_blanco - se_cuadrante_blanco, 
                    ymax = media_cuadrante_blanco + se_cuadrante_blanco), 
                size = .1, width = .3, position = position_dodge(width = 0.9))

##########
ggplot(data = pruebas, aes(
  x = Stage, 
  y = media_cuadrante_blanco, 
  colour = Treatment)) +
  geom_point(aes(colour = Treatment), alpha = 1, size = 4) +
  geom_line(aes(group = Treatment)) +
  geom_errorbar(aes(ymin = media_cuadrante_blanco - se_cuadrante_blanco, 
                    ymax = media_cuadrante_blanco + se_cuadrante_blanco), 
                width = .1, size = 1) +
  geom_point(aes(
    y = media_cuadrantes_opuestos,
    shape = 
  )) +
  facet_grid(~ Treatment)

cuadrante_blanco

######

library(ggplot2)
library(tidyverse)

pruebas_long <- pruebas %>%
  gather(key = "Variable", value = "Value", media_cuadrante_blanco, media_cuadrantes_opuestos) %>%
  gather(key = "Error", value = "ErrorValue", se_cuadrante_blanco, se_cuadrantes_opuestos)


combined_plot <- ggplot(data = pruebas_long, aes(
  x = Stage, 
  y = Value, 
  colour = Treatment, 
  linetype = Variable)) +
  geom_point(aes(shape = Variable), alpha = 1, size = 4) +
  geom_line(aes(group = interaction(Treatment, Variable))) +
  geom_errorbar(aes(ymin = Value - ErrorValue, 
                    ymax = Value + ErrorValue),
                width = .1, size = 1) +
  scale_shape_manual(values = c(16, 17)) +
  labs(title = "Combined Plot",
       x = "Stage",
       y = "Value") +
  theme_minimal()

combined_plot

######################

ggplot(
  data = pruebas_long, aes(
    x = Stage,
    y = Value, 
    fill = Variable
  )
) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(~ Treatment)


####
ggplot(
  data = pruebas_long, aes(
    x = Variable,
    y = Value,
    fill = Treatment
  )
) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(~ Stage)

####


