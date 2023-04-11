library(tidyverse)

df <- read.csv("resultados_raw_campo_abierto/resultados_raw_campo_abierto.csv")

head(df)
str(df)

df[!complete.cases(df), ] # Ver si tenemos datos con NA

  # Sacar SD, media, etc


  ## fuente https://www.r-bloggers.com/2014/03/using-r-quickly-calculating-summary-statistics-with-dplyr/:
library(dplyr)

df %>% 
  group_by(Tratamiento) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) -> df_test

df %>% 
  group_by(Tratamiento) %>%
  summarise(across(where(is.numeric), 
                   ~ mean(.x, na.rm = TRUE))) -> df_test


  # SD V2

sapply(df, function(df) c( "Stand dev" = sd(df), 
                         "Mean"= mean(df,na.rm=TRUE),
                         "n" = length(df),
                         "Median" = median(df),
                         "CoeffofVariation" = sd(df)/mean(df,na.rm=TRUE),
                         "Minimum" = min(df),
                         "Maximun" = max(df),
                         "Upper Quantile" = quantile(df,1),
                         "LowerQuartile" = quantile(df,0)
)
)

  # SD prueba 3
  # 

summary(df)

df %>%
  group_by(Tratamiento) %>%
  summary(df)


  # SD prueba 4, creo la buena jeje

  # e
###############
limpiar_datos <- df |>
  group_by(Treatment, Tratamiento) |> # agrupar por variables categóricas
  summarise(media_distancia = mean(Distance), sd_distancia = sd(Distance), n_distancia = n(), se_distancia = (sd_distancia/(sqrt(n_distancia))), # resumir estadísticos de cada variable; repetir para todas las variables deseadas
            media_tiempo_en_movimiento = mean(Time.immobile), sd_tiempo_en_movimiento = sd(Time.immobile), n_tiempo_en_movimiento = n(), se_tiempo_en_movimiento = (sd_tiempo_en_movimiento/(sqrt(n_tiempo_en_movimiento))),
            media_latencia_primera_inmovilidad = mean(Immobility.latency), sd_latencia_primera_inmovilidad = sd(Immobility.latency), n_latencia_primera_inmovilidad = n(), se_latencia_primera_inmovilidad = (sd_latencia_primera_inmovilidad/(sqrt(n_latencia_primera_inmovilidad))),
            media_velocidad_promedio = mean(Mean.speed), sd_velocidad_promedio = sd(Mean.speed), n_velocidad_promedio = n(), se_velocidad_promedio = (sd_velocidad_promedio/(sqrt(n_velocidad_promedio))),
            media_tiempo_periferia = mean(Periferia...time), sd_tiempo_periferia = sd(Periferia...time), n_tiempo_periferia = n(), se_tiempo_periferia = (sd_tiempo_periferia/(sqrt(n_tiempo_periferia))),
            media_tiempo_medio = mean(Medio...time), sd_tiempo_medio = sd(Medio...time), n_tiempo_medio = n(), se_tiempo_medio = (sd_tiempo_medio/(sqrt(n_tiempo_medio))),
            media_tiempo_centro = mean(Centro...time), sd_tiempo_centro = sd(Centro...time), n_tiempo_centro = n(), se_tiempo_centro = (sd_tiempo_centro/(sqrt(n_tiempo_centro))),
            media_cruces_al_centro = mean(Centro...Number.line.crossings), sd_cruces_al_centro = sd(Centro...Number.line.crossings), n_cruces_al_centro = n(), se_cruces_al_centro = (sd_cruces_al_centro/(sqrt(n_cruces_al_centro))),
            ) 
view(limpiar_datos)

############
distancia <- limpiar_datos |> 
  ggplot(aes(
    x=Tratamiento,
    y=media_distancia
  ))

distancia

distancia <- geom_col()
############
############ Graficó, pero está poniendo 3 barras de errores
############ convendría que además se agrupe (facet?)  

grafico_distancia <- limpiar_datos |>
  ggplot(aes(x = Tratamiento, y = media_distancia, color = Tratamiento)) +
  geom_bar(stat = 'identity', color='black', position=position_dodge())+
  geom_errorbar(aes(ymin = media_distancia - se_distancia,
                    ymax = media_distancia + se_distancia), width = 0.2) +
  labs(x = "Tratamiento", y = "distancia_media") +
  theme_classic()
grafico_distancia


############  Este graficó mejor
############  falta agrupar y que se vean bonitas las barras
############  Creo que las barras de error no están bien

ggplot(limpiar_datos, aes(x=Tratamiento, y=media_distancia)) +
  stat_summary(fun = mean, geom = "bar",
               width=0.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color="red", width=0.5)

############
############ Salió bonito el gráfico
############ faltaría agruparlo y poner errores
library(dplyr) 
library(ggplot2)
library(ggpubr)
theme_set(theme_pubclean())

# Filter and count the number of records by groups

df2 <- limpiar_datos %>%
  group_by(Treatment, Tratamiento, media_distancia) %>%
  summarise(counts=n())
head(df2,4)

# Create the grouped bar plots
## Ver mas en: http://www.sthda.com/english/articles/32-r-graphics-essentials/132-plot-grouped-data-box-plot-bar-plot-and-more/

# Stacked bar plots of y = counts by x = cut,
# colored by the variable color

ggplot(df2, aes(x=Tratamiento, y=media_distancia)) +
  geom_bar(
    aes(color = Tratamiento, fill = Tratamiento),
    stat = "identity", position = position_stack()
  ) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))
# Use position = position_dodge() 
p <- ggplot(df, aes(x = cut, y = counts)) +
  geom_bar(
    aes(color = color, fill = color),
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))
p



############
############ Salierom m,uy bien estas gráficas
############ Solo que no estoy seguro si me está graficando los datos bien
############ además no tuve que hacer tidy de los datos originales
############ ahorra tiempo

da<-df

theme_set(
  theme_bw()
)

str(da)

# convert variable from a numeric to a discrete factor variable
#da$Stage<-as.factor(da$Stage)

# Default plot
e <- ggplot(da, aes(x = Tratamiento, y = Immobility.latency))
e + geom_boxplot()
# Notched box plot with mean points
e + geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",
               shape = 18, size = 2.5, color = "#FC4E07")


f <- ggplot(da, aes(x = Tratamiento, y = Time.immobile))
f + geom_boxplot()
# Notched box plot with mean points
f + geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",
               shape = 18, size = 2.5, color = "#FC4E07")


## Use faceting

library(ggplot2)
p <- ggplot(data = da, aes(x = Tratamiento, y = , Periferia...time, group = Treatment, fill = Treatment))
p <- p + geom_bar(data=subset(da, Treatment=='postcums'), stat = "identity", width = 0.5, position = "dodge")
p
p <- p + facet_grid(. ~ Tratamiento)
p
p <- p + theme_bw()
p
p <- p + theme(axis.text.x = element_text(angle = 90))
p <- p+ labs(title= 'Porcentaje en Zona Deseada vs Opuesta', x='Prueba 1', y='Tiempo(%)')
p <- p+theme(text=element_text(size = 20))
p


############
############ Salieron muchas barras de error, pero se ven normal las barras de erro
############

## ADD SIGNIFICANE

library(ggpubr)
library(rstatix)
library(dplyr)
library(ggplot2)
library(ggsignif)


df_limpio <- limpiar_datos
View(df_limpio)  # aquí se trabajo con el dataframe que se hizo más arriba
  
ggplot(limpiar_datos, aes(x=Tratamiento, y=media_distancia, color=Tratamiento)) +
  geom_bar(stat = 'identity', color='black', position=position_dodge())+
  geom_errorbar(aes(ymin=media_distancia-se_distancia, ymax=media_distancia+se_distancia), width=.1, size=1)+
  labs(title= 'Porcentaje en Zona Deseada vs Zona opuesta', x='Prueba 1', y='Tiempo (%)')+
  theme(text = element_text(size = 20)) 





############

#geom_signif(comparisons = list(c("Control", "Ketamina"), map_signif_level = TRUE)+

view(da)

ggplot(da, aes(x=Tratamiento, y=Distance)) +
  stat_summary(fun = mean, geom = "bar",
  ) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color="red", width=0.5)

ggplot(data=da, aes(x=Tratamiento, y=Distance))+
  geom_smooth(se=F,data=subset(da, Tratamiento=='CUMS_ctrl'))+ # subset(datoss,Tratamiento=='Ketamina')
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color="red", width=0.5)

ggplot(data=da, 
       aes(Treatment,Distance & Tratamiento=='CUMS_ctrl', 
           color=Tratamiento))+
  geom_smooth(se=F)


write.csv(cleandata_st, "cleaddata_st_.csv")

############
############
############ SALIO MUY BIEN :) :)
############
############

# Bar graph / bar chart APA -------------------

grafico_apa <- df_limpio

grafico_apa %>%
  ggplot(aes(x=Tratamiento, 
             y=media_distancia,
             fill = Sexo,
             ymin= media_distancia - se_distancia,
             ymax= media_distancia + se_distancia))+
  geom_col(width = .5, position = position_dodge(.6),
           color = 'black')+
  geom_errorbar(width=.1,position = position_dodge(.6))+
  scale_fill_manual(values=c('#a60df3', '#0ef4ea'))+
  scale_y_continuous(expand = expansion(0), 
                     limits = c(0,40), 
                     breaks = seq(0, 40,5))+
  labs(
    x = 'Tratamiento',
    y = 'Tiempo en zona(%)',
    title= 'Prueba 1'
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


#ymin = media_distancia - se_distancia,
#ymax = media_distancia + se_distancia
############
############

grafico_apa %>%
  ggplot(aes(x=Tratamiento, 
             y=media_tiempo_en_movimiento,
             fill = Sexo,
             ymin=media_tiempo_en_movimiento - se_tiempo_en_movimiento,
             ymax=media_tiempo_en_movimiento - se_tiempo_en_movimiento))+
  geom_col(width = .5, 
           position = position_dodge(.6),
           color = 'black')+
  geom_errorbar(width=.1,position = position_dodge(.6))+
  scale_fill_manual(values=c('#FF00FF', '#00FFFF'))+
  scale_y_continuous(expand = expansion(0), 
                     limits = c(0,40), 
                     breaks = seq(0, 40,5))+
  labs(
    x = 'Tratamiento',
    y = 'Tiempo en zona(%)',
    title= 'Prueba 1'
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
    legend.background = element_rect(color="black")
  )+geom_signif(comparisons = list(c('Deseado', 'Opuesto')),
                map_signif_level = TRUE)


############


############


# estructura básica para graficar una variable simple pre vs post
ggplot(
  df,
  aes(x = reorder(Treatment, desc(Treatment)), y = Periferia...time, fill = Treatment)
) +
  geom_col() +
  facet_wrap(~ Tratamiento) +
  
  
  
  
  ggplot(
    df,
    aes(x = Tratamiento, y = Medio...time)
  ) +
  geom_col() +
  facet_wrap(~ Treatment)



ggplot(
  df,
  aes(x = Tratamiento, y = Centro...time)
) +
  geom_col() +
  facet_wrap(~ Treatment)




ggplot(
  df,
  aes(x = Tratamiento, y = Centro...Number.line.crossings)
) +
  geom_col() +
  facet_wrap(~ Treatment)




ggplot(
  df,
  aes(x = Tratamiento, y = Centro...Number.line.crossings)
) +
  geom_col() +
  facet_wrap(~ Treatment)




ggplot(
  df,
  aes(x = Tratamiento, y = Distance)
) +
  geom_col() +
  facet_wrap(~ Treatment)




ggplot(
  df,
  aes(x = Tratamiento, y = Mean.speed)
) +
  geom_col() +
  facet_wrap(~ Treatment)




ggplot(
  df,
  aes(x = Tratamiento, y = )
) +
  geom_col() +
  facet_wrap(~ Treatment)















