·# Clase 3 acomodo de datos ----

## forcats ----

install.packages("forcats")
library(forcats)
library(tidyverse)

fct_lump() # colapsa los valores menos frecuentes en un factor

    # ejemplo con datos "starwars"
    
sw <- starwars  # imprime serie de datos
sw_na <- filter(sw, !is.na(species)) # filtra especies que tienen NA
sw_mutate_lump <- mutate(sw_na, species = fct_lump(species, n=3)) #  lumping together levels that meet some criteria
sw_count <- count(sw_mutate_lump, species) # cuenta especies agrupadas en el paso pasado

    # ahora vamos a correrlo en pipe
starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n=3)) %>%
  count(species)

    # 1. usando el factor, colapsa las categorías hasta que solo queden N. Amer y Europe
    # - tip fct_lump y el parámetro prop =

wphone <- WorldPhones %>%
  as.data.frame %>%
  pivot_longer(cols = 1:7, names_to = "cont", values_to = "no_phones") 


wphone <- as.factor(rep(wphone$cont, wphone$no.phones))

levels(wphone)
    # RESPUESTA
res <- fct_lump_prop(wphone, prop = 0.2)
table(res)



# Gráficos ----

## plot() ----
plot()

  # con un vector
plot <- sin(seq(from = -2*pi, to = 2*pi, by = 0.1))
plot(plot) 

  # con dos vectores


  # con un factor
    # como son categorías, no va a hacer gráfico de frecuencias/barras

  # con dataframe con un factor
  # se usa "~" que significa en función de
plot(scores ~ cases, data=df)


  # mandar un dataframe entero grafica todo contra todo, algo así
plot(airquality)



  # demo de graficas con demo(graphics)
demo(graphics)

  ### Ejercicio ----

  # utiliza datos de ChickWeight para generar el gráfico mostrado en clase

chick <- ChickWeight
head(chick)

plot(weight ~ Diet, data=chick)

## gráficos de bajo nivel ----

### points() ----
points()

plot(Animals)
points(Animals %>% #aqui metemos el codigo para que solo seleccione los puntos deseados##)

### lines () ----
lines()

lines(datos.filtrados, lty = 1 #2, 3)

plot(women)
lines(women %>% filter(height <= 60), lty = 1)
lines(women %>% filter(height >= 70), lty = 3)
lines(women %>% filter(height > 60, height < 70), lty = 2)
      
### abline () ----
abline()
abline(lm(Animals))  # predicción lineal
                    # Nota, y_lim  


### text() ----
text()
text(Animals, rownames(Animals),
     cex = scale(Animals$body/Animals$brain), #caracter de expanción es el tamaño de esta proporción
     adj = -0.1)

### legend() ----
legend()


### polygon()----
polygon()


### title() ----
title()

### axis() ----
axis()


### EJERCICIO ----

    # utiliza los datos de Indometh para generar el siguiente gráfico (un color por sujeto)

df <- Indometh
head(df)

df2 <- plot(conc ~ time, data=df)
# df3 <- lines(df2,lty=1,col=2)

lines(df$time , df$conc,
      lty=1, 
      type="l",
      col = c("green","blue", "Yellow", "Red")

lines(df2 %>% filter(conc > 1),lty = 2, col = "blue")

lines(women %>% 
        filter(height > 60, height < 70), lty = 2)

    # solucion

plot(Indometh %>% filter(Subject ==1) %>% select (time, conc))....


## multigráficos ----
par() # modifica todos los aspectos de un gráfico en una sola función

layout()


par(mfrow = c(3,1)) # controlan el número de gráficos (multiple figures per row)

## guardar gráfico ----
pdf("mi_plot.pdf") # nombre y formato
plot(data) # imprimimos el fráfico
dev.off()  # cerramos la conección

          # También se puede hacer en la GUI


## GGplot ----
library(ggplot2)

ggplot(data = df
       aes (x,y...)) +
  plot_layer() + ...


df <- Orange
head(df)

ggplot(df, 
       aes(
         x= age,
         y= circumference,
         color = Tree,
         shape = Tree
       )) +
  geom_point(
    shape = 1,
    size = 4,
    color= "red"
  ) +
  geom_smooth(
    method = lm,
    se = FALSE
  ) +
  scale_shape_manual(values = c(1:5))

  # exploremos Toothgrowth
df <- ToothGrowth

  # ver código de profe, modificó para sacar SD y media

geom_line()
geom_errorbar(aes(ymax=ml+sle...))

geom_bar()

geom_histogram()
    # fill = as.factor(law)

  # añadir escala
p + scale_color_gradient()

facet_grid()

facet_wrap()

ggsave()

## Ejercicio ----

df <- diamonds
head(diamonds)

p <- ggplot(df,
            aes(price, carat, color = color))+
  geom_point()

p + facet_grid(cols = vars(cut))
#facet_grid(. ~ cut) + xlab ("Precio (dls)"+ylab("Carats"))

## Otros paquetes gráficos ----

### plotly ----
library(plotly)
plot_ly()

### lattice ----
library(lattice)
  # HEATMAPS con lattice

### leaflet ----
library(leaflet)


### networkD3 ----
  # JAVA
  # crear redes interactivas 

### jsonlite::fromJSON ----




















































































  


















