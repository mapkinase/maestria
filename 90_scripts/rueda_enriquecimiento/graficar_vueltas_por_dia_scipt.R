library(tidyverse)

datos <- read.csv("datos_para_graficar.csv")
str(datos)
  # Pasaro a formato de hora

library(lubridate)
datos$fecha <- ymd(datos$fecha)

  # graficar 
  # https://r-graph-gallery.com/316-possible-inputs-for-the-dygraphs-library.html
  # tranformar a xts format

colnames(datos)

ggplot(datos, aes(x = fecha, y = vueltas_por_dia)) + 
  geom_line(aes(color = Caja), size = 1) 

  