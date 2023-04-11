# Importar librerias a utilizar
library(tidyverse)


# Leer datos ----


datos_crudos <- read.csv(file = '21_enero_caja_2.csv', header = TRUE) 


colnames(datos_crudos)[1] <- "hora"
head(datos_crudos)


datos_crudos %>%  
  filter(!row_number() %in% c(1)) -> datos_limpios 

head(datos_limpios) 

convertir <- function(x){
  x <- as.character(x)
  x <-gsub(pattern = ":", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

datos_limpios$hora <- convertir(datos_limpios$hora)
head(datos_limpios)


str(datos_limpios) 


datos_numericos <- as.data.frame(apply(datos_limpios, 2, as.numeric)) 
sapply(datos_numericos, class)
head(datos_numericos) 


datos_por_dia <- datos_numericos %>% 
  group_by(x = cumsum(hora == 0)) %>%   # agrupa datos en función de los valores en la columna x (hora = 0)
  mutate(y_group = X) %>%               # mutate crea nuevas columnas por cada grupo de valores y los rellena con los valores de Y (registro de rueda)
  tidyr::pivot_wider(names_from = x, values_from = y_group)

head(datos_por_dia)
str(datos_por_dia)


suma_por_dia <- colSums(datos_por_dia, na.rm = TRUE)
suma_por_dia   # Comprobar valores
plot(suma_por_dia)

datos_para_graficos <- as.data.frame(suma_por_dia) # pasa a dataframe
datos_para_graficos

rownames(datos_para_graficos)
colnames(datos_para_graficos)

df_final <- datos_para_graficos

df_final_final <- 
  df_final %>%
  filter(!row_number() %in% c(1, 2))

df_final_final
rownames(df_final_final)
colnames(df_final_final)
plot(df_final_final)

dias <- c(1:8)
df_final_final$dias = dias 
df_final_final
rownames(df_final_final)
colnames(df_final_final)
plot(df_final_final)

ggplot(df_final_final,
       aes(x = dias, y = suma_por_dia)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Número de vueltas por día",
    subtitle = "Madres corredoras",
    x = "Días",
    y = "Número de vueltas a rueda"
  )


df <- read.csv(file = 'registro_por_hora.csv', header = TRUE) 

df %>% colnames[1] -> "hora"








