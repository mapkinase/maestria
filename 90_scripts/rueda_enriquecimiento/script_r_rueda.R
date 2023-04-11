# Importar librerias a utilizar
library(tidyverse)
# library(tidyr)

# Leer datos ----

datos_crudos <- read.csv(file = 'registro_por_hora.csv', header = TRUE) # Lee datos
            # Nota, aquí los datos se encuentran en el mismo directorio del scrip
            # si no es así, hay que especificar el directorio

head(datos_crudos) # corroborar que los datos importados están en orden
            # Como vemos, la primera columna tiene un nombre muy largo
            # Vamos a cambiar el nombre
  
## limpiar nombre de columnas y filas ----

colnames(datos_crudos)[1] # revisar columna a cambiar
colnames(datos_crudos)[1] <- "hora" # cambiar nombre raro por hora
head(datos_crudos) # Revisar

            # ahora tenemos que quitar la primera fila ya que no son datos
    
datos_crudos %>%  filter(!row_number() %in% c(1)) -> datos_limpios # eliminar primera fila
head(datos_limpios) # revisar nuevos datos

            # cambiar ":" por "." en "hora" para facilitar cálculos
            # esto se hace con la siguiente función

convertir <- function(x){
  x <- as.character(x)
  x <-gsub(pattern = ":", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}


datos_limpios$hora <-convertir(datos_limpios$hora) # aplica función convertir en columna hora
head(datos_limpios)

        # aunque los datos ya están limpios, R todavía no los reconoce como números

str(datos_limpios) # si corremos este código, vemos que la columna X se reconoce como un caracter
                  # esto nos impide hacer análisis con números
                  # vamos a convertirlo a valores numéricos


datos_numericos <- as.data.frame(apply(datos_limpios, 2, as.numeric)) # Convertir
sapply(datos_numericos, class)  # Print classes of all colums
                  # como vemos, ahora todos son numéricos
            
                  # antes de seguir con el análisis, vamos a corroborar que nuestros datos se vean bien

head(datos_numericos) # se ven bien!
                      # en la siguiente parte vamos a hacer que nos separe por día

# Separar por día ----

datos_por_dia <- datos_numericos %>% 
  group_by(x = cumsum(hora == 0)) %>%   # agrupa datos en función de los valores en la columna x (hora = 0)
  mutate(y_group = X) %>%               # mutate crea nuevas columnas por cada grupo de valores y los rellena con los valores de Y (registro de rueda)
  tidyr::pivot_wider(names_from = x, values_from = y_group)

head(datos_por_dia)
str(datos_por_dia)
                                        # Ya está separado por día!

write.csv(datos_por_dia, "exortado_datos_por_dia.csv", row.names=TRUE) # Podemos exportar ya los datos en un archivo
            # NOTA; el archivo original produjo una suma total de datos de 51803
            # PENDIENTE: comprobar que este archivo exportado tenga los mismos datos


        
# Suma de valores por días ----

 

suma_por_dia <- colSums(datos_por_dia, na.rm = TRUE)
suma_por_dia   # Comprobar valores
              # haciendo la suma manual de los 7 días + x me dio un valor de 103,296
              # haciendo la suma de los 7 días me dio un valor de 51,803
              # En el cvs original dio un valor de 51,803
              # parece que está bien si usamos los valores de las columnas 0 a 7


# últimos detalles ----

              # hay que convertir de numérico a dataframe
suma_por_dia
print(class(suma_por_dia)) # Como vemos, suma_por_dia es de clase numérica
                           # Hay que pasarlo a dataframe para graficarlo  

              # esto ya se puede graficar con lo siguiente  
plot(suma_por_dia)
              # Sin embargo, vemos que todavía tiene los datos de hora y X que no queremos
              # además, hay que pasarlo a dataframe para graficarlo con GGplot
                





datos_para_graficos <- as.data.frame(suma_por_dia) # pasa a dataframe

print(class(datos_para_graficos)) # ahora si tenemos un dataframe

datos_para_graficos # ya tenemos los datos, pero ahora hay que pasarlo a formato largo para graficarlo
    # aunque ya es dataframe, tiene 2 columnas que no queremos y un formato largo que tenemos que pasar a ancho            
    
    # PENDIENTE: exportar as.data.frame ya con buen formato

datos_final <- pivot_wider(datos_para_graficos,  # pasar a formato wider
                           names_from = suma_por_dia, 
                           values_from = suma_por_dia)
datos_final # vemos que ya está en el formato que queremos
            # sin embargo, no queremos las primeras 2 columnas
            # PENDIENTE: darle nombre a las columnas con el nombre del día

datos_final_v2 <- datos_final[,-1:-2] # elimina columnas por rango; de 1 a 2
datos_final_v2 # Ahora si ya tenemos los datos listos para graficar



# Graficar la suma por día ----

library(ggplot2)
    # Nombres de columnas y filas para graficar
rownames(datos_final_v2)
colnames(datos_final_v2)

ggplot(data=datos_final_v2, 
       aes(x= )

ggplot(data = df_grafico, 
       mapping = aes(x="3153","6611","8605","11145","7895","5980","6352","2062", y = "1")) +
  geom_col()
  
    ## CREO que no tenemos que pasar a formado wider, voy a intentarlo con largo
datos_para_graficos

rownames(datos_para_graficos)
colnames(datos_para_graficos)

df_final <- datos_para_graficos

df_final_final <- df_final %>%  
  filter(!row_number() %in% c(1, 2))

df_final_final
rownames(df_final_final)
colnames(df_final_final)

ggplot(df_final_final,
       aes(x,y) +
         geom_point())

plot(df_final_final$suma_por_dia)










                     
library(dplyr)
df <- read.csv(file = 'registro_por_hora.csv', header = TRUE)

# Revisar que los datos sean int
str(df)
# como la primera columna es num "df[1], se va a convertir a int
df$hora = as.integer(as.numeric(df$hora))
# Comprobar que sea int
str(df)
##NOTA, creo que funciónó mejor con i


df <- df %>% 
  group_by(x = cumsum(hora == 0)) %>% 
  mutate(y_group = dia1) %>%
  tidyr::pivot_wider(names_from = x, values_from = y_group)

head(df)


write.csv(df, "exortado_prueba.csv", row.names=TRUE)


# This code will first group the data by the values in column X using cumsum(X==0).
# This will create a new column with a group number that will increase each time a 0 is found in the X column.
# Then, it will use the mutate function to create a new column named "y_group" and fill it with the values from Y
# Finally, it will use the pivot_wider function from the tidyr library to create new columns for each group and fill them with the corresponding values from the "y_group" column.
# The new columns will be named based on the group number.
# You can also rename the new columns using rename_at function from dplyr to something meaningful
# such as rename_at(vars(matches("x_")), funs(paste0("group_", .)))

# Please note that this code assumes that the values in column X are integers
# and that the column name is X and Y, you should change that accordingly if your column names are different.


# CORRER CON NUMERIC en lugar de INTEGERS


# the previous code can be modified to work with numeric values in column X
# One way to group the data by a range of values in column X is to use the cut function

library(dplyr)

df <- df %>% 
  mutate(x_group = cut(hora,breaks = c(-Inf,0,Inf))) %>% 
  mutate(y_group = dia1) %>%
  tidyr::pivot_wider(names_from = x_group, values_from = y_group)

head(df)



# This code will first use the cut function to create a new column called "x_group" that will group the values in column X into two groups: one group for values less than 0 and one group for values greater or equal than 0. Then, it will use the mutate function to create a new column named "y_group" and fill it with the values from Y. Finally, it will use the pivot_wider function from the tidyr library to create new columns for each group and fill them with the corresponding values from the "y_group" column. The new columns will be named based on the group "x_group".
# You can also rename the new columns using rename_at function from dplyr to something meaningful such as rename_at(vars(matches("x_group_")), funs(paste0("group_", .)))
# Please note that this code assumes that the values in column X are numeric and that the column name is X and Y, you should change that accordingly if your column names are different.


######### Tercera prueba
datos <- read.csv(file = 'registro_por_hora.csv', header = TRUE)
head(datos)
tail(datos)
str(datos)

#cambiar nombre de columnas a mejor formato

# revisar columna a cambiar
colnames(datos)[1]
# cambiar nombre
colnames(datos)[1] <- "hora"
# Revisar
head(datos)

# eliminar primera fila
datos %>%  filter(!row_number() %in% c(1)) -> datos2
head(datos2)

library(tidyr)
library(tidyverse)

datos3 <- pivot_longer(
  data=datos2,
  cols = c("hora"),
  names_to = ""
  
)


head(datos3)

## prueba con codigo de 
df <- datos2
head(df)
## cambiar : por . en código
convertir <- function(x){
  x <- as.character(x)
  x <-gsub(pattern = ":", replacement = ".",x = x, fixed = TRUE)
  x <- as.numeric(x)
  return(x)
}

df$hora <-convertir(df$hora)
head(df)

str(df)

#convertir a numerico
df_num <- as.data.frame(apply(df, 2, as.numeric))
sapply(df_num, class)                                # Print classes of all colums
head(df_num)

df_sort <- df_num %>% 
  group_by(x = cumsum(hora == 0)) %>% 
  mutate(y_group = X) %>%
  tidyr::pivot_wider(names_from = x, values_from = y_group)

head(df_sort)
str(df_sort)

write.csv(df_sort, "exortado_prueba_2.csv", row.names=TRUE)         

### ya está separado por días, ahora vamos a hacer la suma 

df_suma <- colSums(df_sort, na.rm = TRUE)
head(df_suma)

## nos convirtió a valor numérico, hay que regresarlo a dataframe

print(class(df_suma))

df_grafico <- as.data.frame(df_suma)

print(class(df_grafico))
df_grafico
head(df_grafico)
## ahora vamos a graficar la suma
library(ggplot2)

ggplot(data = df_grafico, 
       mapping = aes(x="", y = df_suma)) +
  geom_col()





  
  
    
  