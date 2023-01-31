# Primero, leemos los datos (cvs/excel) con el siguiente código:
# Nota, aquí los datos se encuentran en el mismo directorio del scrip, si no es así, hay que especificar el directorio

datos <- read.csv(file = 'registro_por_hora.csv', header = TRUE)
# RESULTADO: datos <- read.csv(file = 'registro_por_hora.csv')

# Para corroborar que los datos están en orden, usar el siguiente comando para ver los datos importados
head(datos)

#cambiar nombre de columnas a mejor formato

# revisar columna a cambiar
colnames(datos)[1]
# cambiar nombre
colnames(datos)[1] <- "hora"
# Revisar
head(datos)

# PENDIENTE: Crear función que cambie el formato de los horarios
  # por lo mientras, se va a hacer con VSCode
  
library(dplyr)

df <- read.csv(file = 'registro_por_hora.csv', header = TRUE)

  
df <- df %>% 
  group_by(day = as.Date(time, format = "%Y-%m-%d")) %>% 
  mutate(day_one = ifelse(time == "00:00", NA, day_one)) %>%
  tidyr::pivot_wider(names_from = day, values_from = day_one)

head(df)



# You can use the dplyr library in R to group the data by the values in column X
# then use the mutate function to create new columns for each group of values and fill them with the corresponding values from the "Y" column



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





  
  
    
  