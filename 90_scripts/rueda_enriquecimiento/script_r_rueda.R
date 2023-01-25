# Primero, leemos los datos (cvs/excel) con el siguiente código:
# Nota, aquí los datos se encuentran en el mismo directorio del scrip, si no es así, hay que especificar el directorio

datos <- read.csv(file = 'registro_por_hora.csv', header = FALSE)
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

# Como vemos, solo tenemos 2 columnas con todos los datos de todos los días
# Primero, queremos separar en una columna cada día nuevo

# para esto, lo primero que tenemos que hacer es crear una nueva columna
# esta nueva columna la populamos con la leyenda NA
datos$nuevo_dia <- NA
head(datos)
-----------
# PENDIENTE: Crear función que cambie el formato de los horarios
  # por lo mientras, se va a hacer con VSCode
  
--------  
  
# Ahora, creamos una variable que revise si los valores (tiempo) decrecen

decrece <- FALSE

# Loop por las filas de nuestros datos

for (i in 1:nrow(datos)) {
  # Probar si el valor actual decrece
  if (i > 1 && datos$hora[i] < datos$hora[i-1]) {
    decrece <- TRUE
  }
  
  # Revisar si el valor actual decrece y si el previo no es NA
  if(decrece && !is.na(datos$hora[i-1])) {
    # movel el valor previo a una nueva columna
    datos$hora[i-1] <- datos$hora[i-1]
    # remover el valor previo de la columna original
    datos$hora[i-1] <- NA
  }
  
  # revisar si valor actual no decrece
  if (!decrece) {
    # mover valor actual a nueva columna
    datos$nuevo_dia[i] <- datos$hora[i]
    # remover valor actual de columna original
    datos$hora[i] <- NA
  }
  
  # revisar si valor actual no decrece
  if (datos$hora[i] >= datos$hora[i-1]) {
    decrece <- FALSE
  }
}


-----------
#In this example code, the sample data frame df has a single column named x containing the values 5, 6, 7, 2, 8, 9, 4, 5, 6, 7, 3, 8, 9. The code first initializes a new column named y and sets all of its values to NA. Then, it iterates through each row of the data frame and checks if the current value in x is smaller than the previous value. If it is, it sets the variable decreasing to true. If the value is not decreasing, it moves the current value to the new column y and sets the value in the original column x to NA. When the value starts increasing again, it sets the variable decreasing to false.

# This code will move all the values in x until it decreases to the new column y, and it will also remove the values that were moved to the new column from the original column x.  
------------
if(!exists("datos$hora")){
    stop("the column hora does not exist in the dataframe")
  }
for (i in 2:nrow(datos)) {
  # Probar si el valor actual decrece
  if (datos$hora[i] < datos$hora[i-1]) {
    decrece <- TRUE
  }
  # Revisar si el valor actual decrece y si el previo no es NA
  if(decrece && !is.na(datos$hora[i-1])) {
    # movel el valor previo a una nueva columna
    datos$hora[i-1] <- datos$hora[i-1]
    # remover el valor previo de la columna original
    datos$hora[i-1] <- NA
  }
  # revisar si valor actual no decrece
  if (!decrece) {
    # mover valor actual a nueva columna
    datos$nuevo_dia[i] <- datos$hora[i]
    # remover valor actual de columna original
    datos$hora[i] <- NA
  }
  # revisar si valor actual no decrece
  if (datos$hora[i] >= datos$hora[i-1]) {
    decrece <- FALSE
  }
}

    
  