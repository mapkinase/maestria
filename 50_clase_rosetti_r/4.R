# Scripts ----

# Funciones ----


    ## estructura básica ----
nombre <- function(arg.1, arg.2, arg.n){
  contenido # dentro de las llaves
  return(resultado)
}

    ## llamar función ----
resultado <- nombre(arg.1, arg.2, arg.n)

    ## ejemplo ----
suma <- function(a,b){
  c = a + b
  return(c)
}

suma(2,2)

    ## ejercicio ----
      
      # escribe una función para calcular el (1) volumen de un cilindro
      # y (2) el volumen de un cono

volumen_cilindro <- function(r, h){
  v = pi * r^2 * h
  return(v)
}

volumen_cilindro(5,19)


volumen_cono <- function(r, h) {
  v = (1/3 * pi) * r^2 * h
  return(v)
}

volumen_cono(5, 19)


# Control condicional ----

  # Condiciona la ejecución de cierto código a una condición lógica

## if y else ----

a <- 1

if (a > 0){
  print("el valor es mayor a 0")
}


if (a > 0) {
  print("el valor es mayor a 0")
} else {
  print("el valor es menor a 0")
}

## else if -----


if (a > 0) {
  print("el valor es mayor a 0")
} else if 


## comparar escalar con vector ----
a <- 1
b <- c(1:5)
a == b

## comprobar si un escalar está presente entre ----
a %in% b  # si a pertenece a b


## comprobación entre vectores con all() ----

all(v1 == v2)
all(length(v4) == length(v5)) & all(v4 == v5)

## identical -----
identical(v1, v2)

## otros comparadores lógicos ----
union(x,y)
intersect(x,y)
setdiff(x,y)
setequal(x,y)
duplicated(x)
unique(x)

## ejercicio --------

  # crear función que imprima un vector si este tiene una longitud mayor de 3
  # tip length()

vector_1 <- c(1:10)
vector_2 <- c(1:2)


longitud_mayor_3 <-function(vec){
  if (length(vec) > 3) {  # evalúa si longitud es mayor a 3
    print("el vector tiene una longitud mayor a 3")
  } else {
    print("el vector tiene una longitud menor a 3")
  }
  #return(vec)
}
  
longitud_mayor_3(vector_1)  
  




  # crea función que imprima si número es par o impar ()
  # tip   round()

par_impar <- function(numero){
  numero <- round(numero, digits = 0)  # primero redondea el número si es decimal
  if((numero %% 2) == 0) {  # ahora divide en 0 para revisar si es par o impar
    print("el número es par")
  } else {
    print ("el número es impar")
  }
  #return(numero)
}

par_impar(0)

  # crea función que diga si lo que se ingresa es un text o un número
  # tip is.numeric(), is.character()

texto_o_numero <- function(valor){
  if (is.numeric(valor) == TRUE) {
    print("es un valor numérico")
  } else if (is.character(valor) == TRUE) {
    print("el valor es texto")
  }
}

texto_o_numero(2222)


# Control iterativo ----

## for() ----

  # for ejecuta código un numero determinado de veces


for (i in inicio:fin){
  # operación a realizar
}

  # ejemplo
for (i in 1:10) {
  print(i)
}

  # ejemplo con vector
abc <- letters[1:10]
for (i in abc) {
  print(i)
}

  # con length
for (i in 1:length(abc)) {
  print(i)
}


## while() ----

  # mientras se cumpla una condición
  # después de cada iteración revisa la condición lógica
  # puede quedarse en un loop eterno

i <- 0

while (i < 10) {
  i <- i + 1
  print(letters[i])
}


## ejercicio iterativo ----

  # escribe un loop que imprima todas las letras del abecedario
  # excepto las vocales, las cuales deben de ser mayúscular
  # tip   letters   toupper()   %in%    intersect(x,y)

abc <- letters

for (i in abc) {
  print(i)
}


vocales <- c("a", "e", "i", "o", "u")

intersect <- intersect(abc, vocales)
intersect

toupper(intersect)


funcion_abc <- function(alphabet) {
  intersect_2 <- intersect(alphabet, vocales)
  toupper(intersect_2)
  return(intersect_2)
  for (i in intersect_2) {
    print(i)
  }
} 


funcion_abc <- function(alphabet) {
  for (i in alphabet) {
    if (i %in% alfabeto_normal) {
      print(i)
    } else if (i %in% alfabeto_vocales)
    print(i)
  }
  toupper(intersect(alphabet, vocales))

}

funcion_abc <- function(alphabet) {
  alfabeto_vocales <- toupper(intersect(alphabet, vocales))
  alfabeto_normal <- abc - alfabeto_vocales
  for (i in alphabet) {
    if (i %in alfabeto_normal) {
      print(i)
    } else if (i %in% alfabeto_vocales) {
      print(i)
    }
  }
}

funcion_abc(abc)

    # VER RESPUESTA

# Control de flujo vectorizado ----

  # alternativa moderna para el control de flujo
  # encaja con tidyverse


## ifelse en mutate ----

mutate(df, columna_nueva = ifelse (calificacion > 60, "paso", "reprobo"))

  # EJERCICIO

  # usa ifelse en mutate en el df quakes para crear una nueva columna "concern" con tres calificaciones de magnitud (mag) de temblor
  # "imperceptible" que incluye temblores de 4 a 4.5
  # "leve" que incluye temblores de 4.5 a 5.5
  # alerta que incluye temblores de 5.5 a 6

  # grafica las coordenadas de todos los temblores (lat y long)
  # ilustra con puntos rojos los temblores que merecen alerta
library(tidyverse)
library(dplyr)

df <- quakes
head(df)
min(df$mag)
max(df$mag)

      # este código no funcionó
mutate(df, concern = ifelse (mag > 3 & mag < 4.5, "imperceptible",
                             ifelse (mag > 4.5 & mag < 5.5, "leve",
                             ifelse (mag > 5.6 & mag < 6, "alerta"))))

      # este si funcionó

mutate(df, concern = ifelse (mag > 4.5, "leve", "impercetible")) %>%
         mutate(df, concern = ifelse (mag > 5.6, "alerta", concern))


      # VER SOLUCIÓN de los otros ejercicios





      # across()
      # evaluar condición lógica en múltiples columnas
mtcars %>%
  group_by(cyl) %>% 
  summarise(across(where(is.numeric), mean))


    ## EJERCICIO ----

    # usa across y starwars para
      # resumir la media y sd de todas las variables numéricas agrupadas por homeworld 
        # tip is.character, mean, sd y parametro na.rm=T

starwars %>%
  group_by(homeworld) %>%
  summarise(across(where(is.numeric), mean, na.rm=T))

            
      # calcular todos los valores unicos de las variables de texto
        # tip is.character, n_distinct

starwars %>%
  summarise(across(where(is.character), n_distinct))



## case_when -----
x <- 1:50

case_when(
  abs((x/5) - round(x/5)) == 0 ~ "multiplo de 5",
  abs((x/2) - round(x/2)) == 0 ~ "multiplo de 2",
  abs((x/3) - round(x/3)) == 0 ~ "multiplo de 3",
  TRUE ~ as.character (x)  # dice algo asi como es verdad que ninguna condición se cumplió  
                           # si lo pusieramos hasta arriba cambia el código
                           # si no lo pusiéramos, nos pondría NA                                                           
)


## ejercicio de flujo vectorizado --------

  # con starwars 
    # con select selecciona las variables name, height, mass, gender, species
    # con mutate, usa case_when para crear una nueva columna type en la que crees las siguientes categorías
      # large para los height o mass mayores a 200
      # robot para las species que sean Droid
      # other para los demás
    # tip   se pueden hacer referencias a mas de una columna en el case_when

starwars %>%
  select(name, height, mass, gender, species) %>%
  mutate(case_when((height > 200 || mass > 200) ~ "large")) %>%
  mutate(case_when((species == "Droid") ~ "robot")) %>%
  mutate(case_when((species != "Droid") ~ "other"))

      # CHATGTP lo hizo más eficiente
starwars %>%
  select(name, height, mass, gender, species) %>%
  mutate(size = case_when((height > 200 || mass > 200) ~ "large"),
         type = case_when(species == "Droid" ~ "robot",
                          TRUE ~ "other"))


      # REVISAR CÓDIGO RESPUESTA
























