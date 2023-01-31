install.packages("fortunes") # nos da la fortuna en el contesto de R
library(fortunes)
fortune()

install.packages("cowsay") # imprime un animale esclando una frase de nuestra elección
library(cowsay)
say("Hello world!")

animal_gives_fortunes <- function() {
  animal <- sample(names(animals),1)
  animal_says <- paste ("Hello, I´m a", animal, sep = " ")
  fortune_says <- paste(fortune(), collapse = "\n")
  animal_fortune <- paste(animal_says, fortune_says, sep = ": ")
  say(animal_fortune, by = animal)
}

# Hacer prueba
animal_gives_fortunes()

######## Vectores
x <- seq(from = 1, to = 50, by = 5)
y <- c(1:5)
x+y


# considera los vectores x y y
x <- c(4, 6, 5, 7, 10, 9, 4, 15)
y <- c(0, 10, 1, 8, 2, 3, 4, 1)
# qué pasa con
mean(x+y)
# 11.125
mean(c(x,y))
# 5.562
x > 7
# FALSE FALSE TRUE ...
length(x)
# 10
which(c(x,y) == 7)
# 4 # posición

sd(
  min(
    which(
      c(x,y)
      ==5))
  :max(
    which(
      c(
        x,y
      )==4
    )
  ))
# 3.8944


######## FACTORES
calificaciones <- factor(c
                        ("bajo", "alto", "medio", "alto", "alto", "bajo", "medio"))

# obtener la frecuencia de cada valor

table(calificaciones)

# añade al factor ya contruido los niveles ("muy alto" y "muy bajo")

levels(calificaciones) <- c(levels(calificaciones), "muy alto", "muy bajo")
table(calificaciones)
calificaciones

# cambiar los valores de "bajo" por "no satisfecho" -tip: levels()
# levels(calificaciones) <- list(no_satisfactorio = "bajo")
# table(calificaciones)
# calificaciones

levels(calificaciones)[2] <- "no satisfactorio"
# Aquí cambiamos el 2, no es el número 2 del vector, sino el 2 del nivle, que va por orden alfabético
calificaciones



####### Matrices
occupationalStatus
# Matriz integrada en R

# Contruid matriz con función matrix
m <- matrix(1:12, ncol = 3)
m

m[1, 3] # fila, columna

# referenciar una fila o columna completa dejando vacío
m[4, ]


# referenciar subconjunto

# Referenciar diagonal o triangulo inferior/superior
diag(m)
m[upper.tri(m, diag=F)]

# regresar una matriz a un vector
c(m)

# hacer operaciones de matrices con escalares y vectores, o entre matrices


# Matrices de más de dos dimensiones (m[1, j, k]) se llaman arreglos

## Ejercicio

# averigua las dimensiones de la matriz occupationalstatus
dim(occupationalStatus)
# 8*8

# extrae los nombres de las columnas y filas
colnames(occupationalStatus)
rownames(occupationalStatus)

# obten un subconjunto de 3*3 a partir de occupationalstatus

occupationalStatus [1:3, 1:3]

# extrae un vector el triangulo inferior de la matrix occupationalstatus
occupationalStatus[lower.tri(occupationalStatus, diag=F)]


###### Marcos de datos o dataframes
# estructuras bidemnsionales que combinan filas numericas con filas de texto
# prácticamente cualquier base de datos en excel

# construir df con data.frame()

# organismo.info <- data.frame(taxa...)

# referenciar por posición 
organismo.ingo[2]
# operador compacto
organism.info$animal
# si ponemos tabulador depspués del signo de pesos nos aparece la lista
# esto arroja un vector, lo podríamos convertir a factor y sacar media

# por nombre
organism.info["animal"]
# esto arroja otro marco de datos, no podríamos sacar factor, ni media

# funciones que ayudan
airquality

head(airquality)
tail(airquality)
str(airquality)
cbinf.data.frame(df1, df2)
rbind # row

## Ejercicio

# explorar el marco de datos USArrest y averigua su ancho y largo
USArrests
dim(USArrests)
# 50 filas 4 columnas 

# calcula estadísticos básicos para Murder, Assault, UrbanPop y Rape
# mean(), sd(), min(), max()
mean(USArrests$Murder)
sd(USArrests$Assault)
min(USArrests$UrbanPop)
max(USArrests$Rape)


# haz un vector PolicePer100kPeople con valores aleatorios y unela al final de USArrest
# tip, runif() o rnorm() ; # cuál es la diferencia
PolicePer100kPeople_runif <- c(runif(10, 100, 1000))
PolicePer100kPeople_runif

PolicePer100kPeople_rnorm <- c(rnorm(10))
PolicePer100kPeople_rnorm

combinado <- cbind.data.frame(USArrests, PolicePer100kPeople_runif)
combinado



######### Listas

# una estructura que peude contener todos los demás objetos
una.lista <- list(df = data.frame(...))

# acceder a un elemento
una.lista$df
# por su ubicación numérica 
una.lista[[1]] # equivalente a $
una.lista[1]

# agregar componentes
una.lista[[length(una.lista) + 1]] <- c...
# quitar elemtnos

# lapply()
otra.lista <- list(c(1:4), c(10:25))
lapply(otra.lista, sum)

# sapply()
sapply(otra.lista, sum)

# do.call()
do.call(c, otra.lista)
do.call(rbind, otra.lista)

### Usando el siguiente vector

x <- list (c(2,7,8), c("A", "B", "C"))
x

# reemplaza el valor de A con Z
x[[2]][1] <- "Z"
x

# agregar otro vector al final de la lista
x[[legth(x) + 1]]...

# concatena todos lo elementos en un vector de texto
do.call(c,x)


























































