## instalar paquetes para la clase

install.packages(c("readxl", "tibble","tidyr","dplyr","ncyflights13","reshape2","magrittr ", "forcats","tidyverse"))

# como tuve unos errores instalé individualmente algunos de estos
install.packages("readxl") 
install.packages("tibble")
install.packages("ncyflights13")
install.packages("reshape2")
install.packages("magrittr") 
install.packages("forcats")


#### Moch data

sample()



## Cargar y leer datos
read.table(
  file=".csv",
  header=TRUE,
  sep=","
)

write.table()

## readxl
read_excel(), read_xls(), read_xlsx()

install.packages("readxl")
library(readxl)
datos <- ..

## tibble

tibble::as_tibble(head(mtcars, n=3))


### Ejercicio

# crear archivo de csv y guardarlo como .csv
# carga archivo con read.table()

df <- read.table(file="2_datos_files/registro_por_hora.csv")
head(df)

# modifica el marco de datos en R


# guarda el archivo con write.table()
write.table(df, file="2_datos_files/registro_por_hora_ejercicio.csv")


#######3 Areo de datos



#### Ejercico con booleanos

# Utilizando marco chickwts seleccionar un subconjunto utilizando lógicos
df <- chickwts
head(df)

# pesos de los pollos que comieron sunflower
sunf <- df$weight[df$feed=="sunflower"]

head(sunf)
sunf

# peso de todos excepto de los que comieron meatmeal 
no_meat <- df$weight[df$feed !="meatmeal"]
head(no_meat)

# pesos mayores de 200 pero menores de 300

pesos_200_300 <- df$weight[df$weight > 200 & df$weight < 300]
pesos_200_300


########### tidyr

# Paso de formatos acho y largo
pivot_longer()y pivot_wider()

library(tidyr)
dt <- table4b
# en esta tabla, los datos no están acomodados

tidy <- pivot_longer(data=dt, cols = c("1999", "2000"), names_to = "years", values_to = "population")
tidy




## separate separar datos
separate()

sep <- table3

tid.sep <- separate(sep, rate, )


## unite ()
un <- table5
un

tid.un <- unite(un, "year", 2:3, sep = "")
tid.un



#### extract ()


######## Ejercicio
WorldPhones
# usa pivot_longer() para covertir WorldPhones en un df de tres columnas: continente, no_phones y year
library(tidyr)
library(magrittr)

wphone <- as.data.frame(WorldPhones) %>%
  tibble::rownames_to_column(var="Year")

wphone

tidy <- pivot_longer(data=dt, cols = c("1999", "2000"), names_to = "years", values_to = "population")


wphone_longer <- 
  pivot_longer(
  data=wphone,
  c("N.Amer", "Europe", "Asia", "S.Amer", "Oceania", "Africa", "Mid.Amer"),
  # cols = N.Amer:Mid.Amer,
  names_to = "continente", 
  values_to ="no_phones")

wphone_longer


## Separa las fechas y une tiempos de weird.times.df

weird.times.df <- data.frame(
  date=as.Date("2017-02-03"),
  hour=sample(1:24, 15),
  min=sample(1:60, 15),
  second=sample(1:60,15)
)

tiempos_ejercicio <- weird.times.df
tiempos_ejercicio


separate(tiempos_ejercicio,
         col=date,
         into=c(
           "year",
           "month",
           "day"),
         sep="-"
       )

unite(tiempos_ejercicio,
      col=tiempo,
      c("hour", "min", "second"),
      sep=":"
      )



##### dplyr
library(dplyr)
# join: inner_join, left_join, right_join, full_join, semi_join, anti_join,
inner_join(data1, data2, by="ID")

# ejemplos
# cargar ncyflights13 que tiene una gran cantidad de datos
library(tidyverse)
install.packages("ncyflights13")
library(ncyflights13)
ncyflights13


f1 <- filter(flights,
             month == 12,
             day == 29)
head(f1)


slice(flights, 1:10) # cortar primeras 10 filas de flights

f1 <- arrange(flights, month, day, dep_delay)
arrange(flights, desc(arr_delay))

select() # seleccionar columnas


distinct() # columnas donde encontramos filas unicas
distinct(select(flights, tailnum))

mutate()
mutate(flights, gain=arr_delay - deep_delay,
       speed=gain/air_time*60)
select(df,gain,speed)


transmute()

group_by(), ungroup() # aqui usamos variables de factores
group_by(flights, tailnum)


summarise()
summarise(df, n=n(),
          mean_delay=mean(
            deep_delay, na.rm = TRUE
          ))

sample_n(), sample_frac()

n(), n_distinct(), first(), last(), nth(x, n)

######## Ejercicio 

# cargar tips
install.packages("reshape2")

# OJO, cargar datos sin cargar el paquete
data(tips, package = "reshape2")
head(tips)
tail(tips)


#### EJERCICIO
library(dplyr)
# filtra las filas para obtener solo a los no fumadores

filtrado <- filter(tips, smoker == "No")
filtrado

# re arregla el df según la propina en orden descendente
propina <- arrange(tips, desc(tip))
head(propina)
tail(propina)

# selecciona todas las columnas menos la de fumadores

col_sel <- select -...


# muta una nueva columna para conocer el consumo por persona (total de la cuenta entre num de comensales)

mutado <- mutate(tips, consumo_persona = total_bill/size)
head(mutado)
  

# resume el numero de sujetos y la propina (promedio y sd) por día de la semana, hora y sexo

resumen <- tips

resumen %>%
  group_by(time, sex, day) %>%
  summarise(media_propina = mean(tip), standard_dev_propina = sd(tip))
# tal vez me funcionaría mejor agrupar al último

resumen
  


############### wranlgling arreo de datos con magrittr

# ejercicio, reconstruir con pipas el siguiente código que se uso en el ejercicio pasado
df1 <- filter(tips, smoker == "No")
df2 <- arrange (df1, tip)
df3 <- select(df2, -smoker)
df4 <- mutate(df3, bill_per_person = total_bill/size)
df5 <- group_by(df4, sex, day, time)
df6 <- summarize (df5, mtip = mean (tip), sdtip = sd(tip))

# con pipas
df <- tips

df %>% 
  filter(smoker == "No") %>%
  arrange(tip) %>%
  select(-smoker) %>%
  mutate(bill_per_person = total_bill/size) %>%
  group_by(sex, day, time) %>%
  summarize(mtip = mean (tip), sdtip = sd(tip)) -> df_final
  # %>% write.csv(...)

head(df_final)


























































































































