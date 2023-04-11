library(tidyverse)
# Operadores lógicos ----
# 
x == y 	# equivalencia
x != y 	# diferencia
!x 	# negación lógica
x & y 	# y
x | y 	# o 


# Mock data ----
rbeta() 
rbinom()
rchisq()
rexp()
rgamma()
rgeom()
rlogis()
rnbinom()
rpois()
runif()
rweibull()

## ejemplos ----

x <- runif(10, min = 0, max = 10)
x

x <- rnorm(10, mean = 10, sd = 0.5)
x  

## sample - elegir un número de la serie ----
x <- sample(1:10, 1)
x
x <- sample(1:10, 5, replace=T) # con repeticiónes
x <- rpois(10, lambda = 5)

# Leer archivos de datos ----
datos <- read.table(file = "archivo_con_mis_datos.csv", 
                    header = TRUE, 
                    sep = ",")

install.packages("readxl")
library(readxl)
datos <- read_excel("nombre_del_archivo")

tibble::as_tibble(head(mtcars, n=3))

# Data wrangling ----

## pivot_longer() ----

## pivot_wider() ----


## Separate () dividir variables ----


## unite () unir variables ----

## extract() dividir variable usando múltiples criterios -> multiples columnas


# Data Wringling con dplyr ----

## join()----

inner_join(data1, data2, by = "ID")
left_join(data1, data2, by = "ID")
right_join(data1, data2, by = "ID")
full_join(data1, data2, by = "ID")
semi_join(data1, data2, by = "ID")
anti_join(data1, data2, by = "ID")

## filter() ---- filtrar filas usando booleanos
f1 <- filter(flights, month == 12, day == 29)

## slice() ----
slice(flights, 1:10)

## arrange() orden de las filas ----
f1 <- arrange(flights, month, day, dep_delay)
arrange(flights, desc(arr_delay))

## select() seleccionar columnas ----
f1 <- select(flights, year, month, day)
select(flights, year:day) # desde year hasta day
select(flights, -(year,day)) # todo menos desde year y day
select(flights, año = year) # podemos cambiar el nombre de la columna al seleccionarla

## distinct() extraer filas únicas ----
f1 <- distinct(select(flights, tailnum))
f2 <- distinct(select(flights, origin, dest))

## mutate() crea nuevas columans ----
f1 <- mutate(flights, 
             gain = arr_delay - dep_delay,
             speed = gain/air_time * 60)

## transmute() ----
transmute(flights, 
          gain = arr_delay - dep_delay,
          gain_per_hour = gain/(air_time/60))

## group_by/ungroup() ----
f.g <- group_by(flights, tailnum)

## summarise() resume las columnas según las variables agrupadas ----
f.g <- group_by(flights, tailnum)
f.sum <- summarise(f.g, 
                   n = n(), 
                   mean_delay = 
                     mean(dep_delay, na.rm = TRUE))

## sample_n() y sample_frac() -----
sample_n(flights, 10) # muestrea 10 elementos
# intenta tambien sample_frac(flights, 0.01) para muestrear el 1 %

## n() y n_distinct() ----
destinations <- group_by(flights, dest)
summarise(destinations, planes = n_distinct(tailnum), flights = n())

## first(), last() nth(x, n) ----

# forcats acomodo de datos ----

## fct_infreq() reordena un factor por frecuencia ----
starwars %>%
  mutate(eye_color = fct_infreq(eye_color)) %>% 
  group_by(eye_color) %>% 
  summarize(n = n())

## # A tibble: 15 x 2
##    eye_color         n
##    <fct>         <int>
##  1 brown            21
##  2 blue             19
##  3 yellow           11
##  4 black            10

## fct_lump() los valores menos frecuentes ----

starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>%
  count(species)

## # A tibble: 4 x 2
##   species     n
##   <fct>   <int>
## 1 Droid       6
## 2 Gungan      3
## 3 Human      35
## 4 Other      39

## fct_relevel() reordena los niveles de los factores ----

# Gráficos ----

## plot () ----

### un vector ----
res <- sin(seq(from = -2*pi, to = 2*pi, by = 0.1))
plot(res)

### dos vectores() ----
data(women); 
plot(women$height, women$weight)

### un factor ----
res <- as.factor(month.name[round(runif(120, min = 0, max = 12))])
plot(res)

### data frame con un factor ----

df <- data.frame(scores = round(runif(120, min = 0, max = 12)),
                 cases = as.factor(rep(letters[1:4], 30)))
plot(scores ~ cases, data = df)

### un data frame entero
plot(airquality)

## points() ----

data(Animals, package = "MASS")
plot(Animals)
points(Animals %>% tibble::rownames_to_column() %>% 
         filter(rowname == "African elephant") %>% select(body, brain), 
       pch = 8, col = "red", cex = 2)

## lines() ----
plot(women)
lines(women %>% filter(height <= 60), lty = 1)
lines(women %>% filter(height >= 70), lty = 3)
lines(women %>% filter(height > 60, height < 70), lty = 2)

### graficar varias líneas a través del tiempo ----
plot(Indometh %>% filter(Subject == 1) %>% select(time, conc), type = "l")
lines(Indometh %>% filter(Subject == 2) %>% select(time, conc), col = "green")
lines(Indometh %>% filter(Subject == 3) %>% select(time, conc), col = "blue")
lines(Indometh %>% filter(Subject == 4) %>% select(time, conc), col = "red")
lines(Indometh %>% filter(Subject == 5) %>% select(time, conc), col = "cyan")
lines(Indometh %>% filter(Subject == 6) %>% select(time, conc), col = "orange")

## abline() agrega una predicción lineal a un gráfico ----
plot(Animals)
text(Animals + Animals * 0.025, rownames(Animals), cex = 0.5, adj = -0.1)
abline(lm(Animals))

### abline para SD, media, etc ----
plot(Animals, ylim = c(-1000, 6000))
abline(h = mean(Animals$brain))
abline(h = mean(Animals$brain) + sd(Animals$brain), lty = 3)
abline(h = mean(Animals$brain) - sd(Animals$brain), lty = 3)

## text() ----
plot(Animals, col = "white", log = c("xy"))
text(Animals, rownames(Animals), 
     cex = scale(Animals$body/Animals$brain), adj = -0.1)

## legend() ----
## polygon() ----
## title() ----
## axis() ----

# Multigráficos ----

## par() y mfrow= multiple figures per row ----
par(mfrow = c(3,1))

h <- c("Hello", "World", "!")

plot(0, 0, col = "white", xlab = "", ylab = "")

text(0, 0, h[1], cex = 2)

plot(0, 0, col = "white", xlab = "", ylab = "")

text(0, 0, h[2], cex = 2)

plot(0, 0, col = "white", xlab = "", ylab = "")

text(0, 0, h[3], cex = 2)

# ggplot() ----

## geom_point()---- 
ggplot(Orange, aes(x = age, y = circumference, color = Tree)) + 
  geom_point(shape = 1, size = 4)
  
  # con scale_shape_manual
ggplot(Orange, aes(x = age, y = circumference, shape = Tree)) + 
  geom_point() + scale_shape_manual(values = c(1:5))

## geom_smooth() ----
ggplot(Orange, aes(x = age, y = circumference, color = Tree)) + 
  geom_point() + geom_smooth(method = lm, se = FALSE)

## geom_line() -----
ggplot(data = TG, aes(x = dose, y = ml, group = supp, color = supp)) +
  geom_line() + geom_point() + 
  geom_errorbar(aes(ymax = ml + sle, ymin = ml - sle), width = 0)

ggplot(data = TG, aes(x = dose, y = ml, group = supp, color = supp)) + 
  geom_line() + geom_errorbar(aes(ymax = ml+sle, ymin = ml-sle), width = 0) +
  scale_color_manual(values = c("green", "orange"))

## geom_bar() ----
ggplot(data = mPG, aes(x = group, y = mw, fill = group)) + 
  geom_bar(stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = mw+sdw, ymin = mw-sdw), width = 0.25)

## geom_histogram() ----
ggplot(SB.df, aes(x = DriversKilled)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "white")

ggplot(SB.df, aes(x = DriversKilled, fill = as.factor(law))) +
  geom_histogram(binwidth = 10, alpha = .5, position = "identity")

## scale_color_gradient() añadir escala ----
p <- p + scale_color_gradient(low = "red", high = "yellow", "Efecto",
                              with(OrchardSprays, c(min(decrease),mean(decrease),max(decrease))),
                              labels = c("Alto","Medio","Bajo"))
p


## facet_grid ----
  # horizontal
sp + facet_grid(sex ~ .) 
  # vertical
sp + facet_grid(. ~ sex)
  # por ambas variables
sp + facet_grid(sex ~ day)

## facet_wrap() Graficos múltiples varias columnas----
sp + facet_wrap(~ day, ncol = 2) 

## ggsave() ----
ggsave(myplot, file = "myplot.pdf", width = 4, height = 4)

# Otros paquetes para graficar ----

## plotply ----
library(plotly)
plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)

## lattice () ----
library(lattice)
xyplot(lat ~ long | cut(depth, 2), data = quakes, outer = T, grid = T)

### lattice heatmap ----
library(lattice)
heatmap(matrix(DNase$density, ncol = 11))

## leaflet() -----
library(leaflet)
leaflet() %>% addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")

## networkD3 ----
library(networkD3)
src <- c("A", "A", "A", "A", "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J", "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)
simpleNetwork(networkData)

Energy <- jsonlite::fromJSON(
  "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json")
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source", 
              Target = "target", Value = "value", NodeID = "name", units = "TWh", 
              fontSize = 12, nodeWidth = 30)

# Funciones ----

## estructura ----
nombre <- function(arg.1, arg.2, arg.n) {
  contenido # va dentro de las llaves
  return(resultado)
}

Llamado

resultado <- nombre(arg.1, arg.2, arg.n) # argumentos van entre los parentesis

## Ejemplo ----
y <- m * x + b 

recta <- function(m, x, b) {
  y <- m * x + b
  return(y)
}
y <- recta(m = 1, x = 1:10, b = 3)
plot(y)


# Control de flujo vectorizado ----

## ifelse en mutate ----

clase <- c("Estadistica", "Algebra", "Programacion en R")
calificacion <- round(runif(9, 20, 100),2)
estudiante <- rep(c("David", "Jose", "Laura"),3)
historial <- data.frame(clase, calificacion, estudiante)
mutate(historial, paso.o.reprobo = ifelse(calificacion > 60, "paso", "reprobo"))
##               clase calificacion estudiante paso.o.reprobo
## 1       Estadistica        44.13      David        reprobo
## 2           Algebra        39.56       Jose        reprobo
## 3 Programacion en R        97.64      Laura           paso
## 4       Estadistica        65.01      David           paso
## 5           Algebra        93.26       Jose           paso

### ejercicio

  # Usa ifelse en mutate y el dataframe quakes para crear una columna nueva concern con tres calificaciones de magnitud (mag) de temblor:
  # imperceptible, que incluye temblores de 4 a 4.5
  # leve, que incluye temblores de 4.5 a 5.5
  # alerta, que incluye temblores de 5.5 a 6
  # Grafica las coordenadas de todos los temblores (lat y long)
  # Ilustra con puntos rojos los temblores que merecen alerta

q2 <- quakes %>% 
  mutate(concern = ifelse(mag > 4.5, "leve", "imperceptible")) %>%
  mutate(concern = ifelse(mag > 5.5, "alerta", concern)) 
head(q2)

##      lat   long depth mag stations       concern
## 1 -20.42 181.62   562 4.8       41          leve
## 2 -20.62 181.03   650 4.2       15 imperceptible
## 3 -26.00 184.10    42 5.4       43          leve
## 4 -17.97 181.66   626 4.1       19 imperceptible
## 5 -20.42 181.96   649 4.0       11 imperceptible
## 6 -19.68 184.31   195 4.0       12 imperceptible

q2 %>% 
  group_by(concern) %>% 
  tally()
## # A tibble: 3 x 2
##   concern           n
##   <chr>         <int>
## 1 alerta           24
## 2 imperceptible   484
## 3 leve            492

ggplot(q2, aes(x=lat,y=long)) + geom_point() + 
  geom_point(data = q2 %>% 
               filter(concern == "alerta"), aes(x=lat,y=long), color = "red")


## across() evaluar una condición logica en multiples columnas ----

mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(where(is.numeric), mean))

## # A tibble: 3 x 11
##     cyl   mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     4  26.7  105.  82.6  4.07  2.29  19.1 0.909 0.727  4.09  1.55
## 2     6  19.7  183. 122.   3.59  3.12  18.0 0.571 0.429  3.86  3.43
## 3     8  15.1  353. 209.   3.23  4.00  16.8 0     0.143  3.29  3.5

### ejercicio ----

  # Usa across y starwars para:
  # resumir todas la media y sd de todas las variables numericas agrupadas por homeworld
  # tip: is.character, mean, sd y el parámetro na.rm=T
  # calcular todos los valores unicios de las variables de texto
  # tip: is.character, n_distinct

starwars %>% 
  group_by(homeworld) %>% 
  filter(n() > 1) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

## # A tibble: 10 x 4
##    homeworld height  mass birth_year
##    <chr>      <dbl> <dbl>      <dbl>
##  1 Alderaan    176.  64         43  
##  2 Corellia    175   78.5       25  
##  3 Coruscant   174.  50         91  
##  4 Kamino      208.  83.1       31.5
##  5 Kashyyyk    231  124        200  
##  6 Mirial      168   53.1       49  
##  7 Naboo       175.  64.2       55  
##  8 Ryloth      179   55         48  
##  9 Tatooine    170.  85.4       54.6
## 10 <NA>        139.  82        334.

starwars %>% 
  summarise(across(where(is.character), n_distinct))

## # A tibble: 1 x 8
##    name hair_color skin_color eye_color   sex gender homeworld species
##   <int>      <int>      <int>     <int> <int>  <int>     <int>   <int>
## 1    87         13         31        15     5      3        49      38


## Case when() ----

  # Podemos elegir un valor para cada categoria numerica con case_when

x <- 1:50
case_when(
  abs((x/5) - round(x/5)) == 0 ~ "m de 5",
  abs((x/2) - round(x/2)) == 0 ~ "m de 2",
  abs((x/3) - round(x/3)) == 0 ~ "m de 3",
  TRUE ~ as.character(x)
)

##  [1] "1"      "m de 2" "m de 3" "m de 2" "m de 5" "m de 2" "7"      "m de 2"
##  [9] "m de 3" "m de 5" "11"     "m de 2" "13"     "m de 2" "m de 5" "m de 2"
## [17] "17"     "m de 2" "19"     "m de 5" "m de 3" "m de 2" "23"     "m de 2"
## [25] "m de 5" "m de 2" "m de 3" "m de 2" "29"     "m de 5" "31"     "m de 2"
## [33] "m de 3" "m de 2" "m de 5" "m de 2" "37"     "m de 2" "m de 3" "m de 5"
## [41] "41"     "m de 2" "43"     "m de 2" "m de 5" "m de 2" "47"     "m de 2"
## [49] "49"     "m de 5"


### Ejercicio ----

  # Con starwars

  # con select, selecciona las variables name, height, mass, gender, species
  # Con mutate usa case_when para crear una nueva columna type en la que crees las siguentes categorias…
  # large para los height o mass mayores de 200
  # robot para las species que sean Droid
  # y ponle other a todos los demas
  # tip: se pueden hacer referencias a mas de una columna en el case_when
s1 <- starwars %>%
  select(name:mass, gender, species) %>%
  mutate(
    type = case_when(
      height > 200 | mass > 200 ~ "large",
      species == "Droid"        ~ "robot",
      TRUE                      ~ "other"
    )
  )
## # A tibble: 6 x 6
##   name           height  mass gender    species type 
##   <chr>           <int> <dbl> <chr>     <chr>   <chr>
## 1 Luke Skywalker    172    77 masculine Human   other
## 2 C-3PO             167    75 masculine Droid   robot
## 3 R2-D2              96    32 masculine Droid   robot
## 4 Darth Vader       202   136 masculine Human   large
## 5 Leia Organa       150    49 feminine  Human   other
## 6 Owen Lars         178   120 masculine Human   other


# Programación funcional

  # mapear funciones sobre bases de datos previamente agregados en base a alguna variable categórica,
  # aumentando la eficiencia con la que aplicamos funciones y obtenemos resultados de bases con al menos una estructura repetitiva

## purrr() ----
install.packages("tidyverse")
install.packages("purrr")

library(purrr)
library(tidyr)
library(dplyr) 
library(broom)

### nest() y unnest() -----
head(mtcars)

##                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
## Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
## Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

n_mtcars <- mtcars %>% 
  nest(-cyl) # produce un df de listas
n_mtcars

## # A tibble: 3 x 2
##     cyl data              
##   <dbl> <list>            
## 1     6 <tibble [7 x 10]> 
## 2     4 <tibble [11 x 10]>
## 3     8 <tibble [14 x 10]>

## map() ----
my_test <- function(x) {
  lm(mpg ~ wt, data=x)
}

mtcars %>% 
  nest(-cyl) %>% 
  mutate(res = map(data,my_test)) %>%
  mutate(glance_lm = res %>% map(glance))

## # A tibble: 3 x 4
##     cyl data               res    glance_lm        
##   <dbl> <list>             <list> <list>           
## 1     6 <tibble [7 x 10]>  <lm>   <tibble [1 x 12]>
## 2     4 <tibble [11 x 10]> <lm>   <tibble [1 x 12]>
## 3     8 <tibble [14 x 10]> <lm>   <tibble [1 x 12]>


  # otra versión

mtcars %>%
  split(.$cyl) # de R base

## $`4`
##                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## Datsun 710     22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
## Merc 240D      24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
## Merc 230       22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
## Fiat 128       32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## Honda Civic    30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
## Toyota Corolla 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
## Toyota Corona  21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary)

## $`4`
## 
## Call:
## lm(formula = mpg ~ wt, data = .)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.1513 -1.9795 -0.6272  1.9299  5.2523 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   39.571      4.347   9.104 7.77e-06 ***
## wt            -5.647      1.850  -3.052   0.0137 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.332 on 9 degrees of freedom
## Multiple R-squared:  0.5086, Adjusted R-squared:  0.454 

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map("r.squared")

## $`4`
## [1] 0.5086326
## 
## $`6`
## [1] 0.4645102
## 
## $`8`
## [1] 0.4229655

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

##         4         6         8 
## 0.5086326 0.4645102 0.4229655


mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_df("r.squared")

## # A tibble: 1 x 3
##     `4`   `6`   `8`
##   <dbl> <dbl> <dbl>
## 1 0.509 0.465 0.423


## ejercicio relacion lineal y valor de p ----

  # Usando data(txhousing, package="ggplot2"), 
  # describe, mediante un modelo, 
  # la relación lineal entre sales y listings 
  # para cada categoria de year y extrae el valor de p
  # tips: lm(sales ~ listings)



  # Solución (con split):
  
  data(txhousing, package="ggplot2")
res <- txhousing %>%
  split(.$year) %>%
  map(~lm(sales ~ listings, data = .)) %>%
  map(summary) %>%
  sapply(.,coefficients) %>% 
  as.data.frame() %>% slice(8) %>% 
  pivot_longer(cols = '2000':'2015', names_to = "year", values_to = "p.value")
head(res)

## # A tibble: 6 x 2
##   year    p.value
##   <chr>     <dbl>
## 1 2000  5.60e-263
## 2 2001  1.20e-283
## 3 2002  8.01e-281
## 4 2003  8.22e-309
## 5 2004  5.96e-300
## 6 2005  2.71e-238



  # Solución (con nest):
  
  data(txhousing, package="ggplot2")
test_lm <-function(x){
  lm(sales~listings, data=x)
} 
res <- txhousing %>%
  nest(-year) %>%
  mutate(st=map(data, test_lm)) %>%
  mutate(glance_lm = st %>% map(glance)) %>%
  unnest(glance_lm) %>% 
  select(year, p.value)
head(res)

## # A tibble: 6 x 2
##    year   p.value
##   <int>     <dbl>
## 1  2000 5.60e-263
## 2  2001 1.20e-283
## 3  2002 8.01e-281
## 4  2003 8.22e-309
## 5  2004 5.96e-300
## 6  2005 2.71e-238


# lubridate() Fechas ----

library(lubridate)
date1 <- c("12-01-99") # dia, mes, año
dmy(date1)

## [1] "1999-01-12"

date2 <- c("01/12/99") # mes, dia, año
mdy(date2)

## [1] "1999-01-12"

date3 <- c("January 12, 1999") 
mdy(date3)

## [1] "1999-01-12"



  # Registrar horas como horas con lubridate

library(lubridate)
time1 <- c("15_30_30")
time1 <- hms(time1) # hora min segundo
hour(time1)

## [1] 15

minute(time1)

## [1] 30

as.duration(time1)

## [1] "55830s (~15.51 hours)"


# quitar daltos faltantes ----

naq <- airquality[complete.cases(airquality), ] # solucion paquete base

naq <- na.omit(airquality) # otra alternativa

naq <- airquality %>% 
  filter(complete.cases(Ozone, Solar.R, Wind, Temp, Month, Day))  # solucion tidyverse

naq <-airquality %>% # solucion tidyverse
  na.omit()

naq <- airquality %>% 
  drop_na() # solucion tidyverse

naq <- airquality %>% drop_na() # solucion con dplyr 
head(naq)

##   Ozone Solar.R Wind Temp Month Day
## 1    41     190  7.4   67     5   1
## 2    36     118  8.0   72     5   2
## 3    12     149 12.6   74     5   3
## 4    18     313 11.5   62     5   4

# Imputar datos ----

naq <- airquality

naq$Ozone[which(is.na(naq$Ozone))] <- mean(naq$Ozone, na.rm = TRUE) # intenta median

# soluciones con dplyr 

naq %>% 
  mutate(Ozone = ifelse(is.na(Ozone), mean(Ozone, na.rm=TRUE), Ozone))

##         Ozone Solar.R Wind Temp Month Day
## 1    41.00000     190  7.4   67     5   1
## 2    36.00000     118  8.0   72     5   2
## 3    12.00000     149 12.6   74     5   3
## 4    18.00000     313 11.5   62     5   4
## 5    42.12931      NA 14.3   56     5   5
## 6    28.00000      NA 14.9   66     5   6
## 7    23.00000     299  8.6   65     5   7


ibrary(Hmisc)
naq <- airquality
naq$Ozone <- impute(naq$Ozone, fun = mean) # con impute()
head(naq)

##      Ozone Solar.R Wind Temp Month Day
## 1 41.00000     190  7.4   67     5   1
## 2 36.00000     118  8.0   72     5   2
## 3 12.00000     149 12.6   74     5   3
## 4 18.00000     313 11.5   62     5   4
## 5 42.12931      NA 14.3   56     5   5
## 6 28.00000      NA 14.9   66     5   6

naq %>% 
  mutate(Ozone = Hmisc::impute(Ozone, fun = mean)) # solucion con dplyr

##         Ozone Solar.R Wind Temp Month Day
## 1    41.00000     190  7.4   67     5   1
## 2    36.00000     118  8.0   72     5   2
## 3    12.00000     149 12.6   74     5   3
## 4    18.00000     313 11.5   62     5   4


# Datos extremos; detección y capping -------


x <- c(-30, 1:10, 20, 30)

boxplot(x)

boxplot.stats(x)$out

## [1] -30  20  30

boxplot.stats(x, coef = 2)$out # un criterio mas amplio

## [1] -30  30


x <- c(-30, 1:10, 20, 30)
qnt <- quantile(x, probs=c(.25, .75), na.rm = T) # cuantiles
caps <- quantile(x, probs=c(.15, .85), na.rm = T) # elige tis "caps"
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1] # remplaza outliers por max caps 
x[x > (qnt[2] + H)] <- caps[2] # remplaza outliers por min caps 
x

##  [1]  1.8  1.0  2.0  3.0  4.0  5.0  6.0  7.0  8.0  9.0 10.0 12.0 12.0

boxplot(x)

# Scale() estandarización y escalamiento -----

x <- c(1, 2, 3)
scale(x, center = T)[1:3]

## [1] -1  0  1

(x - mean(x)) / sd(x) # manual

## [1] -1  0  1

scale(x, center = F)[1:3]

## [1] 0.3779645 0.7559289 1.1338934

x / sqrt(sum(x^2) / (length(x) - 1)) # manual

## [1] 0.3779645 0.7559289 1.1338934


# Estadística descriptiva() -----


## Tendencia central ----

# media
mean(mtcars$mpg) # na.rm = T si hay valores faltantes

## [1] 20.09062

# mediana
median(mtcars$mpg)

## [1] 19.2

# moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(mtcars$mpg)

## [1] 21



## Medidas de dispersión ----

# desviacion estandar
sd(mtcars$mpg)

## [1] 6.026948

# quantiles
quantile(mtcars$mpg)

##     0%    25%    50%    75%   100% 
## 10.400 15.425 19.200 22.800 33.900

# rango interquartil
IQR(mtcars$mpg)

## [1] 7.375

# rango total
range(mtcars$mpg)

## [1] 10.4 33.9


## Tamaños de muestra ----

length(mtcars$mpg) # numero de observaciones

## [1] 32

length(mtcars) #Si df, num de variables (columnas)

## [1] 11

dim(mtcars) # Solo para matirz o df, numero de observaciones x no de variables

## [1] 32 11

## dplyr::group_by y dplyr::summarize ----

mtcars %>% 
  group_by(cyl) %>% 
  summarise(n = n(), mean_mpg = mean(mpg), sd_mpg = sd(mpg))

## # A tibble: 3 x 4
##     cyl     n mean_mpg sd_mpg
##   <dbl> <int>    <dbl>  <dbl>
## 1     4    11     26.7   4.51
## 2     6     7     19.7   1.45
## 3     8    14     15.1   2.56


### dplyr::summarize_all -----

library(dplyr)
library(magrittr)

mtcars %>% 
  group_by(cyl) %>% 
  summarise_all(list( ~ mean(.)))

## # A tibble: 3 x 11
##     cyl   mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     4  26.7  105.  82.6  4.07  2.29  19.1 0.909 0.727  4.09  1.55
## 2     6  19.7  183. 122.   3.59  3.12  18.0 0.571 0.429  3.86  3.43
## 3     8  15.1  353. 209.   3.23  4.00  16.8 0     0.143  3.29  3.5


### dplyr::summarize y dplyr::across ------

library(dplyr)
library(magrittr)

mtcars %>% group_by(cyl) %>%
  summarise(across(where(is.numeric), mean))

## # A tibble: 3 x 11
##     cyl   mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     4  26.7  105.  82.6  4.07  2.29  19.1 0.909 0.727  4.09  1.55
## 2     6  19.7  183. 122.   3.59  3.12  18.0 0.571 0.429  3.86  3.43
## 3     8  15.1  353. 209.   3.23  4.00  16.8 0     0.143  3.29  3.5



# Estadística inferencial -----

## wilcox ----

wilcox.test() # dos muestras, independientes o pareadas, datos no paramétricos

?ToothGrowth
# The Effect of Vitamin C on Tooth Growth in Guinea Pigs
head(ToothGrowth)

levels(ToothGrowth$supp)

## [1] "OJ" "VC"

wilcox.test(len ~ supp, data = ToothGrowth) # paired = T si son pareadas

## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  len by supp
## W = 575.5, p-value = 0.06449
## alternative hypothesis: true location shift is not equal to 0



## kruskal ----
kruskal.test() # tres o más muestras independientes, datos no paramétricos

?PlantGrowth
# Dried weight of plants obtained under a control 
# and two different treatment conditions.
head(PlantGrowth)

levels(PlantGrowth$group)

## [1] "ctrl" "trt1" "trt2"

kruskal.test(weight ~ group, data = PlantGrowth)

## 
##  Kruskal-Wallis rank sum test
## 
## data:  weight by group
## Kruskal-Wallis chi-squared = 7.9882, df = 2, p-value = 0.01842


### post hocs de kruskal.test() ----

# posthocs con pairwise.wilcox.test
pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group, 
                     p.adjust.method = "BH")

## 
##  Pairwise comparisons using Wilcoxon rank sum test with continuity correction 
## 
## data:  PlantGrowth$weight and PlantGrowth$group 
## 
##      ctrl  trt1 
## trt1 0.199 -    
## trt2 0.095 0.027
## 
## P value adjustment method: BH

## friedman ----
friedman.test() # tres o más muestras repetidas, datos no paramétricos

data("selfesteem", package = "datarium")
?selfesteem
# Self-Esteem Score Data
head(selfesteem)

# operador barra | marca el elemento que identifica las repeticiones
friedman.test(score ~ time | id, data = selfesteem %>% 
                tidyr::pivot_longer(cols = -id, 
                                    names_to = "time", values_to = "score"))

## 
##  Friedman rank sum test
## 
## data:  score and time and id
## Friedman chi-squared = 18.2, df = 2, p-value = 0.0001117


## Correlación ----

cor() # entre dos vectores

?airquality
# New York Air Quality Measurements
head(airquality)

cor(airquality$Ozone, airquality$Wind, use = "complete.obs")

## [1] -0.6015465



cor() # de una matriz o df

cor(airquality, use = "complete.obs")

##                Ozone     Solar.R        Wind       Temp        Month
## Ozone    1.000000000  0.34834169 -0.61249658  0.6985414  0.142885168
## Solar.R  0.348341693  1.00000000 -0.12718345  0.2940876 -0.074066683
## Wind    -0.612496576 -0.12718345  1.00000000 -0.4971897 -0.194495804
## Temp     0.698541410  0.29408764 -0.49718972  1.0000000  0.403971709
## Month    0.142885168 -0.07406668 -0.19449580  0.4039717  1.000000000
## Day     -0.005189769 -0.05775380  0.04987102 -0.0965458 -0.009001079
##                  Day
## Ozone   -0.005189769
## Solar.R -0.057753801
## Wind     0.049871017
## Temp    -0.096545800
## Month   -0.009001079
## Day      1.000000000



cor.test()

cor.test(airquality$Ozone, airquality$Wind, 
         method = "pearson") # "spearman" si no paramétricos

## 
##  Pearson's product-moment correlation
## 
## data:  airquality$Ozone and airquality$Wind
## t = -8.0401, df = 114, p-value = 9.272e-13
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.7063918 -0.4708713
## sample estimates:
##        cor 
## -0.6015465

## lm -----

lm() y cor.test()

?trees
# Diameter, Height and Volume for Black Cherry Trees
head(trees)

lm1 <- lm(Volume ~ Height, trees) 
lm2 <- lm(Girth ~ Height, trees)
cor(lm1$residuals, lm2$residuals) # correlación controlando por Height

## [1] 0.9586123

## chisq.test ----

chisq.test() # prueba de independencia con frecuencias

data(survey, package = "MASS")
?survey
# responses of 237 Statistics I students at the University of Adelaide
head(survey)

tbl <- table(survey$Smoke, survey$Exer) 
tbl

##        
##         Freq None Some
##   Heavy    7    1    3
##   Never   87   18   84
##   Occas   12    3    4
##   Regul    9    1    7




chisq.test() # prueba de independencia con frecuencias

chisq.test(tbl)

## 
##  Pearson's Chi-squared test
## 
## data:  tbl
## X-squared = 5.4885, df = 6, p-value = 0.4828



## T-test ----

### una muestra -----
t.test() # para una muestra, compara contra media poblacional

data(mice, package = "datarium")

t.test(mice$weight, mu = 20)

## 
##  One Sample t-test
## 
## data:  mice$weight
## t = 0.23346, df = 9, p-value = 0.8206
## alternative hypothesis: true mean is not equal to 20
## 95 percent confidence interval:
##  18.78346 21.49654
## sample estimates:
## mean of x 
##     20.14

### dos muestras independientes

data(genderweight, package = "datarium")

t.test(weight ~ group, data = genderweight)

## 
##  Welch Two Sample t-test
## 
## data:  weight by group
## t = -20.791, df = 26.872, p-value < 2.2e-16
## alternative hypothesis: true difference in means between group F and group M is not equal to 0
## 95 percent confidence interval:
##  -24.53135 -20.12353
## sample estimates:
## mean in group F mean in group M 
##        63.49867        85.82612


### dos muestras pareadas

data(mice2, package = "datarium")

t.test(mice2$before,mice2$after, paired = T)

## 
##  Paired t-test
## 
## data:  mice2$before and mice2$after
## t = -25.546, df = 9, p-value = 1.039e-09
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -217.1442 -181.8158
## sample estimates:
## mean of the differences 
##                 -199.48

## ANOVA -----

aov() # para ANOVA o comparación de tres o más muestras

data(stress, package = "datarium")

stress.aov <- aov(score ~ treatment + exercise, stress)
stress.aov

## Call:
##    aov(formula = score ~ treatment + exercise, data = stress)
## 
## Terms:
##                 treatment exercise Residuals
## Sum of Squares    351.384 1776.266  1760.617
## Deg. of Freedom         1        2        56
## 
## Residual standard error: 5.607102
## Estimated effects may be unbalanced


### summary de aov
summary(stress.aov)

##             Df Sum Sq Mean Sq F value   Pr(>F)    
## treatment    1  351.4   351.4   11.18  0.00148 ** 
## exercise     2 1776.3   888.1   28.25 3.29e-09 ***
## Residuals   56 1760.6    31.4                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### ANOVA con interacción -----

  # ANOVA con interacción, usamos * en vez de +
  
stress.aov <- aov(score ~ treatment*exercise, stress)
stress.aov

## Call:
##    aov(formula = score ~ treatment * exercise, data = stress)
## 
## Terms:
##                 treatment exercise treatment:exercise Residuals
## Sum of Squares    351.384 1776.266            217.321  1543.296
## Deg. of Freedom         1        2                  2        54
## 
## Residual standard error: 5.345985
## Estimated effects may be unbalanced


#### summary con la interacción

summary(stress.aov)

##                    Df Sum Sq Mean Sq F value   Pr(>F)    
## treatment           1  351.4   351.4  12.295 0.000923 ***
## exercise            2 1776.3   888.1  31.076 1.04e-09 ***
## treatment:exercise  2  217.3   108.7   3.802 0.028522 *  
## Residuals          54 1543.3    28.6                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1




### Comparaciones post hoc con prueba de Tukey -----

stress.Tukey <- TukeyHSD(stress.aov)
stress.Tukey$treatment 

##        diff     lwr     upr        p adj
## no-yes 4.84 2.07261 7.60739 0.0009226641

stress.Tukey$exercise

##                  diff        lwr       upr        p adj
## moderate-low   -0.610  -4.684196  3.464196 9.308415e-01
## high-low      -11.835 -15.909196 -7.760804 1.227992e-08
## high-moderate -11.225 -15.299196 -7.150804 4.724332e-08

## Ejercicio -----

  # Usa sleep para hacer una t de Student pareada
?sleep
head(sleep)

t.test(formula = extra ~ group, data = sleep, paired = T)

## 
##  Paired t-test
## 
## data:  extra by group
## t = -4.0621, df = 9, p-value = 0.002833
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.4598858 -0.7001142
## sample estimates:
## mean of the differences 
##                   -1.58

  # Haz una ANOVA con warpbreaks y obten el summary()

aov.wbrks <- aov(breaks ~ wool*tension, data = warpbreaks)
summary(aov.wbrks)

##              Df Sum Sq Mean Sq F value   Pr(>F)    
## wool          1    451   450.7   3.765 0.058213 .  
## tension       2   2034  1017.1   8.498 0.000693 ***
## wool:tension  2   1003   501.4   4.189 0.021044 *  
## Residuals    48   5745   119.7                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


  # Realiza las comparaciones post hoc con TukeyHSD()

poshoc.wbrks <- TukeyHSD(aov.wbrks)
poshoc.wbrks$wool

##          diff       lwr       upr      p adj
## B-A -5.777778 -11.76458 0.2090243 0.05821298

poshoc.wbrks$tension

##           diff       lwr       upr        p adj
## M-L -10.000000 -18.81965 -1.180353 0.0228553984
## H-L -14.722222 -23.54187 -5.902575 0.0005595392
## H-M  -4.722222 -13.54187  4.097425 0.4049441962


poshoc.wbrks$`wool:tension` %>% 
  data.frame() %>% 
  cbind(rn = row.names(.), .) %>% 
  filter(p.adj < 0.05)

##              rn      diff       lwr        upr        p.adj
## B:L-A:L B:L-A:L -16.33333 -31.63966  -1.027012 0.0302143219
## A:M-A:L A:M-A:L -20.55556 -35.86188  -5.249234 0.0029580438
## B:M-A:L B:M-A:L -15.77778 -31.08410  -0.471456 0.0398172376
## A:H-A:L A:H-A:L -20.00000 -35.30632  -4.693678 0.0040954674
## B:H-A:L B:H-A:L -25.77778 -41.08410 -10.471456 0.0001136469

  # Discute tus conclusiones


# Regresión ----

## Regresión sencilla ----

  # La relación de una variable predictora y una variable de respuesta

plot(faithful)


## Detalles estadísticos con summary() ----

m1 <- lm(eruptions ~ waiting, data = faithful)
summary(m1)


## Gráficos de residuales ----

par(mfrow = c(2, 2))
plot(m1)

## Regresión múltiple

  # La relación de más de una variable predictora con una variable de respuesta

movies <- read.table("movies.csv",sep = ",", header = T) 
head(movies)

##   Ganancias CostoProd CostoPromo LibrosVendidos
## 1      85.1       8.5   5.100000            4.7
## 2     106.3      12.9   5.800000            8.8
## 3      50.2       5.2   2.100000           15.1
## 4     130.6      10.7   8.399999           12.2
## 5      54.8       3.1   2.900000           10.6
## 6      30.3       3.5   1.200000            3.5

### 1 variable predictora ----

lm.mov1 <- lm(Ganancias ~ CostoProd, data = movies)
summary(lm.mov1)

## 
## Call:
## lm(formula = Ganancias ~ CostoProd, data = movies)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.136  -9.029  -3.689   3.208  29.723 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   15.513     11.603   1.337 0.217989    
## CostoProd      7.978      1.223   6.522 0.000184 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.26 on 8 degrees of freedom
## Multiple R-squared:  0.8417, Adjusted R-squared:  0.8219 
## F-statistic: 42.54 on 1 and 8 DF,  p-value: 0.0001838


### 2 variables predictoras ----
lm.mov2 <- lm(Ganancias ~ CostoProd + CostoPromo, data = movies)
summary(lm.mov2)

## 
## Call:
## lm(formula = Ganancias ~ CostoProd + CostoPromo, data = movies)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.4168  -2.5696   0.8052   2.1200  11.0463 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   11.848      6.765   1.751  0.12334   
## CostoProd      4.228      1.153   3.667  0.00800 **
## CostoPromo     7.436      1.806   4.117  0.00448 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.241 on 7 degrees of freedom
## Multiple R-squared:  0.9537, Adjusted R-squared:  0.9405 
## F-statistic: 72.14 on 2 and 7 DF,  p-value: 2.131e-05


### 3 variables predictoras ----

m.mov3 <- lm(Ganancias ~ CostoProd + CostoPromo + LibrosVendidos, data = movies)
summary(lm.mov3)

## 
## Call:
## lm(formula = Ganancias ~ CostoProd + CostoPromo + LibrosVendidos, 
##     data = movies)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.4384  -3.1695   0.8499   3.5134   9.6207 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)   
## (Intercept)      7.6760     6.7602   1.135   0.2995   
## CostoProd        3.6616     1.1178   3.276   0.0169 * 
## CostoPromo       7.6211     1.6573   4.598   0.0037 **
## LibrosVendidos   0.8285     0.5394   1.536   0.1754   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.541 on 6 degrees of freedom
## Multiple R-squared:  0.9668, Adjusted R-squared:  0.9502 


### Comparando modelos ----


  # Comparando el modelo 1 vs modelo 2

anova(lm.mov1, lm.mov2)

## Analysis of Variance Table
## 
## Model 1: Ganancias ~ CostoProd
## Model 2: Ganancias ~ CostoProd + CostoPromo
##   Res.Df     RSS Df Sum of Sq      F   Pr(>F)   
## 1      8 1626.27                                
## 2      7  475.37  1    1150.9 16.947 0.004478 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



  # Comparando el modelo 2 vs modelo 3

anova(lm.mov2, lm.mov3)

## Analysis of Variance Table
## 
## Model 1: Ganancias ~ CostoProd + CostoPromo
## Model 2: Ganancias ~ CostoProd + CostoPromo + LibrosVendidos
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1      7 475.37                           
## 2      6 341.20  1    134.17 2.3594 0.1754


### Ejercicio ----


  # Usa swiss para realizar una regresión múltiple usando Fertility como variable de respuesta y Agriculture, Examination, Education, Catholic y Infant.Mortality



  # Puedes ir buscando la combinación de variables que mejor predice Fertility comparando tus modelos con anova

  # Discute tus resutados



mod1 <- lm(Fertility ~ Infant.Mortality + Examination + Agriculture + 
             Education + Catholic, data=swiss)
summary(mod1)

## 
## Call:
## lm(formula = Fertility ~ Infant.Mortality + Examination + Agriculture + 
##     Education + Catholic, data = swiss)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.2743  -5.2617   0.5032   4.1198  15.3213 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      66.91518   10.70604   6.250 1.91e-07 ***
## Infant.Mortality  1.07705    0.38172   2.822  0.00734 ** 
## Examination      -0.25801    0.25388  -1.016  0.31546    
## Agriculture      -0.17211    0.07030  -2.448  0.01873 *  
## Education        -0.87094    0.18303  -4.758 2.43e-05 ***
## Catholic          0.10412    0.03526   2.953  0.00519 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 

mod2 <- lm(Fertility ~ Infant.Mortality + Agriculture + Education +
             Catholic,data=swiss)
summary(mod2)

## 
## Call:
## lm(formula = Fertility ~ Infant.Mortality + Agriculture + Education + 
##     Catholic, data = swiss)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.6765  -6.0522   0.7514   3.1664  16.1422 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      62.10131    9.60489   6.466 8.49e-08 ***
## Infant.Mortality  1.07844    0.38187   2.824  0.00722 ** 
## Agriculture      -0.15462    0.06819  -2.267  0.02857 *  
## Education        -0.98026    0.14814  -6.617 5.14e-08 ***
## Catholic          0.12467    0.02889   4.315 9.50e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##

anova(mod1, mod2)

## Analysis of Variance Table
## 
## Model 1: Fertility ~ Infant.Mortality + Examination + Agriculture + Education + 
##     Catholic
## Model 2: Fertility ~ Infant.Mortality + Agriculture + Education + Catholic
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1     41 2105.0                           
## 2     42 2158.1 -1   -53.027 1.0328 0.3155

mod3 <- lm(Fertility ~ Infant.Mortality + Education +
             Catholic,data=swiss)
summary(mod3)

## 
## Call:
## lm(formula = Fertility ~ Infant.Mortality + Education + Catholic, 
##     data = swiss)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -14.4781  -5.4403  -0.5143   4.1568  15.1187 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      48.67707    7.91908   6.147 2.24e-07 ***
## Infant.Mortality  1.29615    0.38699   3.349  0.00169 ** 
## Education        -0.75925    0.11680  -6.501 6.83e-08 ***
## Catholic          0.09607    0.02722   3.530  0.00101 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.505 on 43 degrees of freedom


anova(mod2, mod3)

## Analysis of Variance Table
## 
## Model 1: Fertility ~ Infant.Mortality + Agriculture + Education + Catholic
## Model 2: Fertility ~ Infant.Mortality + Education + Catholic
##   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
## 1     42 2158.1                              
## 2     43 2422.2 -1   -264.18 5.1413 0.02857 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# Modelos Lineales Generalizados, reg logistica -----


  # Combinas variables predictoras discretas y continuas

  # Podemos especifcar la naturaleza de la variable de respuesta mediante una función liga (p.e. Gausiana, Poisson o Binomial)

  # Cuando la variable de respuesta es binomial, también se conoce como Regresión Logística


## 1 variable continua ----

data(stress, package = "datarium")
head(stress)

stress.glm <- glm(score ~ age, data = stress)
summary(stress.glm)

## 
## Call:
## glm(formula = score ~ age, data = stress)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -14.8747   -5.7859    0.7786    5.7263   11.9618  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  25.3551    12.4617   2.035   0.0465 *  
## age           0.9878     0.2073   4.765 1.31e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 48.18089)
## 
##     Null deviance: 3888.3  on 59  degrees of freedom

plot(stress$age, stress$score, pch = 16, xlab = "Age", ylab = "Score")
abline(lm(score ~ age, data = stress),col="red") 


## 1 una variable discreta ----

stress.glm <- glm(score ~ treatment, data = stress)
summary(stress.glm)

## 
## Call:
## glm(formula = score ~ treatment, data = stress)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -16.3567   -4.7467    0.7533    5.1833   15.0433  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   82.157      1.426   57.62   <2e-16 ***
## treatmentno    4.840      2.016    2.40   0.0196 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 60.98075)
## 
##     Null deviance: 3888.3  on 59  degrees of freedom
## Residual deviance: 3536.9  on 58  degrees of freedom

boxplot(score ~ treatment, data = stress)


## 2 variables (continua y distreta) e interacción ----

stress.glm  <- glm(score ~ age*treatment, data = stress)
summary(stress.glm)

## 
## Call:
## glm(formula = score ~ age * treatment, data = stress)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -11.9308   -5.1018    0.5849    4.5209   14.0019  
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       5.5423    19.5280   0.284 0.777603    
## age               1.2891     0.3279   3.931 0.000235 ***
## treatmentno      37.7881    24.8571   1.520 0.134085    
## age:treatmentno  -0.5669     0.4145  -1.368 0.176880    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 44.45309)
## 
##     Null deviance: 3888.3  on 59  degrees of freedom
## Residual deviance: 2489.4  on 56  degrees of freedom
## AIC: 403.8
## 

ggplot(stress, aes(x=age, y=score, color=treatment)) + geom_point() + geom_smooth(method='lm'

                                                                                  
                                                                                  
## Función liga ----
?infert
head(infert)

##   education age parity induced case spontaneous stratum pooled.stratum
## 1    0-5yrs  26      6       1    1           2       1              3
## 2    0-5yrs  42      1       1    1           0       2              1
## 3    0-5yrs  39      6       2    1           0       3              4
## 4    0-5yrs  34      4       2    1           0       4              2
## 5   6-11yrs  35      3       1    1           1       5             32
## 6   6-11yrs  36      4       2    1           1       6             36

unique(infert$case)

## [1] 1 0
                                                                                  
inf.bn <- glm(case ~ age + parity + education + spontaneous, 
              data = infert, family = binomial())
summary(inf.bn)

## 
## Call:
## glm(formula = case ~ age + parity + education + spontaneous, 
##     family = binomial(), data = infert)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7219  -0.7698  -0.6353   0.9825   2.1823  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)      -0.63833    1.33256  -0.479   0.6319    
## age               0.01553    0.02920   0.532   0.5947    
## parity           -0.30516    0.13729  -2.223   0.0262 *  
## education6-11yrs -0.69194    0.76038  -0.910   0.3628    
## education12+ yrs -0.84472    0.79294  -1.065   0.2867    
## spontaneous       1.27783    0.22305   5.729 1.01e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##                                                                                   

inf.no <- glm(case ~ age + parity + education + spontaneous, 
              data = infert)
summary(inf.no)

## 
## Call:
## glm(formula = case ~ age + parity + education + spontaneous, 
##     data = infert)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.7752  -0.2685  -0.1795   0.3808   0.9700  
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       0.371789   0.258916   1.436   0.1523    
## age               0.002873   0.005664   0.507   0.6124    
## parity           -0.059536   0.026263  -2.267   0.0243 *  
## education6-11yrs -0.155096   0.146016  -1.062   0.2892    
## education12+ yrs -0.181238   0.151961  -1.193   0.2342    
## spontaneous       0.271910   0.041317   6.581 2.87e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


## Comparación binomial -----

par(mfrow=c(2,2))
with(infert,plot(case ~ age + jitter(parity,.5) + education + jitter(spontaneous,.5)))

## Comparación post hoc ----

library(multcomp)

summary(glht(inf.no, mcp(education="Tukey")))

## 
##   Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: glm(formula = case ~ age + parity + education + spontaneous, 
##     data = infert)
## 
## Linear Hypotheses:
##                        Estimate Std. Error z value Pr(>|z|)
## 6-11yrs - 0-5yrs == 0  -0.15510    0.14602  -1.062    0.519
## 12+ yrs - 0-5yrs == 0  -0.18124    0.15196  -1.193    0.438
## 12+ yrs - 6-11yrs == 0 -0.02614    0.06043  -0.433    0.896
## (Adjusted p values reported -- single-step method)

## GLM: Ejercicios ----

  # Usa hsb del paquete descriptr para hacer un modelo para cada variable de respuesta: read, write, math, science, socst

  # Calcula el puntaje general (como suma y como promedio) y has un nuevo modelo utilizando esta nueva variable de respuesta

  # Encuentra el “mejor” modelo

  # Grafica las comparaciones para las que salgan significativas


library(descriptr)

?hsb

head(hsb)

##    id female race ses schtyp prog read write math science socst
## 1  70      0    4   1      1    1   57    52   41      47    57
## 2 121      1    4   2      1    3   68    59   53      63    61
## 3  86      0    4   3      1    1   44    33   54      58    31
## 4 141      0    4   3      1    3   63    44   47      53    56
## 5 172      0    4   2      1    2   47    52   57      53    61
## 6 113      0    4   2      1    2   44    52   51      63    61

# cambiar manualmente el valor de la variable de respuesta
glm.read <- glm(read ~ female + race + ses + schtyp + prog, data = hsb)
summary(glm.read)

## 
## Call:
## glm(formula = read ~ female + race + ses + schtyp + prog, data = hsb)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -20.7785   -6.3335   -0.1431    6.0284   20.6762  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  44.1760     2.4777  17.830  < 2e-16 ***
## female1      -0.7421     1.3034  -0.569 0.569783    
## race2         3.5716     3.3355   1.071 0.285622    
## race3         0.4647     2.7477   0.169 0.865872    
## race4         5.6720     2.0314   2.792 0.005772 ** 
## ses2          1.8983     1.7182   1.105 0.270641    
## ses3          4.3395     1.9119   2.270 0.024349 *  
## schtyp2      -1.0406     1.8111  -0.575 0.566253    
## prog2         5.5910     1.6488   3.391 0.000847 ***


# suma de todos los puntajes 
hsb %<>% mutate(sum_scores = read+write+math+science+socst)
glm.sum <- glm(sum_scores ~ female + race + ses + schtyp + prog, data = hsb)
summary(glm.sum)

## 
## Call:
## glm(formula = sum_scores ~ female + race + ses + schtyp + prog, 
##     data = hsb)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -94.050  -20.931   -1.445   26.559   99.629  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 221.8452     9.0899  24.406  < 2e-16 ***
## female1       3.4800     4.7818   0.728 0.467664    
## race2        24.9665    12.2369   2.040 0.042708 *  
## race3         0.3997    10.0805   0.040 0.968411    
## race4        27.4641     7.4528   3.685 0.000298 ***
## ses2         11.0779     6.3036   1.757 0.080462 .  
## ses3         21.2478     7.0143   3.029 0.002793 ** 


# promedio de todos los puntajes
hsb %<>% mutate(mean_scores = sum_scores/5)
glm.mean <- glm(mean_scores ~ female + race + ses + schtyp + prog, data = hsb)
summary(glm.mean)

## 
## Call:
## glm(formula = mean_scores ~ female + race + ses + schtyp + prog, 
##     data = hsb)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -18.810   -4.186   -0.289    5.312   19.926  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 44.36905    1.81798  24.406  < 2e-16 ***
## female1      0.69600    0.95637   0.728 0.467664    
## race2        4.99331    2.44739   2.040 0.042708 *  
## race3        0.07994    2.01610   0.040 0.968411    
## race4        5.49282    1.49056   3.685 0.000298 ***
## ses2         2.21559    1.26073   1.757 0.080462 .  
## ses3         4.24956    1.40286   3.029 0.002793 ** 
## schtyp2     -0.76867    1.32888  -0.578 0.563656    

step(glm.sum)

## Start:  AIC=1977.69
## sum_scores ~ female + race + ses + schtyp + prog
## 
##          Df Deviance    AIC
## - schtyp  1   207037 1976.0
## - female  1   207249 1976.2
## <none>        206673 1977.7
## - ses     2   216716 1983.2
## - race    3   229578 1992.7
## - prog    2   262882 2021.8
## 
## Step:  AIC=1976.04
## sum_scores ~ female + race + ses + prog
## 
##          Df Deviance    AIC
## - female  1   207576 1974.6
## <none>        207037 1976.0
## - ses     2   216834 1981.3
## - race    3   229744 1990.9
## - prog    2   263901 2020.6
## 

# visualize sum of scores vs. categorical variables
boxplot(sum_scores ~ race, data = hsb)
boxplot(sum_scores ~ ses, data = hsb)
boxplot(sum_scores ~ prog, data = hsb)

# visualize mean scores vs. categorical variables
boxplot(mean_scores ~ race, data = hsb)
boxplot(mean_scores ~ ses, data = hsb)
boxplot(mean_scores ~ prog, data = hsb)


# Modelos Lineales Generalizados Mixtos -----


  # Múltiples variables predictora (categóricas y numéricas) sobre una variable de respuesta

  # Evaluar datos en los que existen medidas repetidas

  # Contemplan el error intra grupo, por ejemplo, para observaciones temporales sobre el mismo individuo (p.e. antes y después) o sobre distintos individuos en la misma circunstancia (p.e. lugar, hora o fecha)



library(lmerTest)
?iris
head(iris)

##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa


  # Incluir el random effect

lmm1<-lmer(Sepal.Length ~ Petal.Length + (1|Species), data=iris)
summary(lmm1)

## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: Sepal.Length ~ Petal.Length + (1 | Species)
##    Data: iris
## 
## REML criterion at convergence: 119.8
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.23334 -0.67332 -0.01684  0.67680  3.04429 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  Species  (Intercept) 1.1617   1.0778  
##  Residual             0.1143   0.3381  
## Number of obs: 150, groups:  Species, 3
## 
## Fixed effects:
##               Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)    2.50446    0.66743   2.45496   3.752   0.0463 *  
## Petal.Length   0.88847    0.06379 144.56089  13.927   <2e-16 ***
## ---

anova(lmm1)

## Type III Analysis of Variance Table with Satterthwaite's method
##              Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
## Petal.Length 22.166  22.166     1 144.56  193.96 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

boxplot(Petal.Length ~ Species, data=iris)

## GLMM: Ejercicios-----

  # Revisa como están organizados los datos con head(), levels() y str()

  # Construye un diagrama (a mano) del diseño experimental que sugieren los datos

  # Usa CO2 del para hacer un modelo donde consideres Type, Plant y Treatment (?) como efecto aleatorios anidados (separados por /)

  # Grafica uptake por cada una de las variables significativas

?CO2
head(CO2)
str(CO2)
CO2$Type
CO2$Plant
CO2$Plant
CO2 %>% filter(Treatment == "chilled")

  # Type >> Treatment >> Plant 

lmm.co2 <- lmer(uptake ~ conc + Type*Treatment + 1|Type/Treatment/Plant, data=CO2)
summary(lmm.co2)

## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: uptake ~ conc + Type * Treatment + 1 | Type/Treatment/Plant
##    Data: CO2
## 
## REML criterion at convergence: 549.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.7674 -0.5194  0.1704  0.6621  1.8149 
## 
## Random effects:
##  Groups                 Name                             Variance  Std.Dev. 
##  Plant:(Treatment:Type) (Intercept)                      1.860e-01 0.4312915
##                         conc                             6.782e-08 0.0002604
##                         TypeMississippi                  7.323e+00 2.7060211
##                         Treatmentchilled                 6.912e+00 2.6291153
##                         TypeMississippi:Treatmentchilled 3.389e+01 5.8215663
##  Treatment:Type         (Intercept)                      8.593e+00 2.9314152
##                         conc                             9.761e-06 0.0031243

ggplot(CO2, aes(x=conc,y=uptake,color=Plant)) + geom_point() + facet_grid(Treatment~Type)


# Análisis agregativos ----

  # Reducir el número de variables al identificar aquellas que nos dan información redundante o agrupar variables dentro de una categoría mas amplia
  # Combinar ambos análisis (por individuos y por variables) para caracterizar grupos de individuos por variables
  # 



install.packages("FactoMineR")
library(FactoMineR) # solo un paquete de muchos
?decathlon

data(decathlon)
head(decathlon)

##          100m Long.jump Shot.put High.jump  400m 110m.hurdle Discus Pole.vault
## SEBRLE  11.04      7.58    14.83      2.07 49.81       14.69  43.75       5.02
## CLAY    10.76      7.40    14.26      1.86 49.37       14.05  50.72       4.92
## KARPOV  11.02      7.30    14.77      2.04 48.37       14.09  48.95       4.92
## BERNARD 11.02      7.23    14.25      1.92 48.93       14.99  40.87       5.32
## YURKOV  11.34      7.09    15.19      2.10 50.42       15.31  46.26       4.72
## WARNERS 11.11      7.60    14.31      1.98 48.68       14.23  41.10       4.92
##         Javeline 1500m Rank Points Competition
## SEBRLE     63.19 291.7    1   8217    Decastar
## CLAY       60.15 301.5    2   8122    Decastar
## KARPOV     50.31 300.2    3   8099    Decastar
## BERNARD    62.77 280.1    4   8067    Decastar
## YURKOV     63.44 276.4    5   8036    Decastar
## WARNERS    51.77 278.1    6   8030    Decastar



res.pca <- PCA(decathlon[ ,1:10], scale.unit=TRUE , ncp=5, graph=F)
str(res.pca)

## List of 5
##  $ eig : num [1:10, 1:3] 3.272 1.737 1.405 1.057 0.685 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:10] "comp 1" "comp 2" "comp 3" "comp 4" ...
##   .. ..$ : chr [1:3] "eigenvalue" "percentage of variance" "cumulative percentage of variance"
##  $ var :List of 4
##   ..$ coord  : num [1:10, 1:5] -0.775 0.742 0.623 0.572 -0.68 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:10] "100m" "Long.jump" "Shot.put" "High.jump" ...
##   .. .. ..$ : chr [1:5] "Dim.1" "Dim.2" "Dim.3" "Dim.4" ...
##   ..$ cor    : num [1:10, 1:5] -0.775 0.742 0.623 0.572 -0.68 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:10] "100m" "Long.jump" "Shot.put" "High.jump" ...
##   .. .. ..$ : chr [1:5] "Dim.1" "Dim.2" "Dim.3" "Dim.4" ...
##   ..$ cos2   : num [1:10, 1:5] 0.6 0.55 0.388 0.327 0.462 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:10] "100m" "Long.jump" "Shot.put" "High.jump" ...
##   .. .. ..$ : chr [1:5] "Dim.1" "Dim.2" "Dim.3" "Dim.4" ...
##   ..$ contrib: num [1:10, 1:5] 18.3 16.8 11.8 10 14.1 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:10] "100m" "Long.jump" "Shot.put" "High.jump" ...
##   .. .. ..$ : chr [1:5] "Dim.1" "Dim.2" "Dim.3" "Dim.4" ...
##  $ ind :List of 4
##   ..$ coord  : num [1:41, 




eig.val <- get_eigenvalue(res.pca)
eig.val

##        eigenvalue variance.percent cumulative.variance.percent
## Dim.1   3.2719055        32.719055                    32.71906
## Dim.2   1.7371310        17.371310                    50.09037
## Dim.3   1.4049167        14.049167                    64.13953
## Dim.4   1.0568504        10.568504                    74.70804
## Dim.5   0.6847735         6.847735                    81.55577
## Dim.6   0.5992687         5.992687                    87.54846
## Dim.7   0.4512353         4.512353                    92.06081
## Dim.8   0.3968766         3.968766                    96.02958
## Dim.9   0.2148149         2.148149                    98.17773
## Dim.10  0.1822275         1.822275                   100.00000

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_ind(res.pca)



  # Individuos más extremos en Dim1: BOURGUIGNON vs Karpov

names <- c("BOURGUIGNON", "Karpov")
decathlon[row.names(decathlon) %in% names, ]

##              100m Long.jump Shot.put High.jump  400m 110m.hurdle Discus
## BOURGUIGNON 11.36      6.80    13.46      1.86 51.16       15.67  40.49
## Karpov      10.50      7.81    15.93      2.09 46.81       13.97  51.65
##             Pole.vault Javeline  1500m Rank Points Competition
## BOURGUIGNON       5.02    54.68 291.70   13   7313    Decastar
## Karpov            4.60    55.54 278.11    3   8725    OlympicG

fviz_pca_ind(res.pca, col.ind = "cos2", repel = TRUE)

fviz_pca_var(res.pca, repel = TRUE)



  ## Ejercicio ----


Construye un PCA con data(wine)
Obtén:
  las gráficas de los individuos y de las variables
los valores eigen (>1) y el porcentaje de explicación de las dimensiones
el scree plot
¿Quienes son los individuos más extremos?
  ¿Cómo definirias las dimensiones (en tus propios términos)?
  
  data(wine)
res.pca <- PCA(wine %>% dplyr::select(-Label,-Soil), scale.unit = T, graph = F)
res.pca

## **Results for the Principal Component Analysis (PCA)**
## The analysis was performed on 21 individuals, described by 29 variables
## *The results are available in the following objects:
## 
##    name               description                          
## 1  "$eig"             "eigenvalues"                        
## 2  "$var"             "results for the variables"          
## 3  "$var$coord"       "coord. for the variables"           
## 4  "$var$cor"         "correlations variables - dimensions"
## 5  "$var$cos2"        "cos2 for the variables"             
## 6  "$var$contrib"     "contributions of the variables"     
## 7  "$ind"             "results for the individuals"        
## 8  "$ind$coord"       "coord. for the individuals"         
## 9  "$ind$cos2"        "cos2 for the individuals"           
## 10 "$ind$contrib"     "contributions of the individuals"   
## 11 "$call"            "summary statistics"                 
## 12 "$call$centre"     "mean of the variables"              
## 13 "$call$ecart.type" "standard error of the variables"    
## 14 "$call$row.w"      "weights for the individuals"     

fviz_pca_ind(res.pca)

fviz_pca_var(res.pca)

get_eig(res.pca) %>% as.data.frame() %>% filter(eigenvalue > 1)

##       eigenvalue variance.percent cumulative.variance.percent
## Dim.1  15.049891        51.896177                    51.89618
## Dim.2   5.471810        18.868310                    70.76449
## Dim.3   2.017153         6.955701                    77.72019
## Dim.4   1.359562         4.688146                    82.40833
## Dim.5   1.101292         3.797559                    86.20589

fviz_eig(res.pca)

wine %>% 
  mutate(names = rownames(wine)) %>% 
  dplyr::select(-Label,-Soil) %>% 
  filter(names %in% c("T2  ","1CHA")) %>% # extreme individuals in dim 1
  pivot_longer(cols = -names, names_to = "chars", values_to = "values") %>% 
  filter(chars == "Spice.before.shaking") %>% # extreme variable in dim 1
  head()

## # A tibble: 2 x 3
##   names  chars                values
##   <chr>  <chr>                 <dbl>
## 1 "1CHA" Spice.before.shaking   1.68
## 2 "T2  " Spice.before.shaking   2.67

wine %>% 
  mutate(names = rownames(wine)) %>% 
  dplyr::select(-Label,-Soil) %>% 
  filter(names %in% c("2ING","1DAM")) %>% # extreme individuals in dim 2
  gather("chars","values",-names) %>% 
  filter(chars == "Fruity.before.shaking") %>% # extreme variable in dim 2
  head()

##   names                 chars values
## 1  1DAM Fruity.before.shaking  3.154
## 2  2ING Fruity.before.shaking  2.536

## Clustering: kmeans ----

  # Uno de los métodos más viejos de agregación

  # Se eligen el número de clusters que uno cree que hay en los datos (k)

  # El algoritmo elige k puntos al azar y calcula la distancia a todos los puntos y las observaciones se clasifican en “agregados”

  # Se recalculan los centros y se vuelve a comenzar

  #El proceso continua hasta que no las observaciones no cambian de grupo

iris.kc <- kmeans(iris[1:4],3)
print(iris.kc)

## K-means clustering with 3 clusters of sizes 50, 38, 62
## 
## Cluster means:
##   Sepal.Length Sepal.Width Petal.Length Petal.Width
## 1     5.006000    3.428000     1.462000    0.246000
## 2     6.850000    3.073684     5.742105    2.071053
## 3     5.901613    2.748387     4.393548    1.433871
## 
## Clustering vector:
##   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [38] 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
##  [75] 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 2 2 2 2 3 2 2 2 2
## [112] 2 2 3 3 2 2 2 2 3 2 3 2 3 2 2 3 3 2 2 2 2 2 3 2 2 2 2 3 2 2 2 3 2 2 2 3 2
## [149] 2 3
## 
## Within cluster sum of squares by cluster:
## [1] 15.15100 23.87947 39.82097
##  (between_SS / total_SS =  88.4 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
## [6] "betweenss"    "size"         "iter"         "ifault"


plot(iris$Sepal.Length, iris$Sepal.Width, col=iris.kc$cluster)
points(iris.kc$centers[, c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=2)


## Clustering: pam ----

  # Una alternativa más moderna de agregación es PAM (Partición Alrededor de Medias)

  # Al igual que en kmeans, es necesario especificar el número de clusters

  # Este algoritmo recalcula las distancias entre los objeto dentro de un cluster en cada ciclo, lo que puede dar como resultado clusters más robustos

  # Como kmeans, el resultado puede cambiar al cambiar k

library(cluster)
cars.clus <- pam(scale(cars), k = 3)
plot(cars.clus)

  # El gráfico de siluetas produce una medida para cada valor que nos dice que también encaja en el cluster

  # Valores cerca de 1 indican un buen ajuste, mientras que 0 o valores negativos indican que ese valor probablemente pertenezca a otro cluster

  # En cada cluster, los valores están organizados del mayor a menor

  # Criterios para legir el número de clusters o a saber si el algoritmo esta haciendo un buen trabajo



## hclust -----

  # Como se agregan más puntos a ese cluster inicial puede depender del método: distancia mínima, máxima, promedio, etc.

  # Cada uno de estos métodos puede mostrar distintos aspectos de la estructura de los datos

  # Usar la distancia al punto mínimo genera dendogramas tipo “serpiente”

  # Usar la distancia al punto máximo genera grupos mas chicos y densos

  # Usar el promedio es un compromiso entre estos dos puntos

  # El método de Ward intenta usar distancia mínima, pero que no queden grupos demasiado pequeños

  # Un aspecto que hace a este algoritmo interesante es que las soluciones con muchos clusters están incluidas (anidadas) dentro de las soluciones con menos grupos

  # Esto hace que las observaciones no salten de un grupo a otro como lo hacen en kmeans o pam

  # Tampoco es necesario especificar el número de grupos

uscities.single.link <- hclust(UScitiesD, method = "single")
plot(uscities.single.link)

uscities.average.link <- hclust(UScitiesD, method = "average")
plot(uscities.average.link)

  ## Clustering: Ejercicios ----

  # Calcula los clusters en los datos de faithful usando kmeans y pam

  # Calcula y grafica los clusters en eurodist con hclust

?faithful
head(faithful)

kf <- kmeans(faithful, centers = 3)
plot(faithful, col = kf$cluster)

library(cluster)
f.clus <- pam(faithful, k = 3)
plot(f.clus)

euro.average.link <- hclust(eurodist, method = "average")
plot(euro.average.link)

# Broom resultados estadisticos en tablas ----

tidy() # construye un df que resume la información contenida en un modelo

augment() # añade columnas originales a los datos modelados, como predicciones

glance() # construye un resumen de una línea del model

library(broom)
lmfit <- lm(mpg ~ wt, mtcars)
summary(lmfit)

## 
## Call:
## lm(formula = mpg ~ wt, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.5432 -2.3647 -0.1252  1.4096  6.8727 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  37.2851     1.8776  19.858  < 2e-16 ***
## wt           -5.3445     0.5591  -9.559 1.29e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.046 on 30 degrees of freedom
## Multiple R-squared:  0.7528, Adjusted R-squared:  0.7446 
## F-statistic: 91.38 on 1 and 30 DF,  p-value: 1.294e-10

tidy(lmfit)

## # A tibble: 2 x 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)    37.3      1.88      19.9  8.24e-19
## 2 wt             -5.34     0.559     -9.56 1.29e-10

head(augment(lmfit))

## # A tibble: 6 x 9
##   .rownames           mpg    wt .fitted .resid   .hat .sigma   .cooksd .std.resid
##   <chr>             <dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl>     <dbl>      <dbl>
## 1 Mazda RX4          21    2.62    23.3 -2.28  0.0433   3.07 0.0133       -0.766 
## 2 Mazda RX4 Wag      21    2.88    21.9 -0.920 0.0352   3.09 0.00172      -0.307 
## 3 Datsun 710         22.8  2.32    24.9 -2.09  0.0584   3.07 0.0154       -0.706 
## 4 Hornet 4 Drive     21.4  3.22    20.1  1.30  0.0313   3.09 0.00302       0.433 
## 5 Hornet Sportabout  18.7  3.44    18.9 -0.200 0.0329   3.10 0.0000760    -0.0668
## 6 Valiant            18.1  3.46    18.8 -0.693 0.0332   3.10 0.000921     -0.231


glance(lmfit)

## # A tibble: 1 x 12
##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl> <dbl> <dbl>
## 1     0.753         0.745  3.05      91.4 1.29e-10     1  -80.0  166.  170.
## # ... with 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>



lmfit1 <- lm(mpg ~ wt, mtcars)
lmfit2 <- lm(mpg ~ wt + drat, mtcars)
lmfit3 <- lm(mpg ~ wt + drat + hp, mtcars)

# resumen eficiente

all_models <- rbind.data.frame(
  tidy(lmfit1) %>% mutate(model = 1),
  tidy(lmfit2) %>% mutate(model = 2),
  tidy(lmfit3) %>% mutate(model = 3))
all_models

## # A tibble: 9 x 6
##   term        estimate std.error statistic  p.value model
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl> <dbl>
## 1 (Intercept)  37.3      1.88       19.9   8.24e-19     1
## 2 wt           -5.34     0.559      -9.56  1.29e-10     1
## 3 (Intercept)  30.3      7.32        4.14  2.74e- 4     2
## 4 wt           -4.78     0.797      -6.00  1.59e- 6     2
## 5 drat          1.44     1.46        0.989 3.31e- 1     2
## 6 (Intercept)  29.4      6.16        4.77  5.13e- 5     3
## 7 wt           -3.23     0.796      -4.05  3.64e- 4     3
## 8 drat          1.62     1.23        1.32  1.99e- 1     3
## 9 hp           -0.0322   0.00892    -3.61  1.18e- 3     3


# formattable & kableExtra Formateo ----

install.packages("formattable")
library(formattable)
library(kableExtra)

  # Maneja múltiples formatos númericos

percent(c(0.1, 0.02, 0.03, 0.12))

## [1] 10.00% 2.00%  3.00%  12.00%

accounting(c(1000, 500, 200, -150, 0, 1200))

## [1] 1,000.00 500.00   200.00   (150.00) 0.00     1,200.00

  # Otros como comma(), currency(), scientific()

mtcars[1:5, 1:4] %>%
  mutate(
    car = row.names(.),
    mpg = color_tile("white", "orange")(mpg),
    cyl = cell_spec(cyl, angle = (1:5)*60, 
                    background = "red", color = "white", align = "center"),
    disp = ifelse(disp > 200,
                  cell_spec(disp, color = "red", bold = T),
                  cell_spec(disp, color = "green", italic = T)),
    hp = color_bar("lightgreen")(hp)
  ) %>%
  select(car, everything()) %>%
  kable(escape = F) %>%
  kable_styling("hover", full_width = F) 



df <- data.frame(
  id = 1:10, 
  name = c("Bob", "Ashley", "James", "David", "Jenny", "Hans", 
           "Leo", "John", "Emily", "Lee"),
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30), 
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"), 
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6), 
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8), 
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7), 
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors=FALSE)



formattable(df, 
            list(age = color_tile("white", "orange"),
                 grade = formatter("span", style = x ~ ifelse(x == "A", 
                                                              style(color = "green", font.weight = "bold"), NA)), 
                 area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2), 
                 final_score = formatter("span", 
                                         style = x ~ style(color = ifelse(rank(-x) <= 3, "green","gray")), 
                                         x ~ sprintf("%.2f(rank:%02d)", x, rank(-x))), 
                 registered = formatter("span", 
                                        style = x ~ style(color = ifelse(x, "green", "red")),
                                        x ~ icontext(ifelse(x, "ok", "remove"), 
                                                     ifelse(x, "Yes", "No")))
            ))

# gt Tablas ----
  # https://gt.rstudio.com/
  # https://www.liamdbailey.com/post/making-beautiful-tables-with-gt/


## Rmarkdown & knitr ----











