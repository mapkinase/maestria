# Paquetes para instalar ----
install.packages(c("purrr","broom"))
library(tidyverse)

# programación funcional ----------
  # ver clase, no llegúe a tiempo

n_mtcars %>%
  
  
map()

my_test <- function(x) {
  lm(mpg ~wt, data =x)
}


mtcars %>%
  nest(-cyl) %>%
  mutate(res =  map(data, my_test)) %>%
  mutate(glance_lm = res %>% map(glance)) %>%
  unnest(glance_lm)

  # con spli()
  # nota, el primer modelo es mejor
mtcars %>%
  split(.$cyl) %>%
  
  ## ejercicio -----

  # usando data(txhousing, package = "ggplot2")
  # describe, mediante un modelo, la relación lineal entre sales y listing
  # para cada categoría de year y extrae el valor de p
    
    # tips lm(sales ~ listings)

head(txhousing)

ejercicio <- function(x) {
  lm(sales ~ listings, data = x)
}



# Procesamiento ----

## libridate ----
  # formato de fecha/hora con lubridate

library(lubridate)
date1 <- c()
dmy(date1)

## quitar datos faltantes ----
naq <- na.omit(airquality) # quita todas las filas con NA
naq <- airquality[complete.cases(airquality)] # solución paquete base

naq <- airquality %>% 
  drop_na()

## imputar (sust con media) datos faltantes ----

naq %>%
  mutate (Ozone = ifelse(is.na(Ozone), mean(Ozone, na.rm=TRUE), Ozone))

naqu %>%
  mutate(Ozone = Hmisc::impute(Ozone, fun = mean))

## capping de datos extremos
x <- c(-30, 1:10, 20, 30)

boxplot(x)

boxplot.stats(x)$out

boxplot.stats(x, coef = 2)$out # criterio mas amplio


  # con quantiles
qnt <- quantile(x, probs = c(.25, .75), na.rm = T)....

## scale ----
x <- c(1,2,3)
scale(x,center=T)[1:3]

(x - mean(x)) / sd (x) # manual

scale (x, center = F)[1:3]
x / sqrt(sum(x^2) / (length(x)-1)) # manual
  
  
# Analisis ---------

## est descriptiva ------


mtcars %>%
  group_by(cyl) %>%
  summarise_all(list(~mean...))

mtcars %>%
  group_by(cyl) %>%
  summarise(across(where(is.numeric), meand))
  

## est inferencial --------

### wilcox.text()

levels(ToothGrowth$supp)
wilcox.test(len ~ supp, data = ToothGrowth) # paired = T si son pareados


### kruskall.test()

kruskal.test(reight ~)


#### post.hoc

pairwise.wilcox.test(PlantGrowth$weight, PlantGrowth$group,
                     p.adjust.method = "BH")


### friedman()
friedman.test(
  score ~ time | id, data = selfesteem %>%
    tidyr::pivot_longer(cols...)
)
  


### cor()
cor(airquality$Ozone, airquality$Wind, use = " complete.obs")
cor(airquality, use = "complete.obs")


cor.test(airquality$Ozone, airquality$Wind,
         method = "pearson") # sperman si no parametricos

  # con modelo lineal
lm1 <- lm(Volume ~Height, trees)
lm2 <- lm(Girth ~Height, trees)
cor(lm$residuals, lm2$residuals)

chisq.test()

data(mice2, package = "datarium")
t.test(mice$weight, mu = 20)
# 2 muestras
t.test(weight ~group, mice$after, paires = T))


stress.aov <- aov(score ~ treatment + exercise, stress)
stress.aov
summary(stress.aov)

stress.Tukey <- TukeyHSD(stress.aov)
stress.Tukey$Treatment


##### Ejercicio

  # Usa sleep para hacer una t de studens pareada

df <- sleep
head(df)

data(df)
t.test(df$extra ~ df$group, paired = TRUE)

plot(df$extra ~ df$group)

  # haz un anova con warpbreaks y obten el summary()

df2 <- warpbreaks
head(df2)

war.aov <- aov(df2$breaks ~ df2$wool + df2$tension)
war.aov
summary(war.aov)

plot(war.aov)
  
  # realiza las comparaciones post hoc con TukeyHSD()

war.Tukey <- TukeyHSD(war.aov)
war.Tukey$tension



  # discute tus conclusiones



# regresion sencilla
plot(faithful)


m1 <- lm (eruptions ~ waiting, data = faithful)
summary(m1)

  # graficarlo

par(mfrow = c(2,2))
plot(m1)




# regresión múltiple

  # con una variable

lm.mov1 <- lm(Ganancias ~ CostoProd, data = movies)
summary(lm.mov1)

  # 
lm.mov2 <- lm(Ganancias ~ CostoProd + 
                CostPromo,
              data = movies)
summary(lm.mov2)


  # con 3 variables
lm.mov3 <- lm(Ganancias ~ CostoProd + 
                CostPromo +
                LibrosVendidos,
              data = movies)
summary(lm.mov3)

anova(lm.mov1, lm.mov2) # comparar modelos 
anova(lm.mov1, lm.mov3)

  # ejercicio

  # usa swiss para relaizar una regresión múltiple usando Fertility como variable de respuesta
  # usa Agriculture, Examination, Education, Catholic y Infant.Mortality
df <- swiss
head(df)

reg_mult <- lm(Fertility ~ Agriculture +
               Examination +
               Education +
               Catholic +
               Infant.Mortality,
             data = df)

summary(reg_mult)

  # Busca la combinación de variables que mejor predice fertility
  # comparalo con un ANOVA


reg_f_a <- lm(Fertility ~ Agriculture,
               data = df)

reg_f_ex <- lm(Fertility ~ 
                Examination,
              data = df)

reg_f_ed <- lm(Fertility ~ Education,
              data = df)

reg_f_c <- lm(Fertility ~ Catholic,
              data = df)

reg_f_m <- lm(Fertility ~ Infant.Mortality,
              data = df)


  
anova(reg_f_a, reg_f_ex) # comparar modelos 
anova(reg_f_a, reg_f_ed)
anova(reg_f_a, reg_f_c) # comparar modelos 
anova(reg_f_a, reg_f_m)



### Modelos lineales genarilzados

  # combina variables discretas y continuas
  # jespecificar naturaleza de varible de respuesta (gausiana, binomial, etc)
  # cuando es binomial, es regresion logistica


stress.glm <- glm(score ~age, data = stress)
summary(stress.glm)

  # con variable discreta, equivalente a anova
stress.glm <- glm(score ~ treatment, data = stress)

boxplot(score ~)

  # 2 variables, interaccion de categorica por numerica
stress.glm <- glm(score ~ age*treatment, data= stress)
stress(stress.glm)

  ggplot(
    stress,
    aes
  )

  # funcion liga
  
head(infert)

unique (infert$case)

inf.bn <- glm(case ~ age +
                parity +
                education +
                spontaneous,
              data = infert,
              family = binomial())
summary(inf.bn)


###### NOTAAA , Posion con conteos!!!

  # comprobación post hoc

library(multicomp)
summary(glht(inf.no, mcp(mcp(education = "Tukey"))))

  # omdelos genalzados mixtos

  # introducir random effect




####### Output

  # broom


tidy() # contruye df que resume la info contenida en un modelo
augment() 
glance()


## tablas

library(formatable)
library(kableExtra)




## rmarkdown y knitr












