# CORRER ESTE CÓDIGO PARA INSTALAR KERAS Y TENSORFLOW
# solo necesario para funciones de Machine Learning

# instalar paquetes de keras y tensorflow
install.packages(c("keras", "tensorflow"))

# instalar paquete devtools
install.packages("devtools")

# library (devtools)

#instalar keras y tensorflow para Rstudio desde github
devtools::install_github("rstudio/keras", dependencies = TRUE)
devtools::install_github("rstudio/tensorflow", dependencies = TRUE)

library(keras)
install_keras()

library(tensorflow)
install_tensorflow()

# Confirmar que la instalación fue correcta
tf$constant("Hello Tensorflow!")
# debería de producir un código similar al siguiente: 
# tf$constant("Hello Tensorflow!")

# Fuente: https://www.youtube.com/watch?v=cIUg11mAmK4