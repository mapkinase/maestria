library(ggplot2)
library(RColorBrewer)

Tracking <- read.csv("coordenadas/coordenadas_1.csv") 




library(reshape2)
library(plotly)

df <- Tracking
head(df)
colnames(df)

p <- ggplot(df, aes(Centre.position.X,Centre.position.Y)) +
  geom_raster(aes(fill=Time)) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  labs(x="Posición en X",
       y="Posición en Y",
       #fill = "Tiempo")
       title = "Mapa de Calor OF") +
  theme(text = element_text(family = 'Fira Sans'),
        plot.title = element_text(hjust = 0.5))



ggplotly(p)

### demo

library(reshape2)
library(plotly)

demo <- melt(volcano)
head(demo)

p <- ggplot(df, aes(Var1, Var2)) +
  geom_raster(aes(fill=value)) +
  labs(x="West to East",
       y="North to South",
       title = "Elevation map of Maunga Whau")

ggplotly(p)


## con código de chatgtp

# Load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)

# Load data
files <- list.files(pattern = "\\.csv$")
data <- lapply(files, read.csv)
data <- bind_rows(data)

# Calculate average x and y coordinates
averages <- data %>%
  group_by(Time) %>%
  summarize(mean_x = mean(Centre.position.X), mean_y = mean(Centre.position.Y))

# Plot average x and y coordinates

plot(averages$mean_x, 
     averages$mean_y, 
     type = "p")

head(averages)

p <- ggplot(averages, aes(mean_x,mean_y)) +
  geom_raster(aes(fill=Time)) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  labs(x="Posición en X",
       y="Posición en Y",
       #fill = "Tiempo")
       title = "Mapa de Calor OF") +
  theme(text = element_text(family = 'Fira Sans'),
        plot.title = element_text(hjust = 0.5))

ggplotly(p)


# otro código
  # https://rpubs.com/JoFrhwld/trajectories

library(ggplot2)
library(grid)

library(dplyr)
library(knitr)


ay <- read.csv("coordenadas_1.csv")

head(ay) %>% kable

ggplot(ay, aes(Centre.position.X, Centre.position.Y))+
  geom_path(aes(), arrow = arrow())+
  scale_y_reverse()+
  scale_x_reverse()+
  coord_fixed()

  # este código combinado con el de chatgtp

library(dplyr)

files <- list.files(pattern = "\\.csv$")
data <- lapply(files, read.csv)
data <- bind_rows(na.omit(data))

averages <- data %>%
  group_by(Time) %>%
  summarize(mean_x = mean(Centre.position.X), mean_y = mean(Centre.position.Y))

averages$Time <- unique(data$Time)

plot(averages$Time, averages$mean_x, type = "p", xlab = "Time", ylab = "Mean x")
lines(averages$Time, averages$mean_y, col = "red")
legend("topright", c("Mean x", "Mean y"), lty = c(1, 1), col = c("black", "red"))

  # otra prueba

ggplot(averages, aes(mean_x, mean_y))+
  geom_path(aes(group = Time), arrow = arrow())+
  scale_y_reverse()+
  scale_x_reverse()+
  coord_fixed()

  # Con paquete mousetrap

install.packages("mousetrap")
library(mousetrap)
library(readr)


  # Ejemplo de mousetrap
  # Create data in long format for test purposes

mt_data_long <- mt_export_long(mt_example,
                               use2_variables=c("subject_nr","Condition"))

head(mt_data_long)

  # Import the data using mt_import_long

mt_data <- mt_import_long(mt_data_long)

mt_heatmap(mt_data, xres=500, n_shades=5,mean_image=0.2)

mt_heatmap_ggplot(mt_data, xres=500, n_shades=5,mean_image=0.2)

  ## Not run:
  # Import a hypothetical dataset that contains the
  # custom mouse-tracking variables angle and velocity

mt_data <- mt_import_long(exp_data,
                          add_labels= c("angle", "velocity"))


  # Prueba Mia

df <- read.csv("coordenadas_1.csv")
head(df)
colnames(df)

head(averages)


mt_import_long(df, xpos_label = "Centre.position.X", ypos_label = "Centre.position.Y",
               zpos_label = NULL, timestamps_label = "Time", add_labels = NULL,
               mt_id_label = "mt_id", mt_seq_label = "mt_seq", reset_timestamps = TRUE,
               verbose = TRUE)

mt_import_long(df, xpos_label = "Centre.position.X", ypos_label = "Centre.position.Y")

mt_import_long(df,)

mt_data <- mt_import_long(
  df,
  xpos_label = "Centre.position.X",
  ypos_label = "Centre.position.Y",
  timestamps_label = "Time",
  mt_id_label = "mt_id"
)

head(mt_data)

mt_heatmap_ggplot(data, use = "trajectories", ...)










  # Con Lattice
  # 
  # https://r-graph-gallery.com/27-levelplot-with-lattice.html
  # 


library("lattice")

  # Con un solo archivo
  
levelplot(Time ~ Centre.position.X*Centre.position.Y, data=data  ,xlab="X",
          main="Test")

library(viridisLite)
coul <- viridis(100)
levelplot(Time ~ Centre.position.X*Centre.position.Y, data=data, col.regions = coul) 

library(RColorBrewer)
coul2 <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
levelplot(Time ~ Centre.position.X*Centre.position.Y, data=data, col.regions = coul2) 

  # con average

levelplot(Time ~ Centre.position.X*Centre.position.Y, data=data, xlab="Centre.position.X",
          main="", col.regions = coul)


  # Mapa 2d densidad
  # https://r-graph-gallery.com/2d-density-chart.html

library(tidyverse)
head(df)
head(averages)

  # With scatterplot

ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y) ) +
  geom_point()

ggplot(data, aes(x=mean_x, y=mean_y) ) +
  geom_point()


  # esto crea problemas de overlapping
  # por esto, es mejor usar mapa de densidad en 2D
  # este divide el area del plot en fragments que representan el número de puntos en este fragmento
  # It is a 2d version of the classic histogram
  # It is called using the geom_bin_2d() function. This function offers a bins argument that controls the number of bins you want to display.
  # 

ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y) ) +
  geom_bin2d(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


  # alternative is to divide the plot area in a multitude of hexagons
  # it is thus called a hexbin chart, and is made using the geom_hex() function
  # This function provides the bins argument as well,

ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y) ) +
  geom_hex(bins = 85) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


  # 2d distribution with geom_density_2d or stat_density_2d
  # 
  # As you can plot a density chart instead of a histogram, it is possible to compute a 2d density and represent it
  # 
  # you can show the contour of the distribution, or the area, or use the raster function

  # Show the contour only
ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y) ) +
  geom_density_2d()

# Show the area only
ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

# Area + contour
ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

# Using raster
ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  # scale_fill_distiller(palette = 4, direction = -1) + # The direction argument allows to reverse the palette
  scale_fill_distiller(palette= "Spectral", direction=1) + # call the palette using a name
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )


  # Add Scatterplot to 2d density chart

ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y)) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) 
#  geom_point(colour = "#aea79f", alpha = 0.3)



  ## Contours of a 2D density estimate
  ## 
  ## https://ggplot2.tidyverse.org/reference/geom_density_2d.html

  # geom_density_2d(
  # geom_density_2d(
  # geom_density_2d(
  # geom_density_2d(

m <- ggplot(data, aes(x=Centre.position.X, y=Centre.position.Y)) 
  # geom_point() 
  # xlim(0.5, 6) +
  # ylim(40, 110)

# contour lines
m + geom_density_2d()

# \donttest{
# contour bands
m + geom_density_2d_filled(alpha = 0.5)

# contour bands and contour lines
m + geom_density_2d_filled(alpha = 1) +
  geom_density_2d(linewidth = 0.5, colour = "#aea79f")


# If you map an aesthetic to a categorical variable, you will get a
# set of contours for each value of that variable
m + geom_density_2d(aes(colour = Time))

# If you draw filled contours across multiple facets, the same bins are
# used across all facets
d + geom_density_2d_filled() + facet_wrap(vars(cut))

# If you want to make sure the peak intensity is the same in each facet,
# use `contour_var = "ndensity"`.
d + geom_density_2d_filled(contour_var = "ndensity") + facet_wrap(vars(cut))
                           
# If you want to scale intensity by the number of observations in each group,
# use `contour_var = "count"`.
d + geom_density_2d_filled(contour_var = "count") + facet_wrap(vars(cut))  

# If we turn contouring off, we can use other geoms, such as tiles:
d + stat_density_2d(
  geom = "raster",
  aes(fill = after_stat(density)),
  contour = FALSE
) + scale_fill_viridis_c()

# Or points:
d + stat_density_2d(geom = "point", aes(size = after_stat(density)), n = 20, contour = FALSE)