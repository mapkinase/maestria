# Fuente: https://deanattali.com/blog/building-shiny-apps-tutorial/

# Install ----
install.packages("shiny")

# To ensure you successfully installed Shiny, try running one of the demo apps.

library(shiny)
runExample("01_hello")

# App basics ----

# 2 parts
#   a web page that shows the app to the user (UI) - HTML
#   a computer that powers the app (server)

## Create an empty app ----

ui <- fluidPage()
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

# We are going to copy this template in a new file named app.R
# important that the name of the file is app.R, otherwise it would not be recognized as a Shiny app
# You should not have any R code after the shinyApp(ui = ui, server = server) line. That line needs to be the last line in your file
# It is good practice to place this app in its own folder, and not in a folder that already has other R scripts or files, unless those other files are used by your app
 
## Alternative - separate UI and server files----

# two files: ui.R and server.R
# you do not need to include the shinyApp(ui = ui, server = server) line.

 
## Alternative: Let RStudio fill out a Shiny app template ----

# File > New File > Shiny Web App…. 


# Loading the dataset ----

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)











