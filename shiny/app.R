library(shiny)
library(plotly)

#load the ui and server
source("myui.R")
source("myserver.R")

# Create a new `shinyApp()` using the loaded `ui` and `server` variables
shinyApp(ui = ui, server = server)