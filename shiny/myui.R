library(shiny)
library(plotly)

# chart1
# select input
x_input <- selectInput(
  inputId = "team_name",
  label = "Select one of the TOP8 teams:",
  choices = list("Bucks", "Heat", "Jazz", "Lakser", "Nuggets", "Pacers", 
                 "Pelicans", "Spurs"),
  selected = "Bucks"
)

# sidebar
chart1_sidebar <- sidebarPanel(
 x_input,
 p("Note: Top8 teams are measured by average FG percentage")
)

# show plot
chart1_plot <- mainPanel(
  plotOutput("chart1")
)

chart1_panel <- tabPanel(
  "chart1",
  titlePanel("TOP8 teams FG percentage Home vs. Away games"),
  sidebarLayout(
    chart1_sidebar,
    chart1_plot
  )
)

# chart_3 code

# sidebar
chart_3_sidebar <- sidebarPanel(
  h2("here is the lakers game info"),
  sliderInput(
    inputId = "min_score",
    label = "Score above", min = 77, max = 142, value = 100
  )
)

# show the plot
chart_3_plot <- mainPanel(
  plotlyOutput("chart3")
)

chart_3_panel <- tabPanel(
  "Champion Team",
  titlePanel("Champion team¡¯s (Lakers) PTS throughout season 2019"),
  sidebarLayout(
    chart_3_sidebar,
    chart_3_plot
  )
)

ui <- navbarPage('NBA Statistic Prediction',
                 # tabPanel('INTRODUCTION', 'chart 1', 'chart 2', 'chart 3', 'SUMMARY TAKEWAYS')
                 chart1_panel,
                 chart_3_panel
)
