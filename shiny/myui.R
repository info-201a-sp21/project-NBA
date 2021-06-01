library(shiny)
library(plotly)


# chart_3 code

# get opponants' names
teams <- unique(games_data$HOME_TEAM_NAME)

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
                 chart_3_panel
)