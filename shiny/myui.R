library(shiny)
library(plotly)


# chart_3 code
# sidebar
chart_3_sidebar <- sidebarPanel(
  sliderInput(
    inputId = "min_score",
    label = "Lakers' Score above", min = 77, max = 142, value = 105
  )
)

# show the plot
chart_3_plot <- mainPanel(
  plotlyOutput("chart3")
)

chart_3_panel <- tabPanel(
  "Champion Team",
  titlePanel("Champion team¡¯s (Lakers) PTS throughout season 2019"),
  h4("This chart is trying to figure out how many points the 2019 champion, 
      Lakers, get for each game throughout the whole season. Is the champion 
      team perform steady?"),
  sidebarLayout(
    chart_3_sidebar,
    chart_3_plot
  )
)

ui <- navbarPage('NBA Statistic Prediction',
                 # tabPanel('INTRODUCTION', 'chart 1', 'chart 2', 'chart 3', 'SUMMARY TAKEWAYS')
                 chart_3_panel
)