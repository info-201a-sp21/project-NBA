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
  titlePanel("Champion team's (Lakers) PTS throughout season 2019"),
  h4("This chart is try to figure out how many points the 2019 champion, Lakers,
     get for each game throughout the whole season. Is the champion team perform
     steady?"),
  sidebarLayout(
    chart_3_sidebar,
    chart_3_plot
  )
)



# Introduction Page
introduction <- fluidPage(
  h1("NBA Data Analytics"), 
  hr(),
  br(),
  p("Every year, there are lottery gamblelings about the championship before the off-season start. We, as NBA fans, want to 
    understand what factors could influence the players' performcane which in turn dertermines the candidate teams."),
  
  p("This informatics projected is based on dataset of",
    strong("2019 NBA"),
    "games. We analyzed 30 teams’ performance based on scores, field goal percentage, 3 pointer percentage and other statistics throughout the season."),
  
  p("This dataset can be found through this", a(href="https://www.kaggle.com/nathanlauga/nba-games", "NBA"), "link."),
  
  h3("Our Analysis"),
  p("Below are the questions we seek to answer from the dataset"),
  p("1. Will the home game and away game affect the teams' performances?
      - comparing the winning rate for each team during home game and away game."), 
  p("2. How influential is the three point field goal for the offense side?
      - comparing the average three point field goal during winning and losing game with the average field goal during winning and losing game in each year."), 
  p("3. Is the team with the most regular season wins likely to win the final championship?
      - comparing whether the team with the most wins in regular season is the final champion in each year.")
)


ui <- navbarPage(title = 'NBA Statistic Prediction',
                 tabPanel("INTRODUCTION", introduction), tabPanel("Chart 1", "content"), tabPanel("Chart 2", "content"), tabPanel("CHAMPION TEAM", chart_3_panel), tabPanel("SUMMARY INFORMATION")
                 
)
