library(shiny)
library(plotly)

# chart1 code
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
  tags$p(
    id = "titlePanel",
    "Champion team's (Lakers) PTS throughout season 2019"
  ),
  
#  titlePanel("Champion team's (Lakers) PTS throughout season 2019"),
  
  h4("This chart is trying to figure out how many points the 2019 champion, Lakers,
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
    "games. We analyzed 30 teams' performance based on scores, field goal percentage, 3 pointer percentage and other statistics throughout the season."),
  
  p("This dataset can be found through this", a(href="https://www.kaggle.com/nathanlauga/nba-games", "NBA"), "link."),
  
  h3("Our Analysis"),
  p("Below are the questions we seek to answer from the dataset"),
  p("1. Will the home game and away game affect the teams' performances?
      - comparing the winning rate for each team during home game and away game."), 
  p("2. How influential is the three point field goal for the offense side?
      - comparing the average three point field goal during winning and losing game with the average field goal during winning and losing game in each year."), 
  p("3. How many points the 2019 champion, Lakers, get for each game throughout the whole season? Is the champion team perform steady?")
)


ui <- fluidPage(
  includeCSS("style.css"),
  navbarPage(title = 'NBA Statistic Prediction',
             tabPanel("INTRODUCTION", introduction), tabPanel("Chart 1", chart1_panel), tabPanel("Chart 2", "content"), tabPanel("CHAMPION TEAM", chart_3_panel), tabPanel("SUMMARY INFORMATION")
  )
)
  
  
  
  
  
  
  
