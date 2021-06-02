library(shiny)
library(plotly)

# Introduction Page
introduction <- fluidPage(
  h1("NBA Data Analytics"), 
  hr(),
  br(),
  p("Every year, there are lottery gamblelings about the championship before
    the off-season start. We, as NBA fans, want to understand what factors
    could influence the players' performcane which in turn dertermines
    the candidate teams."),
  
  p("This informatics projected is based on dataset of",
    strong("2019 NBA"),
    "games. We analyzed 30 teams' performance based on scores, field goal
     percentage, 3 pointer percentage and other statistics throughout
     the season."),
  
  p("This dataset can be found through this",
    a(href="https://www.kaggle.com/nathanlauga/nba-games", "NBA"), "link."),
  
  h3("Our Analysis"),
  p("Below are the questions we seek to answer from the dataset"),
  p("1. Will the home game and away game affect the teams' performances?
      - comparing the winning rate for each team during home game
      and away game."), 
  p("2. How influential is the three point field goal for the offense side?
      - comparing the average three point field goal during winning and
      losing game with the average field goal during winning and losing
      game in each year."), 
  p("3. How many points the 2019 champion, Lakers, get for each game
    throughout the whole season? Is the champion team perform steady?")
)

# chart1 code
# select input
x_input <- selectInput(
  inputId = "team_name",
  label = "Select one of the TOP 8 teams:",
  choices = list("Bucks", "Heat", "Jazz", "Lakers", "Nuggets", "Pacers", 
                 "Pelicans", "Spurs"),
  selected = "Bucks"
)

# sidebar
chart1_sidebar <- sidebarPanel(
 x_input,
 p("Note: Top 8 teams are measured by average FG percentage")
)

# show plot
chart1_plot <- mainPanel(
  h4("This chart was intended to visualize and compare how top 8 teams who
     have the highest Field Goal percentage in general perform in their home
     and away games. The grouped bar chart shows clearly the difference
     between each team??s Field Goal percentage in their home and away games."),
  br(),
  plotOutput("chart1")
)

chart1_panel <- tabPanel(
  "Chart 1",
  titlePanel("Does Home and Away game affect TOP 8 teams FG percentage?"),
  hr(),
  sidebarLayout(
    chart1_sidebar,
    chart1_plot
  )
)

#chart 2 code
buttons <- radioButtons(
  inputId = "which_games",
  label = "Select which games you would like to view",
  choices = list("All Games", "Home", "Away"),
  selected = "All Games"
)

chart_2_sidebar <- sidebarPanel(
  buttons
)

chart_2_plot <- mainPanel(
  plotOutput("chart2")
)

chart_2_panel <- tabPanel(
  "chart2",
  titlePanel("2019 Lakers 3-Point Percentage vs. League Average"),
  sidebarLayout(
    chart2_sidebar,
    chart2_plot
  ))

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
  h4("This chart is trying to figure out how many points the 2019 champion,
      Lakers, get for each game throughout the whole season. Is the champion
      team perform steady? The red line is the mean PTS from selected range,
      so the more closer to the red line, the more steady they perform."),
  br(),
  plotlyOutput("chart3")
)

chart_3_panel <- tabPanel(
  "Champion Team",
  tags$p(
    id = "titlePanel",
    "Does Champion team's (Lakers) perform steady throughout season 2019?"
  ),
  hr(),
  sidebarLayout(
    chart_3_sidebar,
    chart_3_plot
  )
)

summary <- mainPanel(
  h4("summary table"),
  tableOutput("summary")
)

ui <- fluidPage(
  includeCSS("style.css"),
  navbarPage(title = 'NBA Statistic Prediction',
             tabPanel("INTRODUCTION", introduction),
             tabPanel("TOP 8 TEAMS", chart1_panel),
             tabPanel("LAKERS 3-PT %", chart_2_panel),
             tabPanel("CHAMPION TEAM", chart_3_panel),
             tabPanel("SUMMARY INFORMATION", summary)
  )
)
  
  
  
  
  
  
  
