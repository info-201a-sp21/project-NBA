library(shiny)
library(plotly)

# Introduction Page
introduction <- fluidPage(
  tags$p(
    id = "titlePanel",
    "NBA Data Analytics"
  ),
  hr(),
  br(),
  p("Every year, there are lottery gamblelings about the championship before
    the off-season start. We, as NBA fans, want to understand what factors
    could influence the players' performcane which in turn dertermines
    the candidate teams."),
  p(
    "This informatics projected is based on dataset of",
    strong("2019 NBA"),
    "games. We analyzed 30 teams' performance based on scores, field goal
     percentage, 3 pointer percentage and other statistics throughout
     the season."
  ),
  p(
    "This dataset can be found through this",
    a(href = "https://www.kaggle.com/nathanlauga/nba-games", "NBA"), "link."
  ),
  h3("Our Analysis"),
  p("Below are the questions we seek to answer from the dataset"),
  p("1. Does Home or Away games affect TOP 8 teams' FG percentage?"),
  p("2. How does the 2019 Lakers 3-Point Percentage compare to the League
    Average?"),
  p("3. How many points the 2019 champion, Lakers, get for each game
    throughout the whole season? Is the champion team perform steady?"),
  br(),
  img(src = "teams_logo.jpg", width = "60%", height = "60%")
)

# chart1 code
# select input
team_name_input <- selectInput(
  inputId = "team_name",
  label = "Select one of the TOP 8 teams to see the difference:",
  choices = list(
    "Bucks", "Heat", "Jazz", "Lakers", "Nuggets", "Pacers",
    "Pelicans", "Spurs"
  ),
  selected = "Bucks"
)

values_or_diff <- radioButtons(
  inputId = "values_difference",
  label = h3("Show values or difference:"),
  choices = list("Values" = "values", "Difference" = "difference")
)

# create sidebar
chart1_sidebar <- sidebarPanel(
  team_name_input,
  textOutput("difference"),
  br(),
  p("Note: Top 8 teams are measured by average FG percentage."),
  br(),
  values_or_diff
)

# show plot
chart1_plot <- mainPanel(
  h4("This chart was intended to visualize and compare how top 8 teams who
     have the highest Field Goal percentage in general perform in their home
     and away games. The grouped bar chart shows clearly the difference
     between each team's Field Goal percentage in their home and away games."),
  br(),
  plotOutput("chart1")
)

# consolidate
chart1_panel <- tabPanel(
  "Chart 1",
  titlePanel("Does Home or Away games affect TOP 8 teams' FG percentage?"),
  hr(),
  sidebarLayout(
    chart1_sidebar,
    chart1_plot
  )
)

# chart 2 code
# create radio buttons
buttons <- radioButtons(
  inputId = "which_games",
  label = "Select which games you would like to view",
  choices = list("All Games", "Home", "Away"),
  selected = "All Games"
)

# create sidebar
chart_2_sidebar <- sidebarPanel(
  buttons
)

# show plot
chart_2_plot <- mainPanel(
  h4("The purpose of this chart was to visuzlize the relationship between
     the points the Lakers, who won the NBA Title in 2019, scored and their
     3 point percentage. The league average on 3 point percentage and points
     is also plotted to show how much better the Lakers are in each of these
     categories. There are also plots with the Lakers home and away games
     to visulaize the difference in scoring based on where they are playing
     (if there is a difference at all). The goal of this plot was to see if
     the Lakers 3 point percentage was to blame for their success
     in the 2019 season."),
  br(),
  plotlyOutput("chart2")
)

# consolidate
chart_2_panel <- tabPanel(
  "Chart 2",
  titlePanel("How does the 2019 Lakers 3-Point Percentage
             compare to the League Average?"),
  hr(),
  sidebarLayout(
    chart_2_sidebar,
    chart_2_plot
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
  h4("This chart is trying to figure out how many points the 2019 champion,
      Lakers, get for each game throughout the whole season. Is the champion
      team perform steady? The red line is the mean PTS from selected range,
      so the more closer to the red line, the more steady they perform."),
  br(),
  plotlyOutput("chart3")
)

# consolidate
chart_3_panel <- tabPanel(
  "Champion Team",
  titlePanel("Does Champion team's (Lakers) perform steady throughout
             season 2019?"),
  hr(),
  sidebarLayout(
    chart_3_sidebar,
    chart_3_plot
  )
)

# create summary page
summary <- fluidPage(
  h1("First Takeaway"),
  hr(),
  p("From the first graph, 7 out of 8 teams have a higher home games field goal
    percentages than away games. Last year's championship Lakers has a field
    goal percetage difference of 0.21, which is significantly lower than Heat,
    Nuggets, Jazz. However, it is slightly higher than Spurs and Pacers.
    Therefore, we can conclude that field goal percentage is not the only
    factor that could determine the result of the games."),
  plotOutput("chart1_takeaway"),
  br(),
  h1("Second Takeaway"),
  hr(),
  p("The takeaway from the second chart is that Lakers's 3 point percentage
    during away games was more closely to the league's average.
    Lakers performed better at home game than away games during the 2019 season.
    However, Lakers did not have the highest 3 point percentage, thus only 3
    point percentage does not have a significant role in determine the result
    of the games."),
  plotlyOutput("chart2_takeaway"),
  br(),
  h1("Third Takeaway"),
  hr(),
  p("The last takeaway from the Lakers's every game points during 2019 season
    is that at the 115 points threshold, the Lakers performed close to its
    average 113 whether it is home or away games. Although, at the beginning
    of the season October 12, 2019, the Lakers had its lowest points, this is
    probably because they had not adjust to the intense competition. One month
    laster, they started to have a steady performance and got close to the
    team's average. "),
  plotlyOutput("chart3_takeaway"),
  br(),
  h1("Summary Table"),
  p("The takeaway from the second chart is that Lakers's 3 point percentage
    during away games was more closely to the league's average. Lakers
    performed better at home game than away games during the 2019 season.
    However, Lakers did not have the highest 3 point percentage, thus only 3
    point percentage does not have a significant role in determine the result
    of the games."),
  hr(),
  tableOutput("summary")
)

# consolidate into ui page
ui <- fluidPage(
  includeCSS("style.css"),
  navbarPage(
    title = "2019 NBA Season Statistical Analysis",
    tabPanel("INTRODUCTION", introduction),
    tabPanel("HOME VS. AWAY FG %", chart1_panel),
    tabPanel("LAKERS 3-PT %", chart_2_panel),
    tabPanel("CHAMPION TEAM", chart_3_panel),
    tabPanel("SUMMARY INFORMATION", summary)
  )
)
