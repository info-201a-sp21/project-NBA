# server.R
library(dplyr)
library(tidyr)
library(ggplot2)
library(gghighlight)

# load and clean data sets
games_data <- read.csv("../data/games.csv", stringsAsFactors = FALSE)
games_data_2019 <- games_data %>%
  filter(SEASON == "2019")
teams_data <- read.csv("../data/teams.csv", stringsAsFactors = FALSE)
teams_data <- teams_data %>%
  select(TEAM_ID, NICKNAME)

# add team names for home teams
teams_data <- teams_data %>%
  rename(HOME_TEAM_ID = TEAM_ID)
add_home_team_name <-
  left_join(games_data_2019, teams_data, by = "HOME_TEAM_ID") %>%
  rename(HOME_TEAM_NAME = NICKNAME)

# add team names for visitor teams
teams_data <- teams_data %>%
  rename(VISITOR_TEAM_ID = HOME_TEAM_ID)
games_data <-
  left_join(add_home_team_name, teams_data, by = "VISITOR_TEAM_ID") %>%
  rename(VISITOR_TEAM_NAME = NICKNAME)

server <- function(input, output) {
  
  #chart1
  output$chart1 <- ({
    # summarize average FG percentage for home teams & visitor teams
    home_team_avg_fg <- games_data %>%
      group_by(HOME_TEAM_NAME) %>%
      summarize(avg_fg_perc = mean(FG_PCT_home)) %>%
      rename(home_avg_fg_perc = avg_fg_perc, team_name = HOME_TEAM_NAME)
    
    visitor_team_avg_fg <- games_data %>%
      group_by(VISITOR_TEAM_NAME) %>%
      summarize(avg_fg_perc = mean(FG_PCT_away)) %>%
      rename(away_avg_fg_perc = avg_fg_perc, team_name = VISITOR_TEAM_NAME)
    
    # combine the average FG percentage for home teams & visitor teams
    teams_avg_fg <-
      left_join(home_team_avg_fg, visitor_team_avg_fg, by = "team_name")
    
    # calculate average FG percentage of both home and away games
    # for each team; select top 8 teams with highest average value
    top8_teams_avg_fg <- teams_avg_fg %>%
      mutate(average = (home_avg_fg_perc + away_avg_fg_perc) / 2) %>%
      arrange(-average) %>%
      top_n(8) %>%
      select(team_name, home_avg_fg_perc, away_avg_fg_perc)
    
    # reshape data set
    top8_teams_avg_fg <-
      gather(top8_teams_avg_fg, home_away, teams_avg_fg, -team_name)
    
    # create a grouped barchart for teams' Top8 average field goal percentage
    top8_teams_avg_fg_chart <-
      ggplot(
        top8_teams_avg_fg,
        aes(y = teams_avg_fg, x = input$team_name)
      ) +
      geom_bar(aes(fill = factor(home_away,
                                 labels = c("home games", "away games")
      )),
      position = "dodge", stat = "identity"
      ) +
      scale_y_continuous(limits = c(0.4, 0.5)) +
      labs(
        title = "Top8 teams FG percentage home vs. away game",
        x = "Team name", y = "Teams average FG percentage"
      ) +
      theme(legend.title = element_blank())
    
    return(top8_teams_avg_fg_chart)
  })
  
  #chart 3
  output$chart3 <- renderPlotly({
    
    # select Lakers games
    lakers_games <- games_data %>%
      filter(HOME_TEAM_NAME == "Lakers" | VISITOR_TEAM_NAME == "Lakers") %>%
      select(GAME_DATE_EST, PTS_home, PTS_away, HOME_TEAM_NAME, VISITOR_TEAM_NAME)
    
    # distinguish home/away game
    lakers_games$type <- ifelse(lakers_games$HOME_TEAM_NAME == "Lakers",
                                "Home", "Away"
    )
    
    # get lakers point for each game
    lakers_games$lakers_point <- ifelse(lakers_games$type == "Home",
                                        lakers_games$PTS_home, lakers_games$PTS_away
    )
    
    # get opponent's name
    lakers_games$opponent <- ifelse(lakers_games$type == "Home",
                                    lakers_games$VISITOR_TEAM_NAME,
                                    lakers_games$HOME_TEAM_NAME
    )
    
    # create a line chart
    lakers_pts_chart <- ggplot(data = lakers_games) +
      geom_line(
        mapping = aes(x = as.Date(GAME_DATE_EST), y = lakers_point),
        color = "purple"
      ) +
      gghighlight(lakers_point >= input$min_score, use_direct_label = FALSE) +
      geom_point(mapping = aes(
        x = as.Date(GAME_DATE_EST), y = lakers_point,
        text = paste(
          "Date :", as.Date(GAME_DATE_EST),
          "<br>Lakers point: ", lakers_point,
          "<br>Opponent: ", opponent,
          "<br>Type: ", type, "game"
        )
      ), color = "gold") +
      labs(x = "Date", y = "Lakers points", title = "Season 2019 Lakers points") +
      geom_line(
        mapping = aes(x = as.Date(GAME_DATE_EST), y = input$min_score),
        color = "red",
        linetype = "dashed"
      )
    
    lakers_pts_chart <- ggplotly(lakers_pts_chart, tooltip = "text")
    
    return(lakers_pts_chart)
    
  })
}

