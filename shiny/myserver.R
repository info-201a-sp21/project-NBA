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
  output$chart1 <- renderPlot({
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
        aes(y = teams_avg_fg, x = team_name)
      ) +
      geom_bar(aes(fill = factor(home_away,
                                 labels = c("home games", "away games")
      )),
      position = "dodge", stat = "identity"
      ) +
      coord_cartesian(ylim=c(0.4,0.5)) +
      labs(
        x = "Team name", y = "Teams average FG percentage"
      ) +
      gghighlight(team_name == input$team_name, use_direct_label = FALSE) +
      theme(legend.title = element_blank())
    
    return(top8_teams_avg_fg_chart)
  })
  
  #chart 2
  output$chart2 <- renderPlotly({
    
    # Filter for all Lakers games
    lakers_home_games <- filter(games_data_2019, HOME_TEAM_ID == "1610612747")
    lakers_away_games <- filter(games_data_2019,
                                VISITOR_TEAM_ID == "1610612747")
    lakers_games <- full_join(lakers_away_games, lakers_home_games)
    
    # Compute league averages for 3 point percentage and points
    ave_fg3_pct <- games_data_2019 %>%
      select(FG3_PCT_home, FG3_PCT_away) %>%
      summarize(
        count = n() * 2,
        FG3_PCT_home = sum(FG3_PCT_home),
        FG3_PCT_away = sum(FG3_PCT_away),
        ave = (FG3_PCT_home + FG3_PCT_away) / count) %>%
      pull(ave)
    ave_fg3_pct <- round(ave_fg3_pct, digits = 3)
    
    ave_pts <- games_data_2019 %>%
      select(PTS_home, PTS_away) %>%
      summarize(
        count = n() * 2,
        PTS_home = sum(PTS_home),
        PTS_away = sum(PTS_away),
        ave = (PTS_home + PTS_away) / count) %>%
      pull(ave)
    ave_pts <- round(ave_pts, digits = 3)
    
    # combine FG3_PCT and PTS for home and away games to use as x and y axes
    away_fg3_pct <- lakers_away_games$FG3_PCT_away
    
    away_pts <- lakers_away_games$PTS_away
    
    home_fg3_pct <- lakers_home_games$FG3_PCT_home
    
    home_pts <- lakers_home_games$PTS_home
    
    fg3_pct <- c(lakers_home_games$FG3_PCT_home, lakers_away_games$FG3_PCT_away)
    
    pts <- c(lakers_home_games$PTS_home, lakers_away_games$PTS_away)
    
    # create "colors" vector for Legend in scatterplot
    colors <- c("Lakers Games" = "black", "League Average" = "red",
                "Lakers Away Games" = "black", "Lakers Home Games" = "black")
    
    # create a scatterplot comparing the League Average
    # (3 Point Percentage and Points) to the stats in each Lakers Game
    lakers_3pt_away <- ggplot(data = lakers_away_games) +
      geom_point(mapping = aes(x = away_fg3_pct, y = away_pts,
                               color = "Lakers Away Games")) +
      geom_point(mapping = aes(x = ave_fg3_pct, y = ave_pts,
                               color = "League Average")) +
      labs(y = "Points (Away)", x = "3 pt pct (Away)") +
      labs(title = "Lakers 3 Point Percentage (Away) vs League Average") +
      scale_color_manual(values = colors, guide = "none")
    
    lakers_3pt_home <- ggplot(data = lakers_home_games) +
      geom_point(mapping = aes(x = home_fg3_pct, y = home_pts,
                               color = "Lakers Home Games")) +
      geom_point(mapping = aes(x = ave_fg3_pct, y = ave_pts,
                               color = "League Average")) +
      labs(y = "Points (Home)", x = "3 pt pct (Home)") +
      labs(title = "Lakers 3 Point Percentage (Home) vs League Average") +
      scale_color_manual(values = colors, guide = "none")
    
    lakers_3pt <- ggplot(data = lakers_games) +
      geom_point(mapping = aes(x = fg3_pct, y = pts, color = "Lakers Games")) +
      geom_point(mapping = aes(x = ave_fg3_pct, y = ave_pts,
                               color = "League Average")) +
      labs(y = "Points", x = "3 pt pct") +
      labs(title = "Lakers 3 Point Percentage vs League Average") +
      scale_color_manual(values = colors, guide = "none")
    
    # Make scatterplot interactive
    lakers_fg3_pct_vs_league <- ggplotly(lakers_3pt)
    
    away_lakers_fg3_pct_vs_league <- ggplotly(lakers_3pt_away)
    
    home_lakers_fg3_pct_vs_league <- ggplotly(lakers_3pt_home)
    
    if(input$which_games == "All Games") {
      return(lakers_fg3_pct_vs_league)
    }
    
    else if(input$which_games == "Away") {
      return(away_lakers_fg3_pct_vs_league)
    }
    
    else if(input$which_games == "Home") {
      return(home_lakers_fg3_pct_vs_league)
    }
    
  }
  )
  
  
  #chart 3
  output$chart3 <- renderPlotly({
    
    # select Lakers games
    lakers_games <- games_data %>%
      filter(HOME_TEAM_NAME == "Lakers" | VISITOR_TEAM_NAME == "Lakers") %>%
      select(GAME_DATE_EST, PTS_home, PTS_away,
             HOME_TEAM_NAME, VISITOR_TEAM_NAME)
    
    # distinguish home/away game
    lakers_games$type <- ifelse(lakers_games$HOME_TEAM_NAME == "Lakers",
      
                                                          "Home", "Away"
    )
    
    # get lakers point for each game
    lakers_games$lakers_point <- ifelse(lakers_games$type == "Home",
                                        lakers_games$PTS_home,
                                        lakers_games$PTS_away
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
      labs(x = "Date", y = "Lakers points",
           title = "Season 2019 Lakers points") +
      geom_line(
        mapping = aes(x = as.Date(GAME_DATE_EST), y = mean(lakers_point)),
        color = "red",
        linetype = "dashed"
      )
    
    lakers_pts_chart <- ggplotly(lakers_pts_chart, tooltip = "text")
    
    return(lakers_pts_chart)
    
  })
  
  #summary table
  output$summary <-renderTable({
    
    # Table
    all_teams_data <- games_data %>%
      select(HOME_TEAM_NAME, REB_home, FT_PCT_home, FG_PCT_home) %>%
      group_by(HOME_TEAM_NAME) %>%
      summarize(
        Rebound = sum(REB_home),
        Free_Throw_Percentage = sum(FT_PCT_home),
        Field_Goal_Percentage = sum(FG_PCT_home)
      ) %>%
      arrange(Field_Goal_Percentage)
    names(all_teams_data)[1:4] <- c("Team Name", "Rebounds", "Free Throw
                                    Percentage", "Field Goal Percentage" )
    
    return(all_teams_data)
  },
  striped = TRUE)
}

