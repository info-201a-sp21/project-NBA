# Scatter Plot: Lakers 3 Point Percentage vs League Average
# load needed packages

chart2 <- function(games_data) {
  
  NBA_games_2020 <- games %>% filter(SEASON == "2019")
  
  # Filter for all Lakers games
  lakers_home_games <- filter(NBA_games_2020, HOME_TEAM_ID == "1610612747")
  
  lakers_away_games <- filter(NBA_games_2020, VISITOR_TEAM_ID == "1610612747")
  
  lakers_games <- full_join(lakers_away_games, lakers_home_games)
  
  # Compute league averages for 3 point percentage and points
  Ave_FG3_PCT <- NBA_games_2020 %>%
    select(FG3_PCT_home, FG3_PCT_away) %>%
    summarize(
      count = n() * 2,
      FG3_PCT_home = sum(FG3_PCT_home),
      FG3_PCT_away = sum(FG3_PCT_away),
      ave = (FG3_PCT_home + FG3_PCT_away) / count) %>%
    pull(ave)
  Ave_FG3_PCT <- round(Ave_FG3_PCT, digits = 3)
  
  Ave_PTS <- NBA_games_2020 %>%
    select(PTS_home, PTS_away) %>%
    summarize(
      count = n() * 2,
      PTS_home = sum(PTS_home),
      PTS_away = sum(PTS_away),
      ave = (PTS_home + PTS_away) / count) %>%
    pull(ave)
  Ave_PTS <- round(Ave_PTS, digits = 3)
  
  # combine FG3_PCT and PTS for home and away games to use as x and y axes
  FG3_PCT <- c(lakers_home_games$FG3_PCT_home, lakers_away_games$FG3_PCT_away)
  
  PTS <- c(lakers_home_games$PTS_home, lakers_away_games$PTS_away)
  
  # create "colors" vector for Legend in scatterplot
  colors <- c("Lakers Games" = "black", "League Average" = "red")
  
  # create a scatterplot comparing the League Average 
  # (3 Point Percentage and Points) to the stats in each Lakers Game
  Lakers_3pt <- ggplot(data = lakers_games) +
    geom_point(mapping = aes(x = FG3_PCT, y = PTS, color = "Lakers Games")) +
    geom_point(mapping = aes(x = Ave_FG3_PCT, y = Ave_PTS,
                             color = "League Average")) +
    labs(y = "Points", x = "3 pt pct") +
    labs(title = "Lakers 3 Point Percentage vs League Average") +
    scale_color_manual(values = colors, guide = "none")
  
  # Make scatterplot interactive
  Lakers_FG3_PCT_vs_League <- ggplotly(Lakers_3pt) 
  
  return(Lakers_FG3_PCT_vs_League)
}