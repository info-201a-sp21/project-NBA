# Scatter Plot: Lakers 3 Point Percentage vs League Average
# load needed packages

chart2 <- function(games_data) {

  nba_games_2020 <- games_data %>% filter(SEASON == "2019")

  # Filter for all Lakers games
  lakers_home_games <- filter(nba_games_2020, HOME_TEAM_ID == "1610612747")
  lakers_away_games <- filter(nba_games_2020, VISITOR_TEAM_ID == "1610612747")
  lakers_games <- full_join(lakers_away_games, lakers_home_games)

  # Compute league averages for 3 point percentage and points
  ave_fg3_pct <- nba_games_2020 %>%
    select(FG3_PCT_home, FG3_PCT_away) %>%
    summarize(
      count = n() * 2,
      FG3_PCT_home = sum(FG3_PCT_home),
      FG3_PCT_away = sum(FG3_PCT_away),
      ave = (FG3_PCT_home + FG3_PCT_away) / count) %>%
    pull(ave)
  ave_fg3_pct <- round(ave_fg3_pct, digits = 3)

  ave_pts <- nba_games_2020 %>%
    select(PTS_home, PTS_away) %>%
    summarize(
      count = n() * 2,
      PTS_home = sum(PTS_home),
      PTS_away = sum(PTS_away),
      ave = (PTS_home + PTS_away) / count) %>%
    pull(ave)
  ave_pts <- round(ave_pts, digits = 3)

  # combine FG3_PCT and PTS for home and away games to use as x and y axes
  fg3_pct <- c(lakers_home_games$FG3_PCT_home, lakers_away_games$FG3_PCT_away)

  pts <- c(lakers_home_games$PTS_home, lakers_away_games$PTS_away)

  # create "colors" vector for Legend in scatterplot
  colors <- c("Lakers Games" = "black", "League Average" = "red")

  # create a scatterplot comparing the League Average
  # (3 Point Percentage and Points) to the stats in each Lakers Game
  lakers_3pt <- ggplot(data = lakers_games) +
    geom_point(mapping = aes(x = fg3_pct, y = pts, color = "Lakers Games")) +
    geom_point(mapping = aes(x = ave_fg3_pct, y = ave_pts,
                             color = "League Average")) +
    labs(y = "Points", x = "3 pt pct") +
    labs(title = "Lakers 3 Point Percentage vs League Average") +
    scale_color_manual(values = colors, guide = "none")

  # Make scatterplot interactive
  lakers_fg3_pct_vs_league <- ggplotly(lakers_3pt)

  return(lakers_fg3_pct_vs_league)
}

