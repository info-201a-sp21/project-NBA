# Scatter Plot: Lakers 3 Point Percentage vs League Average
# load needed packages
games_data <- read.csv("../data/games.csv", stringsAsFactors = FALSE)

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

  return(lakers_fg3_pct_vs_league)
}

print(home_lakers_fg3_pct_vs_league)
