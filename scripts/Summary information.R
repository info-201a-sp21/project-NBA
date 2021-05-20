summary_info <- function(games_data) {
  info <- list()

  # Filter data to 2019 season, the date is 2020
  nba_games_2020 <- games_data %>% filter(SEASON == "2019")


  ### Summary information: Does home or away, different environment and
  ### audiences, affect players' shooting percentage?
  ### FG_PCT Home vs. Away
  ave_fg_ptc_home <- nba_games_2020 %>%
    select(FG_PCT_home) %>%
    summarize(
      count = n(),
      FG_PCT_home = sum(FG_PCT_home),
      ave = FG_PCT_home / count
    ) %>%
    pull(ave)
  info$ave_fg_ptc_home <- round(ave_fg_ptc_home, digits = 3)


  ave_fg_ptc_away <- nba_games_2020 %>%
    select(FG_PCT_away) %>%
    summarize(
      count = n(),
      FG_PCT_away = sum(FG_PCT_away),
      ave = FG_PCT_away / count
    ) %>%
    pull(ave)
  info$ave_fg_ptc_away <- round(ave_fg_ptc_away, digits = 3)


  ### FG3_PCT Home vs. Away
  ave_fg3_pct_home <- nba_games_2020 %>%
    select(FG3_PCT_home) %>%
    summarize(
      count = n(),
      FG3_PCT_home = sum(FG3_PCT_home),
      ave = FG3_PCT_home / count
    ) %>%
    pull(ave)
  info$ave_fg3_pct_home <- round(ave_fg3_pct_home, digits = 3)

  ave_fg3_pct_away <- nba_games_2020 %>%
    select(FG3_PCT_away) %>%
    summarize(
      count = n(),
      FG3_PCT_away = sum(FG3_PCT_away),
      ave = FG3_PCT_away / count
    ) %>%
    pull(ave)
  info$ave_fg3_pct_away <- round(ave_fg3_pct_away, digits = 3)

  # total number of win and lose
  num_home_win <- nba_games_2020 %>%
    select(HOME_TEAM_WINS) %>%
    filter(HOME_TEAM_WINS == "1") %>%
    summarize(total_win = n()) %>%
    pull(total_win)
  info$num_home_win <- num_home_win

  num_home_loss <- nba_games_2020 %>%
    select(HOME_TEAM_WINS) %>%
    filter(HOME_TEAM_WINS == "0") %>%
    summarize(total_loose = n()) %>%
    pull(total_loose)
  info$num_home_loss <- num_home_loss

  ### Rebound Home vs. Away
  rebound_home <- nba_games_2020 %>%
    select(REB_home) %>%
    summarize(
      count = n(),
      REB_home = sum(REB_home),
      ave = REB_home / count
    ) %>%
    pull(ave)
  info$rebound_home <- round(rebound_home, digits = 3)


  rebound_away <- nba_games_2020 %>%
    select(REB_away) %>%
    summarize(
      count = n(),
      REB_away = sum(REB_away),
      ave = REB_away / count
    ) %>%
    pull(ave)
  info$rebound_away <- round(rebound_away, digits = 3)

  return(info)
}
