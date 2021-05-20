### Summary table

summary_table <- function(games_data) {

  # Table
  all_teams_data <- games_data %>%
    select(HOME_TEAM_NAME, REB_home, FT_PCT_home, FG_PCT_home) %>%
    group_by(HOME_TEAM_NAME) %>%
    summarize(
      REB_home = sum(REB_home),
      FT_PCT_home = sum(FT_PCT_home),
      FG_PCT_home = sum(FG_PCT_home)
    ) %>%
    arrange(FG_PCT_home)

  return(all_teams_data)
}
