### Summary table

summary_table <- function(games_data) {

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

  return(all_teams_data)
}
