# Double bar graph: FG Percentage for home vs. away.

chart1 <- function(games_data) {

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
    labs(
      title = "Top8 teams FG percentage home vs. away game",
      x = "Team name", y = "Teams average FG percentage"
    ) +
    theme(legend.title = element_blank())

  return(top8_teams_avg_fg_chart)
}
