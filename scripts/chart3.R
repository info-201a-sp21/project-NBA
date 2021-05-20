# Line char: Season 2019 champion team (Lakers) PTS throughout the whole season

chart3 <- function(games_data) {
  
# select Lakers games
lakers_games <- games_data %>%
  filter(HOME_TEAM_NAME == "Lakers" | VISITOR_TEAM_NAME == "Lakers") %>%
  select(GAME_DATE_EST, PTS_home, PTS_away, HOME_TEAM_NAME, VISITOR_TEAM_NAME)

# distinguish home/away game
lakers_games$type <- ifelse(lakers_games$HOME_TEAM_NAME == "Lakers",
  "home", "away"
)

# get lakers point for each game
lakers_games$lakers_point <- ifelse(lakers_games$type == "home",
  lakers_games$PTS_home, lakers_games$PTS_away
)

# get opponent's name
lakers_games$opponent <- ifelse(lakers_games$type == "home",
  lakers_games$VISITOR_TEAM_NAME,
  lakers_games$HOME_TEAM_NAME
)

# create a line chart
lakers_pts_chart <- ggplot(data = lakers_games) +
  geom_line(
    mapping = aes(x = as.Date(GAME_DATE_EST), y = lakers_point),
    color = "purple"
  ) +
  geom_point(mapping = aes(
    x = as.Date(GAME_DATE_EST), y = lakers_point,
    text = paste(
      "Date :", as.Date(GAME_DATE_EST),
      "<br>Lakers point: ", lakers_point,
      "<br>Opponent: ", opponent
    )
  ), color = "gold") +
  labs(x = "Date", y = "Lakers points", title = "Season 2019 Lakers points") +
  geom_smooth(
    mapping = aes(x = as.Date(GAME_DATE_EST), y = lakers_point),
    se = FALSE, method = "loess", size = 0.5, color = "red", linetype = "dashed"
  )
lakers_pts_chart <- ggplotly(lakers_pts_chart, tooltip = "text")

return(lakers_pts_chart)
}