# Line char: Season 2019 champion team (Lakers) PTS throughout the whole season

library(dplyr)
library(ggplot2)
library(plotly)

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
ggplotly(lakers_pts_chart, tooltip = "text")

