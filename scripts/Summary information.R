
# Read files
games <- read.csv("../data/games.csv", stringsAsFactors = FALSE)

# Filter data to 2019 season, the date is 2020
NBA_games_2020 <- games %>% filter(SEASON == "2019")


### Summary information: Does home or away, different environment and audiences, affect players' shooting percentage? 
### FG_PCT Home vs. Away
Ave_FG_PTC_home <- NBA_games_2020 %>%
  select(FG_PCT_home) %>%
  summarize(
    count = n(),
    FG_PCT_home = sum(FG_PCT_home),
    ave = FG_PCT_home / count)%>%
  pull(ave)
Ave_FG_PTC_home <- round(Ave_FG_PTC_home, digits = 3)

Ave_FG_PTC_away <- NBA_games_2020 %>%
  select(FG_PCT_away) %>%
  summarize(
    count = n(),
    FG_PCT_away = sum(FG_PCT_away),
    ave = FG_PCT_away / count) %>%
  pull(ave)
Ave_FG_PTC_away <- round(Ave_FG_PTC_away, digits = 3)


### FG3_PCT Home vs. Away
Ave_FG3_PCT_home <- NBA_games_2020 %>%
  select(FG3_PCT_home) %>%
  summarize(
    count = n(),
    FG3_PCT_home = sum(FG3_PCT_home),
    ave = FG3_PCT_home / count) %>%
  pull(ave)
Ave_FG3_PCT_home <- round(Ave_FG3_PCT_home, digits = 3)

Ave_FG3_PCT_away <- NBA_games_2020 %>%
  select(FG3_PCT_away) %>%
  summarize(
    count = n(),
    FG3_PCT_away = sum(FG3_PCT_away),
    ave = FG3_PCT_away / count) %>%
  pull(ave)
Ave_FG3_PCT_away <- round(Ave_FG3_PCT_away, digits = 3)

# total number of win and lose
num_home_win <- NBA_games_2020 %>%
  select(HOME_TEAM_WINS) %>%
  filter(HOME_TEAM_WINS == "1") %>%
  summarize(total_win = n()) %>%
  pull(total_win)

num_home_loose <- NBA_games_2020 %>%
  select(HOME_TEAM_WINS) %>%
  filter(HOME_TEAM_WINS == "0") %>%
  summarize(total_loose = n()) %>%
  pull(total_loose)

### Rebound Home vs. Away
Rebound_home <- NBA_games_2020 %>%
  select(REB_home) %>%
  summarize(
    count = n(),
    REB_home = sum(REB_home),
    ave = REB_home / count) %>%
  pull(ave)
Rebound_home <- round(Rebound_home, digits = 3)


Rebound_away <- NBA_games_2020 %>%
  select(REB_away) %>%
  summarize(
    count = n(),
    REB_away = sum(REB_away),
    ave = REB_away / count) %>%
  pull(ave)
Rebound_away <- round(Rebound_away, digits = 3)

