# Double bar graph: FG Percentage for home vs. away.
# load needed packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)

# load and clean data sets
games_data <- read.csv("../data/games.csv", stringsAsFactors = FALSE)
games_data_2020 <- games_data %>% 
  filter(SEASON == "2019")
teams_data <- read.csv("../data/teams.csv", stringsAsFactors = FALSE)
teams_data <- teams_data %>% 
  select(TEAM_ID, NICKNAME)

# add team names for home teams
teams_data <- teams_data %>% 
  rename(HOME_TEAM_ID = TEAM_ID)
add_home_team_name <- 
  left_join(games_data_2020, teams_data, by = "HOME_TEAM_ID") %>% 
  rename(HOME_TEAM_NAME = NICKNAME)

# add team names for visitor teams
teams_data <- teams_data %>% 
  rename(VISITOR_TEAM_ID = HOME_TEAM_ID)
games_data <-
  left_join(add_home_team_name, teams_data, by = "VISITOR_TEAM_ID") %>% 
  rename(VISITOR_TEAM_NAME = NICKNAME)

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
  mutate(average = (home_avg_fg_perc+away_avg_fg_perc)/2) %>% 
  arrange(-average) %>% 
  top_n(8) %>% 
  select(team_name, home_avg_fg_perc, away_avg_fg_perc)

# reshape data set
top8_teams_avg_fg <- 
  gather(top8_teams_avg_fg, home_away, teams_avg_fg, -team_name)

# create a grouped barchart for teams' Top8 average field goal percentage
top8_teams_avg_fg_chart <-
  ggplot(top8_teams_avg_fg, 
         aes(fill = home_away, y = teams_avg_fg, x = team_name)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set3")

# make the map interactive
ggplotly(top8_teams_avg_fg_chart)

