### Summary table

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



# Table
all_teams_data <- games_data %>%
  select(HOME_TEAM_NAME, REB_home, FT_PCT_home, FG_PCT_home) %>%
  group_by(HOME_TEAM_NAME) %>%
  summarize(REB_home = sum(REB_home),
            FT_PCT_home = sum(FT_PCT_home),
            FG_PCT_home = sum(FG_PCT_home)) %>%
  arrange(FG_PCT_home) 

  
# Table with 4 teams
offseason_teams_data <- games_data %>%
  select(HOME_TEAM_NAME, REB_home, FT_PCT_home, FG_PCT_home) %>%
  group_by(HOME_TEAM_NAME) %>%
  filter(HOME_TEAM_NAME %in% c("Warriors", "Raptors", "Trail Blazers", "Bucks")) %>%
  summarize(REB_home = sum(REB_home),
            FT_PCT_home = sum(FT_PCT_home),
            FG_PCT_home = sum(FG_PCT_home))

  

  
  
  
  
  
  
  
