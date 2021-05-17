# Domain of Interest

### Domain: NBA
### Why are you interested in this field/domain?
  We all like and play sports. We are passionate about basketball. It’s popular among American culture.

### Examples of other data driven projects related to this domain
  1. https://www.theatlantic.com/entertainment/archive/2015/06/nba-data-analytics/396776/
  * Every micro-movement on the court would be tracked, quantified, and archived. The Rockets incorporated advanced analysis into how they play the game and thus set the all-time record for three-pointers in the season.
  2. https://sportsanalytics.berkeley.edu/projects/nba-shot-eval.pdf
  * They first track and analyze data with some utility functions. Then they find out the distance between the shooter and closest teammate has an important role. Therefore, they underscore the spacing in offense while practicing and during the games.
  3. https://theconversation.com/for-these-students-using-data-in-sports-is-about-more-than-winning-games-148898
  * Golden State Warriors is one of the first teams use data and analytics to improve their performances. As a result, the team subsequently won league championship in 2015, 2017 and 2018. Analysis help them make more informed decisions.


### What data-driven questions do you hope to answer about this domain (share at least 3)?
  1. Will the home game and away game affect the teams' performances?
      * comparing the winning rate for each team during home game and away game.
  2. How influential is the three point field goal for the offense side?
      * comparing the average three point field goal during winning and losing game with the average field goal during winning and losing game in each year.
  3. Is the team with the most regular season wins likely to win the final championship?
      * comparing whether the team with the most wins in regular season is the final champion in each year.

# Finding Data
### Where did you download the data (e.g., a web URL)?
  1. https://www.kaggle.com/nathanlauga/nba-games
  2. https://data.world/datatouille/stephen-curry-stats
  3. https://www.kaggle.com/ehallmar/nba-historical-stats-and-betting-data

### How was the data collected or generated? Make sure to explain who collected the data (not necessarily the same people that host the data), and who or what the data is about?
  1. NBA stats website, collected by Nathan Lauga. The data collected from every game records. This data set was created to try and predict the winners of NBA games.
  2. Tristan Malherbe created the data. The data collected from every game records. This set of data dives into the player Stephen Curry statistics to explore his ability.
  3. Evan Hallmark owned the data. This file includes betting information on the money lines for a large selection of nba games.

### How many observations (rows) are in your data?
  1. 24,196 rows
  2. 878 observations
  3. 125,287 observations

### How many features (columns) are in the data?
  1. 21 columns
  2. 21 columns
  3. 7 columns

### What questions (from above) can be answered using the data in this dataset?
  1. The first dataset contains home ID , away ID and number of scores. Thus could compare the winning rate for both home and away games which answers the first question.
  2. The second dataset contains 3 points shots and scores of lose or win for a team. This could answer the second question.
  3. The third dataset contains season id, game date and win or loose. By sum all the wins in regular season and select the top three teams, then compare to specific year of the championship team could answer the third question.
