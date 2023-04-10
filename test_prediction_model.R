library(anytime)

source("~/Desktop/Projects/softball-projects/get_power_ratings.R")
source("~/Desktop/Projects/softball-projects/create_winperc_model.R")
load("~/Desktop/Projects/softball-projects/power_rating_model.RDA")

scoreboard_2022 <- readRDS("~/Desktop/softballR-data/data/ncaa_scoreboard_2022.RDS")
scoreboard_2023 <- readRDS("~/Desktop/softballR-data/data/ncaa_scoreboard_2023.RDS") %>% 
  mutate(game_date = anydate(game_date),
         home_win = home_team_runs > away_team_runs)

first_day <- min(scoreboard_2023$game_date)
three_weeks <- first_day + 21

scoreboard_2023$pred <- NA

for(i in 1:nrow(scoreboard_2023)){
  
  if(scoreboard_2023$game_date[i] <= three_weeks){
    
    power_ratings <- standings_2022
    
    if(nrow(power_ratings %>% filter(team == scoreboard_2023$home_team[i])) == 0){
      scoreboard_2023$team1_rating[i] <- 0
    } else{
      scoreboard_2023$team1_rating[i] <- 
        power_ratings %>% 
        filter(team == scoreboard_2023$home_team[i]) %>% 
        pull(power_rating)
    }
    
    if(nrow(power_ratings %>% filter(team == scoreboard_2023$away_team[i])) == 0){
      scoreboard_2023$team2_rating[i] <- 0
    } else{
      scoreboard_2023$team2_rating[i] <- 
        power_ratings %>% 
        filter(team == scoreboard_2023$away_team[i]) %>% 
        pull(power_rating)
    }
    
    scoreboard_2023$pred[i] <- predict(model, scoreboard_2023[i,], type = "response")
    
  } else{
    
    scoreboard_through_date <- scoreboard_2023 %>% 
      filter(game_date < scoreboard_2023$game_date[i])
    
    power_ratings <- get_power_ratings(scoreboard_through_date)
    
    if(nrow(power_ratings[which(power_ratings$team == scoreboard_2023$home_team[i]),]) == 0){
      scoreboard_2023$team1_rating[i] <- 0
    } else{
      scoreboard_2023$team1_rating[i] <- 
        power_ratings[which(power_ratings$team == scoreboard_2023$home_team[i]),]$power_rating
    }
    
    if(nrow(power_ratings[which(power_ratings$team == scoreboard_2023$away_team[i]),]) == 0){
      scoreboard_2023$team2_rating[i] <- 0
    } else{
      scoreboard_2023$team2_rating[i] <- 
        power_ratings[which(power_ratings$team == scoreboard_2023$away_team[i]),]$power_rating
    }
    
    scoreboard_2023$pred[i] <-  predict(model, scoreboard_2023[i,], type = "response")
    
  }
  
  if(i %% 100 == 0) print(i)
}

scoreboard_2023$pred_home_win <- scoreboard_2023$pred >= .5

mean(scoreboard_2023$home_win == scoreboard_2023$pred_home_win, na.rm=T)
