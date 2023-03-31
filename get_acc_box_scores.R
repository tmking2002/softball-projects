library(tidyverse)
library(tictoc)
library(softballR)

team_ids <- get_ncaa_teams(2023) %>%
  filter(team_name %in% c("Clemson", "Duke", "Louisville", "Florida St.", "Virginia Tech", "Georgia Tech",
                          "North Carolina", "Notre Dame", "Virginia", "Pittsburgh", "Boston College", "NC State", "Syracuse")) %>%
  select(team_name,team_id)

box <- data.frame()

for(i in 10:nrow(team_ids)){
  tic(team_ids$team_name[i])
  box <- rbind(box, get_ncaa_player_box(team_ids$team_id[i]) %>%
                 extract2("Hitting") %>%
                 filter(team == team_ids$team_name[i]))
  toc()
}

write_csv(box,"~/Desktop/Projects/softball-projects/ACC Box Scores.csv")
