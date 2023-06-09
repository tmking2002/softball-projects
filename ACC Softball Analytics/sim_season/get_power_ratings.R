library(tidyverse)
library(fmsb)

get_power_ratings <- function(scoreboard){

  scoreboard_longer <- rbind(scoreboard[c(9,1,4,5,8)] %>% `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")),
                             scoreboard[c(9,5,8,1,4)] %>% `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")))

  rpi <- get_current_rpi(scoreboard) %>%
    select(team_name, rpi_rank)

  sos <- scoreboard_longer %>%
    merge(rpi, by.x = "opponent", by.y = "team_name") %>%
    group_by(team) %>%
    summarise(avg_opponent_rpi = mean(rpi_rank)) %>%
    ungroup() %>%
    mutate(rank = rank(avg_opponent_rpi)) %>%
    select(team, rank)

  runs_scored <- scoreboard_longer %>%
    group_by(team) %>%
    summarise(avg_runs_scored = mean(runs),
              games = n()) %>%
    filter(games >= 10) %>%
    select(-games) %>%
    drop_na()

  runs_allowed <- scoreboard_longer %>%
    group_by(team) %>%
    summarise(avg_runs_allowed = mean(opponent_runs),
              games = n()) %>%
    filter(games >= 10) %>%
    select(-games) %>%
    drop_na()

  best_offenses <- scoreboard_longer %>%
    merge(runs_allowed, by.x = "opponent", by.y = "team") %>%
    mutate(diff = runs - avg_runs_allowed) %>%
    group_by(team) %>%
    summarise(offensive_rating = mean(diff),
              games = n()) %>%
    ungroup() %>%
    filter(games >= 10) %>%
    drop_na()

  best_defenses <- scoreboard_longer %>%
    merge(runs_scored, by.x = "opponent", by.y = "team") %>%
    mutate(diff = avg_runs_scored - opponent_runs) %>%
    group_by(team) %>%
    summarise(defensive_rating = mean(diff),
              games = n()) %>%
    ungroup() %>%
    filter(games >= 10) %>%
    drop_na()

  standings <- scoreboard_longer %>%
    group_by(team) %>%
    summarise(wins = sum(runs > opponent_runs),
              losses = sum(runs < opponent_runs),
              ties = sum(runs == opponent_runs),
              win_perc = wins / (wins + losses),
              games = sum(wins, losses, ties)) %>%
    filter(games >= 10) %>%
    drop_na() %>%
    merge(best_offenses, by = "team") %>%
    merge(best_defenses, by = "team") %>%
    merge(sos, by = "team") %>%
    select(team, wins, losses, ties, win_perc, offensive_rating, defensive_rating, rank)

  model <- lm(win_perc ~ offensive_rating + defensive_rating + rank, data = standings)

  standings$overall_rating <- predict(model, standings) - coef(model)["rank"] * standings$rank

  standings$power_rating <- (standings$overall_rating-min(standings$overall_rating)) /
    (max(standings$overall_rating - min(standings$overall_rating)))

  return(standings)

}

rm(list = setdiff(ls(), c("get_power_ratings", "get_current_rpi")))
