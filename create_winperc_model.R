source("~/Desktop/Projects/softball-projects/get_power_ratings.R")

scoreboard <- readRDS("~/Desktop/softballR-data/data/ncaa_scoreboard_2022.RDS")

scoreboard_longer <- rbind(scoreboard[c(1,2,4,3,5)] %>% `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")),
                           scoreboard[c(1,3,5,2,4)] %>% `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")))

team1_scoreboard <- scoreboard[c(1,2,4,3,5)] %>% `names<-`(c("date","team_name","runs","opponent_name","opponent_runs"))
team2_scoreboard <- scoreboard[c(1,3,5,2,4)] %>% `names<-`(c("date","team_name","runs","opponent_name","opponent_runs"))

scoreboard_upd <- rbind(team1_scoreboard, team2_scoreboard) %>%
  mutate(win = case_when(runs > opponent_runs ~ 1,
                         runs < opponent_runs ~ 0,
                         runs == opponent_runs ~ 0.5))


win_perc <- scoreboard_upd %>%
  group_by(team_name) %>%
  summarise(games = n(),
            win_perc = mean(win)) %>%
  filter(games > 5) %>%
  select(-games) %>% 
  drop_na()

scoreboard_upd_2 <- scoreboard_upd %>%
  merge(win_perc, by.x = "opponent_name", by.y = "team_name") %>%
  rename(opponent_win_perc = win_perc) %>%
  merge(win_perc, by = "team_name")


opponent_win_perc <- scoreboard_upd_2 %>%
  group_by(team_name) %>%
  summarise(opponent_opponent_win_perc = mean(opponent_win_perc))

scoreboard_upd_3 <- scoreboard_upd_2 %>%
  merge(opponent_win_perc, by.x = "opponent_name", by.y = "team_name")


rpi <- scoreboard_upd_3 %>%
  group_by(team_name) %>%
  summarise(rpi_coef = (.5 * mean(win_perc) + .25 * mean(opponent_win_perc) + .25 * mean(opponent_opponent_win_perc)),
            record = paste(floor(sum(win)),floor(n() - sum(win)),ceiling(sum(win) %% 1), sep = "-")) %>%
  ungroup() %>%
  mutate(rpi_rank = rank(-rpi_coef))

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

standings_2022 <- scoreboard_longer %>% 
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


model <- lm(win_perc ~ offensive_rating + defensive_rating + rank, data = standings_2022)
save(model, file = "~/Desktop/Projects/softball-projects/power_rating_winperc_model.RDA")

standings_2022$overall_rating <- predict(model, standings_2022) - coef(model)["rank"] * standings_2022$rank

standings_2022$power_rating <- (standings_2022$overall_rating-min(standings_2022$overall_rating)) / 
  (max(standings_2022$overall_rating - min(standings_2022$overall_rating)))

rm(list = setdiff(ls(), c("standings_2022","get_power_ratings", "get_current_rpi")))
