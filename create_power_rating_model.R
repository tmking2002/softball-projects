source("~/Desktop/Projects/softball-projects/get_power_ratings.R")

scoreboard_2022 <- readRDS("~/Desktop/softballR-data/data/ncaa_scoreboard_2022.RDS")
scoreboard_2023 <- readRDS("~/Desktop/softballR-data/data/ncaa_scoreboard_2023.RDS")

standings_2022 <- get_power_ratings(scoreboard_2022)

scoreboard_train <- scoreboard_2022 %>%
  mutate(team1_win = team1_runs > team2_runs) %>%
  left_join(standings_2022 %>% rename("team1_rating" = "power_rating"), by = c("team1" = "team")) %>%
  left_join(standings_2022 %>% rename("team2_rating" = "power_rating"), by = c("team2" = "team")) %>%
  drop_na(team1_rating, team2_rating) %>% 
  filter(team1_rating > 0 & team2_rating > 0) %>% 
  mutate(log_team1_rating = log(team1_rating),
         log_team2_rating = log(team2_rating),
         rating_diff = team1_rating - team2_rating,
         log_rating_diff = log_team1_rating - log_team2_rating)

model <- glm(team1_win ~ exp(team1_rating) + exp(team2_rating), data = scoreboard_train, family = "binomial")
save(model, file = "~/Desktop/Projects/softball-projects/power_rating_model.RDA")

