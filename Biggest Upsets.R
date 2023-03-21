library(softballR)
library(tidyverse)
library(gt)
library(gtExtras)

setwd("~/Desktop/Softball")

source("Get Current RPI.R")

scoreboard_2022 <- load_espn_scoreboard(2022)
scoreboard_2023 <- readRDS("~/Desktop/softballR-data/data/ncaa_scoreboard_2023.RDS")
#scoreboard_2023 <- load_ncaa_scoreboard(2023)

logos <- scoreboard_2022 %>% 
  distinct(home_team_display_name, home_team_logo)

rpi_2022 <- read_csv("2022 RPI Rankings.csv")
rpi_2023 <- get_current_rpi(scoreboard_2023)

scoreboard_train <- scoreboard_2022 %>% 
  filter(description == "Final") %>% 
  mutate(home_win = as.numeric(home_team_runs) > as.numeric(away_team_runs)) %>% 
  left_join(rpi_2022 %>% rename("home_rank" = "Rank"), by = c("home_team_display_name" = "School")) %>% 
  left_join(rpi_2022 %>% rename("away_rank" = "Rank"), by = c("away_team_display_name" = "School")) %>% 
  mutate(log_home_rank = log(home_rank),
         log_away_rank = log(away_rank),
         rank_diff = home_rank - away_rank,
         log_rank_diff = log_home_rank - log_away_rank)

model <- glm(home_win ~ log_rank_diff, data = scoreboard_train, family = "binomial")

start_date <- as.Date("2023-03-14")
end_date <- as.Date("2023-03-19")

scoreboard_test <- scoreboard_2023 %>% 
  mutate(date = as.Date(paste0(word(date,3,sep="/"),"-",word(date,1,sep="/"),"-",word(date,2,sep="/")))) %>% 
  filter(date < end_date & date >= start_date) %>% 
  mutate(team1_win = as.numeric(team1_runs) > as.numeric(team2_runs)) %>% 
  left_join(rpi_2023 %>% rename("team1_rank" = "rpi_rank"), by = c("team1" = "team_name")) %>% 
  left_join(rpi_2023 %>% rename("team2_rank" = "rpi_rank"), by = c("team2" = "team_name")) %>% 
  mutate(log_team1_rank = log(team1_rank),
         log_team2_rank = log(team2_rank),
         rank_diff = team1_rank - team2_rank,
         log_rank_diff = log_team1_rank - log_team2_rank)

scoreboard_test$likelihood <- predict(model, scoreboard_test, type = "response")
scoreboard_test$resid <- scoreboard_test$team1_win - scoreboard_test$likelihood

scoreboard_test <- scoreboard_test %>% 
  mutate(team1_runs = as.numeric(team1_runs),
         team2_runs = as.numeric(team2_runs),
         score = paste(team1_runs, team2_runs, sep = "-"),
         rank_diff = abs(rank_diff),
         log_rank_diff = abs(log_rank_diff)) %>% 
  merge(logos, by.x = "team1", by.y = "home_team_display_name") %>% 
  rename(team1_logo = home_team_logo) %>% 
  merge(logos, by.x = "team2", by.y = "home_team_display_name") %>% 
  rename(team2_logo = home_team_logo)

table <- scoreboard_test %>% 
  arrange(desc(abs(resid))) %>% 
  head(n = 5) %>%
  select(date,team1_logo, team1, team2_logo, team2, team1_runs, team2_runs, score, rank_diff, log_rank_diff) %>% 
  gt() %>% 
  fmt_date(date, date_style = "Md") %>% 
  cols_hide(c(team1_runs,team2_runs)) %>% 
  fmt_number(log_rank_diff, decimals = 2) %>% 
  gt_img_rows(team1_logo) %>% 
  gt_img_rows(team2_logo) %>% 
  cols_label(date = "Date",
             team1_logo = "",
             team1 = "",
             team2_logo = "",
             team2 = "",
             score = "Score",
             rank_diff = "Raw RPI Diff.",
             log_rank_diff = "Log RPI Diff.") %>% 
  tab_style(cell_borders(sides = "right",
                         style = "dashed",
                         color = "grey"),
            cells_body(columns = c(date, team1, team2),
                       rows = everything())) %>% 
  tab_style(style = cell_fill(color = "#CDEBC5"),
            locations = cells_body(
              columns = c(team1, team1_logo),
              rows = team1_runs > team2_runs
            )) %>% 
  tab_style(style = cell_fill(color = "#CDEBC5"),
            locations = cells_body(
              columns = c(team2, team2_logo),
              rows = team1_runs < team2_runs
            )) %>% 
  gt_theme_espn() %>% 
  tab_header(title = "Biggest NCAA Softball Upsets",
             subtitle = paste0(format(start_date, "%m/%d")," - ",format(end_date, "%m/%d"))) %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px")
  
gtsave(table, "Biggest Upsets.png")

