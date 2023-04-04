library(softballR)
library(tidyverse)
library(gt)
library(gtExtras)

setwd("~/Desktop/Projects/softball-projects")

source("~/Desktop/Projects/softball-projects/get_power_ratings.R")

scoreboard_2022 <- load_espn_scoreboard(2022)
scoreboard_2023 <- readRDS("~/Desktop/softballR-data/data/ncaa_scoreboard_2023.RDS")

standings_2023 <- get_power_ratings(scoreboard_2023) %>% 
  select(team, power_rating)

logos <- scoreboard_2022 %>%
  distinct(home_team_display_name, home_team_logo)

load("~/Desktop/Projects/softball-projects/power_rating_model.RDA")

start_date <- as.Date("2023-03-27")
end_date <- as.Date("2023-04-02")

scoreboard_test <- scoreboard_2023 %>% 
  mutate(date = as.Date(paste0(word(date,3,sep="/"),"-",word(date,1,sep="/"),"-",word(date,2,sep="/")))) %>%
  filter(date <= end_date & date >= start_date) %>%
  mutate(team1_win = team1_runs > team2_runs) %>%
  left_join(standings_2023 %>% rename("team1_rating" = "power_rating"), by = c("team1" = "team")) %>%
  left_join(standings_2023 %>% rename("team2_rating" = "power_rating"), by = c("team2" = "team")) %>%
  drop_na(team1_rating, team2_rating) %>% 
  filter(team1_rating > 0 & team2_rating > 0) %>% 
  mutate(log_team1_rating = log(team1_rating),
         log_team2_rating = log(team2_rating),
         rating_diff = team1_rating - team2_rating,
         log_rating_diff = log_team1_rating - log_team2_rating)


scoreboard_test$likelihood <- predict(model, scoreboard_test, type = "response")
scoreboard_test$resid <- scoreboard_test$team1_win - scoreboard_test$likelihood

scoreboard_test <- scoreboard_test %>%
  mutate(team1_runs = as.numeric(team1_runs),
         team2_runs = as.numeric(team2_runs),
         score = paste(team1_runs, team2_runs, sep = "-"),
         rating_diff = abs(rating_diff),
         log_rating_diff = abs(log_rating_diff)) %>%
  merge(logos, by.x = "team1", by.y = "home_team_display_name") %>%
  rename(team1_logo = home_team_logo) %>%
  merge(logos, by.x = "team2", by.y = "home_team_display_name") %>%
  rename(team2_logo = home_team_logo)

table <- scoreboard_test %>%
  arrange(desc(abs(resid))) %>%
  head(n = 5) %>%
  select(date,team1_logo, team1, team2_logo, team2, team1_runs, team2_runs, score, rating_diff, log_rating_diff) %>%
  gt() %>%
  fmt_date(date, date_style = "Md") %>%
  cols_hide(c(team1_runs,team2_runs)) %>%
  fmt_number(c(rating_diff, log_rating_diff), decimals = 2) %>%
  gt_img_rows(team1_logo) %>%
  gt_img_rows(team2_logo) %>%
  cols_label(date = "Date",
             team1_logo = "",
             team1 = "",
             team2_logo = "",
             team2 = "",
             score = "Score",
             rating_diff = "Rating Diff.",
             log_rating_diff = "Log. Rating Diff.") %>%
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
  tab_footnote(footnote = "(Strength of Schedule Adjusted Power Rating)",
               locations = cells_column_labels(columns = rating_diff)) %>% 
  opt_align_table_header(align = "center") %>%
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px")

#gtsave(table, "Biggest Upsets.png")

