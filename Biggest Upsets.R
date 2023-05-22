library(softballR)
library(tidyverse)
library(gt)
library(gtExtras)

setwd("~/Projects/softball-projects")

source("~/Projects/softball-projects/get_power_ratings.R")

scoreboard_2023 <- load_ncaa_softball_scoreboard(2023) %>% 

standings_2023 <- get_power_ratings(scoreboard_2023) %>% 
  select(team, power_rating)

logos <- scoreboard_2023 %>% 
  distinct(home_team, home_team_logo)

load("~/Projects/softball-projects/power_rating_model.RDA")

start_date <- as.Date("2023-05-10")
end_date <- as.Date("2023-05-13")

scoreboard_test <- scoreboard_2023 %>% 
  mutate(date = as.Date(paste0(word(game_date,3,sep="/"),"-",word(game_date,1,sep="/"),"-",word(game_date,2,sep="/")))) %>%
  filter(date <= end_date & date >= start_date) %>%
  mutate(team1_win = home_team_runs > away_team_runs) %>%
  left_join(standings_2023 %>% rename("team1_rating" = "power_rating"), by = c("home_team" = "team")) %>%
  left_join(standings_2023 %>% rename("team2_rating" = "power_rating"), by = c("away_team" = "team")) %>%
  drop_na(team1_rating, team2_rating) %>% 
  filter(team1_rating > 0 & team2_rating > 0) %>% 
  mutate(log_team1_rating = log(team1_rating),
         log_team2_rating = log(team2_rating),
         rating_diff = team1_rating - team2_rating,
         log_rating_diff = log_team1_rating - log_team2_rating)


scoreboard_test$likelihood <- predict(model, scoreboard_test, type = "response")
scoreboard_test$resid <- scoreboard_test$team1_win - scoreboard_test$likelihood

scoreboard_test <- scoreboard_test %>%
  mutate(score = paste(home_team_runs, away_team_runs, sep = "-"),
         rating_diff = abs(rating_diff),
         home_team = str_replace(home_team, "&amp;", "&"))


table <- scoreboard_test %>%
  arrange(desc(abs(resid))) %>%
  head(n = 5) %>%
  select(date,home_team_logo, home_team, away_team_logo, away_team, home_team_runs, away_team_runs, score, rating_diff) %>%
  gt() %>%
  fmt_date(date, date_style = "Md") %>%
  cols_hide(c(home_team_runs,away_team_runs)) %>%
  fmt_number(rating_diff, decimals = 2) %>%
  gt_img_rows(home_team_logo) %>%
  gt_img_rows(away_team_logo) %>%
  cols_label(date = "Date",
             home_team_logo = "",
             home_team = "",
             away_team_logo = "",
             away_team = "",
             score = "Score",
             rating_diff = "Rating Diff.") %>%
  tab_style(cell_borders(sides = "right",
                         style = "dashed",
                         color = "grey"),
            cells_body(columns = c(date, home_team, away_team),
                       rows = everything())) %>%
  tab_style(style = cell_fill(color = "#CDEBC5"),
            locations = cells_body(
              columns = c(home_team, home_team_logo),
              rows = home_team_runs > away_team_runs
            )) %>%
  tab_style(style = cell_fill(color = "#CDEBC5"),
            locations = cells_body(
              columns = c(away_team, away_team_logo),
              rows = home_team_runs < away_team_runs
            )) %>%
  gt_theme_espn() %>%
  tab_header(title = "Biggest NCAA Softball Upsets",
             subtitle = paste0(format(start_date, "%m/%d")," - ",format(end_date, "%m/%d"))) %>%
  tab_footnote(footnote = "(Strength of Schedule Adjusted Power Rating)",
               locations = cells_column_labels(columns = rating_diff)) %>% 
  opt_align_table_header(align = "center") %>%
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px")

gtsave(table, "Biggest Upsets.png")

