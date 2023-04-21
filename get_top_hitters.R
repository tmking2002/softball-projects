library(softballR)
library(tidyverse)
library(gt)
library(gtExtras)

source("~/Projects/softball-projects/get_power_ratings.R")

box <- readRDS("~/Projects/softball-projects/d1_hitting_box_scores_2023.RDS") %>% 
  distinct() %>% 
  filter(!(str_detect(player, "Timeout was reached")))

scoreboard <- load_ncaa_scoreboard(2023)

logos <- scoreboard %>% 
  distinct(home_team, home_team_logo)

power_ratings <- get_power_ratings(scoreboard)

opponent_defensive_stats <- rbind(scoreboard[c(9,1,4,5,8)] %>% 
                                    `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")),
                                  scoreboard[c(9,5,8,1,4)] %>% 
                                    `names<-`(c("date", "team", "runs", "opponent", "opponent_runs"))) %>% 
  merge(power_ratings %>% select(team, defensive_rating), by.x = "opponent", by.y = "team") %>% 
  group_by(team) %>% 
  summarise(avg_opp_def_rating = mean(defensive_rating)) %>% 
  ungroup()

team_games <- box %>% 
  group_by(team, game_id) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(team) %>% 
  summarise(games = n()) %>% 
  filter(games >= 20)

team_stats <- box %>% 
  mutate(across(c(3:74), as.numeric)) %>% 
  mutate(across(c(3:74), ~replace_na(.,0))) %>% 
  merge(team_games, by = "team") %>% 
  group_by(team, games) %>% 
  summarise(across(4:74, 
                   .fns = sum)) %>% 
  filter(games >= 20) %>%
  mutate(across(3:72,
                .fns = \(col) col / games)) %>% 
  mutate(x1b = h - x2b - x3b - hr) %>% 
  ungroup() %>% 
  select(r, x1b, x2b, x3b, hr, bb_2, hbp)

team_model <- lm(r ~ 0 + ., data = team_stats)

stats <- box %>% 
  mutate(across(c(3:74), as.numeric)) %>% 
  mutate(across(c(3:74), ~replace_na(.,0))) %>% 
  mutate(x1b = h - x2b - x3b - hr) %>% 
  group_by(player, team) %>% 
  summarise(across(where(is.numeric), 
                   .fns = sum),
            OBP = (h + bb_2 + hbp) / (ab + bb_2 + hbp + sf + sh),
            SLG = tb / ab,
            OPS = OBP + SLG,
            SOr = k / (ab + bb + hbp + sf + sh),
            wOBA = (coef(team_model)["bb_2"] * bb_2 + coef(team_model)["hbp"] * hbp + 
                      coef(team_model)["x1b"] * x1b + coef(team_model)["x2b"] * x2b +
                      coef(team_model)["x3b"] * x3b + coef(team_model)["hr"] * hr) /
                   (ab + bb - ibb + sf + sh)) %>% 
  filter(!is.infinite(wOBA) & !is.na(wOBA)) %>% 
  filter(ab >= 50) %>% 
  merge(logos, by.x = "team", by.y = "home_team") %>% 
  merge(opponent_defensive_stats, by = "team")

avg_obp = mean(stats$h + stats$bb_2 + stats$hbp) / mean(stats$ab + stats$bb + stats$hbp + stats$sf + stats$sh)
avg_woba = mean(stats$wOBA * stats$ab) / mean(stats$ab)

woba_constant <- avg_woba - avg_obp

stats$wOBA <- stats$wOBA - woba_constant

model <- lm(wOBA ~ avg_opp_def_rating, data = stats)

hitter_stats_upd <- stats %>% 
  filter(ab >= 100) %>% 
  mutate(wOBA = wOBA + coef(model)[2] * avg_opp_def_rating) %>% 
  separate(player, c("last", "first"), ", ") %>% 
  mutate(player = paste(first, last),
         rank = rank(-wOBA)) %>% 
  arrange(rank) %>% 
  select(rank, home_team_logo, player, wOBA, OBP, OPS, SOr)


table <- hitter_stats_upd %>%
  filter(rank > 0 & rank <= 10) %>% 
  gt() %>% 
  cols_label(home_team_logo = "",
             player = "",
             SOr = "K Rate") %>% 
  gt_img_rows(home_team_logo) %>% 
  fmt_number(c(wOBA, OBP, OPS, SOr), decimals = 3) %>% 
  data_color(columns = wOBA,
             colors = scales::col_numeric(
               palette = c("#FF6962", "#77DE78"),
               domain = c(min(hitter_stats_upd$wOBA),max(hitter_stats_upd$wOBA))
             )) %>% 
  tab_style(style = cell_borders(sides = "left", color = "grey"),
            locations = cells_body(columns = c(home_team_logo, wOBA, OBP))) %>% 
  tab_header(title = "Top Hitters by wOBA",
             subtitle = "wOBA: On Base Average Weighted by Opponent Def. Rating") %>% 
  gt_theme_espn() %>% 
  tab_options(heading.align = 'center')

gtsave(table, "top_hitters_1.png")

