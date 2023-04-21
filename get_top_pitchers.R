library(softballR)
library(tidyverse)
library(gt)
library(gtExtras)

source("~/Projects/softball-projects/get_power_ratings.R")

box <- readRDS("~/Projects/softball-projects/d1_pitching_box_scores_2023.RDS") %>% 
  distinct()

scoreboard <- load_ncaa_scoreboard(2023)

logos <- scoreboard %>% 
  distinct(home_team, home_team_logo)

power_ratings <- get_power_ratings(scoreboard)

opponent_offensive_stats <- rbind(scoreboard[c(9,1,4,5,8)] %>% 
                                    `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")),
                                  scoreboard[c(9,5,8,1,4)] %>% 
                                    `names<-`(c("date", "team", "runs", "opponent", "opponent_runs"))) %>% 
  merge(power_ratings %>% select(team, offensive_rating), by.x = "opponent", by.y = "team") %>% 
  group_by(team) %>% 
  summarise(avg_opp_off_rating = mean(offensive_rating)) %>% 
  ungroup()

stats <- box %>% 
  separate(ip, c("innings", "frac"), sep = "\\.") %>% 
  mutate(ip = ifelse(is.na(frac), innings, as.numeric(innings) + as.numeric(frac) * 1/3)) %>% 
  select(-c(innings, frac)) %>% 
  mutate(across(c(3:34,38), as.numeric)) %>% 
  group_by(player, team) %>% 
  summarise(across(where(is.numeric), 
                   .fns = sum),
            SOr = so / bf,
            BBr = (bb + hb) / bf,
            ERA = er / ip * 7,
            FIP = ((13 * hr_a) + (3 * (bb + hb)) - 2 * so) / ip) %>% 
  merge(opponent_offensive_stats, by = "team") %>% 
  merge(logos, by.x = "team", by.y = "home_team")

avg_era = mean(stats$ERA * stats$ip) / mean(stats$ip)
avg_fip = mean(stats$FIP * stats$ip) / mean(stats$ip)

fip_constant <- avg_era - avg_fip

stats$FIP <- stats$FIP + fip_constant

model <- lm(FIP ~ avg_opp_off_rating, data = stats)

pitcher_stats_upd <- stats %>% 
  filter(ip >= 100) %>% 
  mutate(wFIP = FIP + coef(model)[2] * avg_opp_off_rating) %>% 
  separate(player, c("last", "first"), ", ") %>% 
  mutate(player = paste(first, last),
         rank = rank(wFIP)) %>% 
  arrange(wFIP) %>% 
  select(rank, home_team_logo, player, wFIP, SOr, BBr, ERA)


table <- pitcher_stats_upd %>%
  filter(rank > 10 & rank <= 20) %>% 
  gt() %>% 
  cols_label(home_team_logo = "",
             player = "",
             SOr = "K Rate",
             BBr = "BB Rate") %>% 
  gt_img_rows(home_team_logo) %>% 
  fmt_number(wFIP, decimals = 3) %>% 
  fmt_number(c(ERA, SOr, BBr), decimals = 2) %>% 
  data_color(columns = wFIP,
             colors = scales::col_numeric(
               palette = c("#77DE78", "#FF6962"),
               domain = c(min(pitcher_stats_upd$wFIP),max(pitcher_stats_upd$wFIP))
             )) %>% 
  tab_style(style = cell_borders(sides = "left", color = "grey"),
            locations = cells_body(columns = c(home_team_logo, wFIP, SOr))) %>% 
  tab_header(title = "Top Pitchers by wFIP",
             subtitle = "wFIP: Fielding Independent Pitching Weighted by Opponent Off. Rating") %>% 
  gt_theme_espn() %>% 
  tab_options(heading.align = 'center')

gtsave(table, "top_pitchers_2.png")

