library(softballR)
library(tidyverse)
library(gt)
library(gtExtras)

source("~/Projects/softball-projects/get_power_ratings.R")

headshots <- read_csv("~/Projects/softball-projects/ACC Softball Analytics/acc_headshots.csv")

get_ncaa_player_box <- function(game_id){
  
  options(warn = -1)
  
  get_pitching_box <- function(id){
    
    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
      readLines()
    
    pitching_id <- raw[grep("\t   <a href=\"/game/box_score/", raw)] %>%
      stringr::str_remove_all("\t   <a href=\"/game/box_score/|\">Box Score </a>")
    
    raw <- glue::glue("https://stats.ncaa.org/game/box_score/{pitching_id}?year_stat_category_id=15021") %>%
      rvest::read_html() %>%
      rvest::html_table()
    
    first_team <- as.character(raw[[6]][1,1])
    second_team <- as.character(raw[[7]][1,1])
    
    upd <- rbind(raw[[6]],raw[[7]]) %>%
      `names<-`(raw[[6]][2,]) %>%
      janitor::clean_names() %>%
      dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))
    
    upd$team <- ifelse(upd$player %in% raw[[6]]$X1, first_team, second_team)
    upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
    upd[upd == ""] <- "0"
    upd[] <- lapply(upd, gsub, pattern="/", replacement="")
    
    upd <- upd %>%
      dplyr::mutate(across(3:26, as.numeric)) %>%
      dplyr::filter(ip > 0) %>% 
      dplyr::mutate(game_id = game_id)
    
    return(upd)
    
  }
  
  pitching <- try(get_pitching_box(game_id))
  
  return(pitching)
  
}

scoreboard <- load_ncaa_scoreboard(2023)

power_ratings <- get_power_ratings(scoreboard)

acc_teams <- c("Clemson", "Duke", "Louisville", "Florida St.", "Virginia Tech", "Georgia Tech",
               "North Carolina", "Notre Dame", "Virginia", "Pittsburgh", "Boston College", "NC State", "Syracuse")

acc_games <- scoreboard %>% 
  filter(home_team %in% acc_teams & away_team %in% acc_teams)

game_ids <- acc_games %>% 
  filter(game_date %in% c("04/14/2023", "04/15/2023", "04/16/2023")) %>% 
  pull(game_id)

tic()
box <- do.call(rbind, lapply(X = game_ids, FUN = get_ncaa_player_box))
toc()

stats <- box %>% 
  merge(power_ratings %>% select(team, offensive_rating), by.x = "opponent", by.y = "team") %>% 
  separate(player, c("last", "first"), sep = ", ") %>% 
  mutate(player = paste0(first, " ", last)) %>% 
  separate(ip, c("innings", "frac"), sep = "\\.") %>% 
  mutate(ip = ifelse(is.na(frac), innings, as.numeric(innings) + as.numeric(frac) * (1/3))) %>% 
  group_by(player) %>% 
  summarise(SOr = sum(so) / sum(bf),
            BBr = sum(bb) / sum(bf),
            ERA = sum(er) / sum(as.numeric(ip)) * 7,
            FIP = (13 * sum(hr_a) + 3 * (sum(bb) + sum(hb)) - 2 * sum(so)) / sum(as.numeric(ip)),
            wins = sum(as.numeric(w)),
            ip = sum(as.numeric(ip)),
            opponent_offensive_rating = mean(offensive_rating),
            bf = sum(bf)) %>% 
  filter(bf >= 25) %>% 
  merge(headshots, by.x = "player", by.y = "name")

sos_coef <- coef(lm(FIP ~ opponent_offensive_rating, stats))[2]

avg_era <- mean(stats$ERA)
avg_fip <- mean(stats$FIP)

stats$FIP <- stats$FIP + (avg_era - avg_fip)

stats$adjusted_fip <- stats$FIP - stats$opponent_offensive_rating * sos_coef

table <- stats %>%  
  arrange(adjusted_fip) %>% 
  head(n=6) %>% 
  select(url, player, team, ip, wins, ERA, adjusted_fip, SOr, BBr) %>% 
  gt() %>% 
  gt_img_circle(url, height = 60) %>% 
  fmt_number(ip, decimals = 0) %>% 
  fmt_number(6:9, decimals = 2) %>%
  cols_label(url = "--------",
             team = "Team",
             SOr = "K Rate",
             BBr = "BB Rate") %>% 
  data_color(columns = ERA,
             palette = c("#98FB98","#ff6961"),
             domain = c(max(stats$ERA), min(stats$ERA))) %>% 
  data_color(columns = adjusted_fip,
             palette = c("#98FB98","#ff6961"),
             domain = c(max(stats$adjusted_fip), min(stats$adjusted_fip))) %>% 
  data_color(columns = BBr,
             palette = c("#98FB98","#ff6961"),
             domain = c(max(stats$BBr), min(stats$BBr))) %>% 
  data_color(columns = SOr,
             palette = c("#ff6961","#98FB98"),
             domain = c(max(stats$SOr), min(stats$SOr))) %>% 
  tab_header(title = "ACC Top Pitchers",
             subtitle = "4/14 - 4/16 Weekend") %>%
  gt_theme_espn() %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px") %>% 
  tab_style(style = cell_borders(sides = c("left","right"),
                                 color = "grey"),
            locations = cells_body(columns = 6:9)) 

setwd("~/Projects/softball-projects/ACC Softball Analytics/")

gtsave(table, "Top Pitchers.png")  
