library(tidyverse)
library(softballR)
library(gt)
library(gtExtras)

setwd("~/Projects/softball-projects")
#setwd("~/Desktop/Projects/softball-projects/")

source("get_power_ratings.R")

scoreboard <- load_ncaa_scoreboard(2023)

ratings <- get_power_ratings(scoreboard) %>%
  select(team, power_rating)

logos <- load_espn_scoreboard(2023) %>%
  distinct(home_team_display_name, home_team_logo) %>%
  mutate(home_team_display_name = ifelse(home_team_display_name == "Florida State", "Florida St.", home_team_display_name))

acc_teams <- c("Clemson", "Duke", "Louisville", "Florida St.", "Virginia Tech", "Georgia Tech",
               "North Carolina", "Notre Dame", "Virginia", "Pittsburgh", "Boston College", "NC State", "Syracuse")

completed <- scoreboard %>%
  filter(team1 %in% acc_teams & team2 %in% acc_teams)

completed <- rbind(completed[c(1,2,3,4,5)] %>% `names<-`(c("date", "team", "opponent", "runs", "opponent_runs")),
                           completed[c(1,3,2,5,4)] %>% `names<-`(c("date", "team", "opponent", "runs", "opponent_runs"))) %>%
  mutate(win = runs > opponent_runs,
         tie = runs == opponent_runs)

current_standings <- completed %>%
  group_by(team) %>%
  summarise(games = n(),
            wins = sum(win),
            ties = sum(tie),
            losses = games - wins - ties,
            record = paste0(wins,"-",losses,"-",ties))

remaining <- data.frame(team = c(rep("Clemson", 12), rep("Duke", 9), rep("Louisville",12), rep("Florida St.",15), rep("Virginia Tech",12), rep("Georgia Tech",12),
                                 rep("North Carolina",12), rep("Notre Dame", 12), rep("Virginia",12), rep("Pittsburgh",15), rep("Boston College",15), rep("NC State",12), rep("Syracuse",12)),
                        opponent = c(rep("Florida St.",3), rep("NC State", 3), rep("Pittsburgh",3), rep("Virginia Tech", 3),
                                     rep("Boston College",3), rep("Georgia Tech", 3), rep("Pittsburgh", 3),
                                     rep("Notre Dame",3), rep("Virginia",3), rep("Boston College",3), rep("Florida St.",3),
                                     rep("Clemson",3), rep("Virginia",3), rep("Virginia Tech",3), rep("Notre Dame",3), rep("Louisville",3),
                                     rep("Virginia",3), rep("Notre Dame",3), rep("Florida St.",3), rep("Clemson",3),
                                     rep("Boston College",3), rep("Pittsburgh",3), rep("Duke",3), rep("North Carolina",3),
                                     rep("Pittsburgh",3), rep("Syracuse",3), rep("NC State",3), rep("Georgia Tech",3),
                                     rep("Louisville",3), rep("Virginia Tech",3), rep("Boston College",3), rep("Florida St.",3),
                                     rep("Virginia Tech",3), rep("Florida State",3), rep("Louisville",3), rep("Syracuse", 3),
                                     rep("North Carolina",3), rep("Georgia Tech",3), rep("Clemson",3), rep("Duke",3), rep("NC State",3),
                                     rep("Georgia Tech",3), rep("Duke",3), rep("Notre Dame",3), rep("Louisville",3), rep("Syracuse",3),
                                     rep("Syracuse",3), rep("Clemson",3), rep("North Carolina",3), rep("Pittsburgh",3),
                                     rep("NC State",3), rep("North Carolina",3), rep("Boston College",3), rep("Virginia",3)))

remaining_sos <- remaining %>% 
  merge(ratings, by.x = "opponent", by.y = "team") %>% 
  group_by(team) %>% 
  summarise(avg_rating = mean(power_rating)) %>% 
  ungroup() %>% 
  mutate(rank = rank(-avg_rating)) %>% 
  select(team, rank)


simulate_seasons <- function(probs, n_sims){

  results <- data.frame()

  for(sim in 1:n_sims){

    for(i in 1:nrow(probs)){

      probs$win[i] <- sample(c(T,F), prob = c(probs$prob[i], 1 - probs$prob[i]))

    }

    probs_upd <- rbind(probs %>% select(team, opponent, win),
                       completed %>% select(team, opponent, win)) %>%
      mutate(sim_id = sim)

    results <- rbind(results, probs_upd)
    
    if(sim %% 100 == 0) print(sim) 
  }
  
  
  return(results)

}

get_standings <- function(results){

  records <- results %>%
    group_by(team, sim_id) %>%
    summarise(games = n(),
              wins = sum(win),
              losses = games - wins,
              win_perc = wins / games) %>%
    ungroup() %>%
    group_by(sim_id) %>%
    mutate(rank = rank(-win_perc, ties.method = "random")) %>%
    ungroup()

  records_upd <- records %>%
    group_by(team) %>%
    summarise(win = mean(rank == 1),
              top3 = mean(rank <= 3),
              top6 = mean(rank <= 6),
              make_tournament = mean(rank <= 10))

  #print(mean(records %>% filter(rank  == 10) %>% pull(wins)))

  return(records_upd)

}

probs <- remaining %>%
  select(team, opponent) %>%
  merge(ratings, by.x = "team", by.y = "team") %>%
  rename(team1_rating = power_rating) %>%
  merge(ratings, by.x = "opponent", by.y = "team") %>%
  rename(team2_rating = power_rating)

load(file = "power_rating_model.RDA")
probs$prob = predict(model, probs, type = "response")

sims <- 5000

results <- simulate_seasons(probs,sims)

standings <- get_standings(results)

table <- standings %>%
  merge(logos, by.x = "team", by.y = "home_team_display_name") %>%
  merge(current_standings %>% select(team, record)) %>%
  merge(remaining_sos, by = "team") %>% 
  mutate(total = win + top3 + top6 + make_tournament) %>% 
  arrange(desc(total)) %>%
  select(home_team_logo, team, record, rank, win, top3, top6, make_tournament) %>%
  gt() %>%
  gt_img_rows(home_team_logo) %>%
  cols_label(home_team_logo = "",
             team = "",
             rank = "Remaining SOS",
             win = "Win",
             top3 = "Top 3",
             top6 = "R1 Bye",
             make_tournament = "Make Tournament") %>%
  fmt_percent(5:8, decimals = 1) %>%
  data_color(columns = c(win, top3, top6, make_tournament),
             colors = scales::col_numeric(
               palette = c("#FF6962", "#77DE78"),
               domain = c(0,1)
             )) %>%
  data_color(columns = rank,
             colors = scales::col_numeric(
               palette = c("#FF6962", "#77DE78"),
               domain = c(1,13)
             )) %>% 
  gt_theme_espn() %>%
  cols_align(align = "center",
             columns = 3:6) %>%
  tab_header(title = "2023 ACC Softball Tournament Odds",
             subtitle = paste0("Based on ",sims," Simulations of Remainder of Season")) %>%
  tab_footnote(footnote = paste0("Through ", max(scoreboard$date)),
               locations = cells_column_labels(columns = record)) %>% 
  opt_align_table_header(align = "center") %>%
  tab_style(style = cell_borders(sides = "right",
                                 weight = px(3)),
            locations = cells_body(columns = 4)) %>%
  tab_style(style = cell_borders(sides = "top",
                                 weight = px(3)),
            locations = cells_body(rows = 1))

gtsave(table, "ACC Rankings Predictions 4_2.png")
