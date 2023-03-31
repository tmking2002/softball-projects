library(tidyverse)
library(softballR)
library(gt)
library(gtExtras)
scoreboard <- readRDS("~/Desktop/softballR-data/data/ncaa_scoreboard_2023.RDS")
source("~/Desktop/Projects/softball-projects/get_current_rpi.R")

rpi <- get_current_rpi(scoreboard) %>%
  select(team_name, rpi_rank)

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

remaining <- data.frame(team = c(rep("Clemson", 15), rep("Duke", 12), rep("Louisville",15), rep("Florida St.",18), rep("Virginia Tech",15), rep("Georgia Tech",15),
                                 rep("North Carolina",15), rep("Notre Dame", 15), rep("Virginia",15), rep("Pittsburgh",18), rep("Boston College",18), rep("NC State",12), rep("Syracuse",15)),
                        opponent = c(rep("Boston College",3), rep("Florida St.",3), rep("NC State", 3), rep("Pittsburgh",3), rep("Virginia Tech", 3),
                                     rep("Virginia Tech",3), rep("Boston College",3), rep("Georgia Tech", 3), rep("Pittsburgh", 3),
                                     rep("Syracuse",3), rep("Notre Dame",3), rep("Virginia",3), rep("Boston College",3), rep("Florida St.",3),
                                     rep("Georgia Tech",3), rep("Clemson",3), rep("Virginia",3), rep("Virginia Tech",3), rep("Notre Dame",3), rep("Louisville",3),
                                     rep("Duke",3), rep("Virginia",3), rep("Notre Dame",3), rep("Florida St.",3), rep("Clemson",3),
                                     rep("Florida St.",3), rep("Boston College",3), rep("Pittsburgh",3), rep("Duke",3), rep("North Carolina",3),
                                     rep("Virginia",3), rep("Pittsburgh",3), rep("Syracuse",3), rep("NC State",3), rep("Georgia Tech",3),
                                     rep("Pittsburgh",3), rep("Louisville",3), rep("Virginia Tech",3), rep("Boston College",3), rep("Florida St.",3),
                                     rep("North Carolina",3), rep("Virginia Tech",3), rep("Florida State",3), rep("Louisville",3), rep("Syracuse", 3),
                                     rep("Notre Dame",3), rep("North Carolina",3), rep("Georgia Tech",3), rep("Clemson",3), rep("Duke",3), rep("NC State",3),
                                     rep("Clemson",3), rep("Georgia Tech",3), rep("Duke",3), rep("Notre Dame",3), rep("Louisville",3), rep("Syracuse",3),
                                     rep("Syracuse",3), rep("Clemson",3), rep("North Carolina",3), rep("Pittsburgh",3),
                                     rep("Louisville",3), rep("NC State",3), rep("North Carolina",3), rep("Boston College",3), rep("Virginia",3)))

remaining_sos <- remaining %>% 
  merge(rpi, by.x = "opponent", by.y = "team_name") %>% 
  group_by(team) %>% 
  summarise(avg_opponent_rpi = mean(rpi_rank)) %>% 
  ungroup() %>% 
  mutate(rank = rank(avg_opponent_rpi)) %>% 
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
    mutate(rank = rank(-win_perc), ties.method = "random") %>%
    ungroup()

  records_upd <- records %>%
    group_by(team) %>%
    summarise(win = mean(rank == 1),
              top3 = mean(rank <= 3),
              top5 = mean(rank <= 5),
              make_tournament = mean(rank <= 10))

  #print(mean(records %>% filter(rank  == 10) %>% pull(wins)))

  return(records_upd)

}

probs <- remaining %>%
  select(team, opponent) %>%
  merge(rpi, by.x = "team", by.y = "team_name") %>%
  rename(team_rank = rpi_rank) %>%
  merge(rpi, by.x = "opponent", by.y = "team_name") %>%
  rename(opponent_rank = rpi_rank) %>%
  mutate(log_rank_diff = log(team_rank) - log(opponent_rank))

load(file = "rpi_model.RDA")
probs$prob = predict(model, probs, type = "response")

results <- simulate_seasons(probs,1000)

standings <- get_standings(results)

table <- standings %>%
  merge(logos, by.x = "team", by.y = "home_team_display_name") %>%
  merge(current_standings %>% select(team, record)) %>%
  merge(remaining_sos, by = "team") %>% 
  select(home_team_logo, team, record, rank, win, top3, top5, make_tournament) %>%
  arrange(desc(win), desc(top3), desc(top5), desc(make_tournament)) %>%
  gt() %>%
  gt_img_rows(home_team_logo) %>%
  cols_label(home_team_logo = "",
             team = "",
             rank = "Remaining SOS",
             win = "Win",
             top3 = "Top 3",
             top5 = "Top 5",
             make_tournament = "Make Tournament") %>%
  fmt_percent(5:8, decimals = 1) %>%
  data_color(columns = c(win, top3, top5, make_tournament),
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
             subtitle = "Based on 1000 Simulations of Remainder of Season") %>%
  tab_footnote(footnote = "Through 3/30/23",
               locations = cells_column_labels(columns = record)) %>% 
  opt_align_table_header(align = "center") %>%
  tab_style(style = cell_borders(sides = "right",
                                 weight = px(3)),
            locations = cells_body(columns = 4)) %>%
  tab_style(style = cell_borders(sides = "top",
                                 weight = px(3)),
            locations = cells_body(rows = 1))

gtsave(table, "~/Desktop/Projects/softball-projects/ACC Rankings Predictions.png")
