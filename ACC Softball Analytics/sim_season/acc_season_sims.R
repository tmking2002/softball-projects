library(tidyverse)
library(softballR)
library(glue)
library(anytime)
library(gt)
library(gtExtras)

sim_season <- function(scoreboard, num_sims){
  
  ratings <- get_power_ratings(scoreboard) %>%
    select(team, power_rating)
  
  logos <- scoreboard %>% 
    distinct(home_team, home_team_logo)
  
  acc_teams <- c("Clemson", "Duke", "Louisville", "Florida St.", "Virginia Tech", "Georgia Tech",
                 "North Carolina", "Notre Dame", "Virginia", "Pittsburgh", "Boston College", "NC State", "Syracuse")
  
  team_ids <- get_ncaa_teams(2023) %>% 
    filter(team_name %in% acc_teams)
  
  completed <- scoreboard %>%
    filter(away_team %in% acc_teams & home_team %in% acc_teams)
  
  completed <- rbind(completed[c(9,1,4,5,8)] %>% `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")),
                     completed[c(9,5,8,1,4)] %>% `names<-`(c("date", "team", "runs", "opponent", "opponent_runs"))) %>%
    mutate(win = runs > opponent_runs,
           tie = runs == opponent_runs)
  
  current_standings <- completed %>%
    group_by(team) %>%
    summarise(games = n(),
              wins = sum(win),
              ties = sum(tie),
              losses = games - wins - ties,
              record = paste0(wins,"-",losses,"-",ties))
  
  remaining <- data.frame()
  
  for(i in 1:nrow(team_ids)){
    
    raw <- glue("https://stats.ncaa.org/teams/{team_ids$team_id[i]}") %>% 
      readLines()
    
    teams <- acc_teams[which(acc_teams != team_ids$team_name[i])]
    
    locs <- grep(paste(teams, collapse = "|"), raw)
    
    date <- gsub('<[^>]+>', '', raw[locs - 2]) %>% 
      trimws() %>% 
      str_remove_all("\\(1\\)|\\(2\\)")
      
    opponent <- gsub('.*alt="([^"]+)".*', '\\1', raw[locs])
    
    games <- data.frame(team = team_ids$team_name[i], opponent, date) %>% 
      filter(anydate(date) >= Sys.Date())
    
    remaining <- rbind(remaining, games)
  }
  
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
    
    return(list(records_upd, records))
    
  }
  
  probs <- remaining %>%
    select(team, opponent) %>%
    merge(ratings, by.x = "team", by.y = "team") %>%
    rename(team1_rating = power_rating) %>%
    merge(ratings, by.x = "opponent", by.y = "team") %>%
    rename(team2_rating = power_rating)
  
  load(file = "power_rating_model.RDA")
  probs$prob = predict(model, probs, type = "response")
  
  results <- simulate_seasons(probs,num_sims)
  
  standings <- get_standings(results)[[1]]
  records <- get_standings(results)[[2]]
  
  table <- standings %>%
    merge(current_standings %>% select(team, record)) %>%
    merge(remaining_sos, by = "team", all = TRUE) %>% 
    merge(logos, by.x = "team", by.y = "home_team") %>% 
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
               subtitle = paste0("Based on ",num_sims," Simulations of Remainder of Season")) %>%
    tab_footnote(footnote = paste0("Through ", max(scoreboard$game_date)),
                 locations = cells_column_labels(columns = record)) %>% 
    opt_align_table_header(align = "center") %>%
    tab_style(style = cell_borders(sides = "right",
                                   weight = px(3)),
              locations = cells_body(columns = 4)) %>%
    tab_style(style = cell_borders(sides = "top",
                                   weight = px(3)),
              locations = cells_body(rows = 1))
  
  return(list(table, records, results))
  
}
