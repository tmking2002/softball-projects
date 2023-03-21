library(tidyverse)
library(rvest)

acc_stats <- "https://theacc.com/stats.aspx?path=softball&year=2023" %>% 
  read_html() %>% 
  html_table()

team_hitting <- acc_stats[[1]] %>% select(-c(1,22:25))
team_pitching <- acc_stats[[2]] %>% select(-Index)
team_fielding <- acc_stats[[3]] %>% select(-Index) 

individual_hitting <- acc_stats[[4]] %>% select(-Index)
individual_pitching <- acc_stats[[5]] %>% select(-Index) 
individual_fielding <- acc_stats[[6]] %>% select(-Index)

rm(list=setdiff(ls(), c("team_hitting","team_pitching","team_fielding","individual_hitting","individual_pitching","individual_fielding")))
