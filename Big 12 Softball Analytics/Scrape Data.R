library(tidyverse)
library(rvest)

# 2023

#### Big 12 ####

big12_stats <- "https://big12sports.com/stats.aspx?path=softball&year=2023" %>% 
  read_html() %>% 
  html_table()

#### American ####

american_stats <- "https://theamerican.org/stats.aspx?path=softball&year=2023" %>% 
  read_html() %>% 
  html_table()

american_hitting <- american_stats[[1]] %>% select(-c(1,22:25)) %>% filter(Team %in%  c("UCF","Houston"))
american_pitching <- american_stats[[2]] %>% select(-Index) %>% filter(Team %in%  c("UCF","Houston"))
american_fielding <- american_stats[[3]] %>% select(-Index) %>% filter(Team %in%  c("UCF","Houston"))

american_individual_hitting <- american_stats[[4]] %>% select(-Index) %>% filter(str_detect(Player,"(UCF)") | str_detect(Player,"(Houston)"))
american_individual_pitching <- american_stats[[5]] %>% select(-Index) %>% filter(str_detect(Player,"(UCF)") | str_detect(Player,"(Houston)"))
american_individual_fielding <- american_stats[[6]] %>% select(-Index) %>% filter(str_detect(Player,"(UCF)") | str_detect(Player,"(Houston)"))

#### BYU ####

wcc_stats <- "https://wccsports.com/stats.aspx?path=softball&year=2023" %>% 
  read_html() %>% 
  html_table()

byu_hitting <- wcc_stats[[1]] %>% select(-c(1,22:25)) %>% filter(Team == "Brigham Young")
byu_pitching <- wcc_stats[[2]] %>% select(-Index) %>% filter(Team == "Brigham Young")
byu_fielding <- wcc_stats[[3]] %>% select(-Index) %>% filter(Team == "Brigham Young")

byu_individual_hitting <- wcc_stats[[4]] %>% select(-Index) %>% filter(str_detect(Player,"(Brigham Young)"))
byu_individual_pitching <- wcc_stats[[5]] %>% select(-Index) %>% filter(str_detect(Player,"(Brigham Young)"))
byu_individual_fielding <- wcc_stats[[6]] %>% select(-Index) %>% filter(str_detect(Player,"(Brigham Young)"))

#### Combine Data ####

team_hitting <- big12_stats[[1]] %>% select(-c(1,22:25)) %>% rbind(american_hitting) %>% rbind(byu_hitting)
team_pitching <- big12_stats[[2]] %>% select(-Index) %>% rbind(american_pitching) %>% rbind(byu_pitching)
team_fielding <- big12_stats[[3]] %>% select(-Index) %>% rbind(american_fielding) %>% rbind(byu_fielding)



individual_hitting <- big12_stats[[4]] %>% select(-Index) %>% rbind(american_individual_hitting) %>% rbind(byu_individual_hitting)
individual_pitching <- big12_stats[[5]] %>% select(-Index) %>% rbind(american_individual_pitching) %>% rbind(byu_individual_pitching)
individual_fielding <- big12_stats[[6]] %>% select(-Index) %>% rbind(american_individual_fielding) %>% rbind(byu_individual_fielding)

rm(list=setdiff(ls(), c("team_hitting","team_pitching","team_fielding","individual_hitting","individual_pitching","individual_fielding")))

# 2022

#### Big 12 ####

big12_stats_2022 <- "https://big12sports.com/stats.aspx?path=softball&year=2022" %>% 
  read_html() %>% 
  html_table()

#### American ####

american_stats_2022 <- "https://theamerican.org/stats.aspx?path=softball&year=2022" %>% 
  read_html() %>% 
  html_table()

american_hitting_2022 <- american_stats_2022[[1]] %>% select(-c(1,22:25)) %>% filter(Team %in%  c("UCF","Houston"))
american_pitching_2022 <- american_stats_2022[[2]] %>% select(-Index) %>% filter(Team %in%  c("UCF","Houston"))
american_fielding_2022 <- american_stats_2022[[3]] %>% select(-Index) %>% filter(Team %in%  c("UCF","Houston"))

american_individual_hitting_2022 <- american_stats_2022[[4]] %>% select(-Index) %>% filter(str_detect(Player,"(UCF)") | str_detect(Player,"(Houston)"))
american_individual_pitching_2022 <- american_stats_2022[[5]] %>% select(-Index) %>% filter(str_detect(Player,"(UCF)") | str_detect(Player,"(Houston)"))
american_individual_fielding_2022 <- american_stats_2022[[6]] %>% select(-Index) %>% filter(str_detect(Player,"(UCF)") | str_detect(Player,"(Houston)"))

#### BYU ####

wcc_stats_2022 <- "https://wccsports.com/stats.aspx?path=softball&year=2022" %>% 
  read_html() %>% 
  html_table()

byu_hitting_2022 <- wcc_stats_2022[[1]] %>% select(-c(1,22:25)) %>% filter(Team == "Brigham Young")
byu_pitching_2022 <- wcc_stats_2022[[2]] %>% select(-Index) %>% filter(Team == "Brigham Young")
byu_fielding_2022 <- wcc_stats_2022[[3]] %>% select(-Index) %>% filter(Team == "Brigham Young")

byu_individual_hitting_2022 <- wcc_stats_2022[[4]] %>% select(-Index) %>% filter(str_detect(Player,"(Brigham Young)"))
byu_individual_pitching_2022 <- wcc_stats_2022[[5]] %>% select(-Index) %>% filter(str_detect(Player,"(Brigham Young)"))
byu_individual_fielding_2022 <- wcc_stats_2022[[6]] %>% select(-Index) %>% filter(str_detect(Player,"(Brigham Young)"))

#### Combine Data ####

team_hitting_2022 <- big12_stats_2022[[1]] %>% select(-c(1,22:25)) %>% rbind(american_hitting_2022) %>% rbind(byu_hitting_2022)
team_pitching_2022 <- big12_stats_2022[[2]] %>% select(-Index) %>% rbind(american_pitching_2022) %>% rbind(byu_pitching_2022)
team_fielding_2022 <- big12_stats_2022[[3]] %>% select(-Index) %>% rbind(american_fielding_2022) %>% rbind(byu_fielding_2022)



individual_hitting_2022 <- big12_stats_2022[[4]] %>% select(-Index) %>% rbind(american_individual_hitting_2022) %>% rbind(byu_individual_hitting_2022)
individual_pitching_2022 <- big12_stats_2022[[5]] %>% select(-Index) %>% rbind(american_individual_pitching_2022) %>% rbind(byu_individual_pitching_2022)
individual_fielding_2022 <- big12_stats_2022[[6]] %>% select(-Index) %>% rbind(american_individual_fielding_2022) %>% rbind(byu_individual_fielding_2022)

rm(list=setdiff(ls(), c("team_hitting","team_pitching","team_fielding","individual_hitting","individual_pitching","individual_fielding",
                        "team_hitting_2022","team_pitching_2022","team_fielding_2022","individual_hitting_2022","individual_pitching_2022","individual_fielding_2022")))




