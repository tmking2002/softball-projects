devtools::install_github("tmking2002/softballR")
library(softballR)
library(magrittr)
library(gt)

team <- "Louisville"

teams <- get_ncaa_teams(2023)
pbp <- get_ncaa_pbp(teams %>% dplyr::filter(team_name == team) %>% dplyr::pull(team_id))

team_data <- pbp %>% 
  dplyr::filter(batting_team == team & stringr::str_detect(events,"\\(")) 

split <- team_data %>% 
  dplyr::pull(events) %>% 
  stringr::str_split("\\(|\\)")

sequences <- data.frame()

for(i in 1:length(split)){
  
  curr <- split[[i]]
  
  player <- stringr::word(curr[1], 1,1)
  sequence <- stringr::word(curr[2],2,2)
  
  sequences <- rbind(sequences, data.frame(player, sequence))
}

sequences <- sequences %>% 
  tidyr::drop_na(sequence)

team_sequences <- sequences %>% 
  tidyr::separate(sequence, c(paste0("pitch_",0:12)),sep = "")

longer <- team_sequences %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("pitch_")) %>% 
  dplyr::mutate(pitch_no = substr(name, 7,8)) %>% 
  dplyr::filter(!is.na(value) & value != "")
  

first_pitch <- longer %>% 
  dplyr::filter(pitch_no == 1) %>% 
  dplyr::group_by(player) %>% 
  dplyr::summarise(total = dplyr::n(),
                   first_pitch_swing = sum(ifelse(value %in% c("F","S"),T,F)) / total) %>% 
  dplyr::filter(total >= 5) %>% 
  dplyr::select(-total)

all_pitches <- longer %>% 
  dplyr::group_by(player) %>% 
  dplyr::summarise(total = dplyr::n(),
                   swing = sum(ifelse(value %in% c("F","S"),T,F)) / total) %>% 
  dplyr::filter(total >= 10)  %>% 
  dplyr::select(-total)

atbat_length <- longer %>%
  dplyr::mutate(new_atbat = is.na(dplyr::lag(pitch_no)) | dplyr::lag(pitch_no) != as.numeric(pitch_no) - 1,
                streak_id = cumsum(new_atbat)) %>% 
  dplyr::group_by(player,streak_id) %>% 
  dplyr::summarise(length = dplyr::last(pitch_no)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(player) %>% 
  dplyr::summarise(ab = dplyr::n(),
                   avg_length = mean(as.numeric(length))) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(ab >= 5)

stats <- merge(first_pitch, all_pitches, by = "player") %>% 
  merge(atbat_length, by = "player") %>% 
  dplyr::select(player, ab, first_pitch_swing, swing, avg_length)

table <- stats %>% 
  dplyr::mutate(player = stringr::str_remove(stringr::str_to_title(player),",")) %>% 
  dplyr::arrange(desc(first_pitch_swing)) %>% 
  gt() %>% 
  cols_label(player = "",
             first_pitch_swing = "First Pitch Swing %",
             swing = "Swing %",
             avg_length = "Avg. AB Length",
             ab = "AB") %>% 
  fmt_percent(3:4, decimals = 1) %>% 
  fmt_number(5, decimals = 2) %>% 
  data_color(columns = 3,
             colors = scales::col_numeric(
               palette = c("red", "green"),
               domain = c(0,.6)
             )) %>% 
  tab_header(title = paste0(team," Pitch Tendencies"))

setwd("~/Desktop/Projects/softball-projects")

gtsave(table, paste0(team, " Pitch Tendencies.png"))

       