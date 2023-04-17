library(softballR)
library(tictoc)
library(tidyverse)

scoreboard <- load_ncaa_scoreboard(2023, division = "D1")

game_ids <- scoreboard %>% pull(game_id) %>% sort

get_ncaa_player_box <- function(game_id){
  
  options(warn = -1)
  
  get_hitting_box <- function(id){
    
    raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
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
    
    upd <- upd %>%
      dplyr::mutate(across(3:26, as.numeric))
    
    return(upd)
    
  }
  
  hitting <- try(get_hitting_box(game_id))
  
  return(hitting)
  
}

tic()
box <- do.call(rbind, lapply(X = game_ids, FUN = get_ncaa_player_box))
toc()

saveRds(object = box, file = "d1_box_scores_2023.csv")