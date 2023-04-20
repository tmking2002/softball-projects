library(softballR)
library(tictoc)
library(tidyverse)

scoreboard <- load_ncaa_scoreboard(2023, division = "D1")

game_ids <- scoreboard %>% pull(game_id) %>% sort

i <- 0

get_ncaa_player_box <- function(game_id){
  
  i <<- i + 1
  
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
      dplyr::mutate(across(3:26, as.numeric)) %>%
      dplyr::mutate(game_id = game_id)
    
    return(upd)
    
  }
  
  hitting <- try(get_hitting_box(game_id))
  
  if(i %% 100 == 0) {toc(); tic(i+100)}
  
  return(hitting)
  
}

tic(100)
box <- do.call(rbind, lapply(X = game_ids[1:10], FUN = get_ncaa_player_box))
toc()

box <- box %>% 
  filter(!str_detect(player,"Error : Document is empty|subscript out of bounds")) %>% 
  distinct()

saveRDS(object = box, file = "d1_hitting_box_scores_2023.RDS")
