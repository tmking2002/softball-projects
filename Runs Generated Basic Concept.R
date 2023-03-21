library(rvest)
library(tidyverse)
library(glue)

get_runs_generated <- function(team_id){

  team_site <- glue::glue("https://stats.ncaa.org/teams/{team_id}") %>%
    readLines()

  ids <- rbind(get_ncaa_teams(2021),
               get_ncaa_teams(2022),
               get_ncaa_teams(2023))

  team_id_curr <- team_id

  team_name <- ids %>%
    dplyr::filter(team_id == team_id_curr) %>%
    dplyr::pull(team_name)

  games <- grep("http://web2.ncaa.org/ncaa_style/img/All_Logos", team_site)[2:56] + 3

  games <- games[which(str_detect(team_site[games],"BOX_SCORE_WINDOW"))]

  url_exts <- c()

  for(i in 1:length(games)){

    current_ext <- as.numeric(str_split(team_site[games[i]],"          <a target=\"BOX_SCORE_WINDOW\" class=\"skipMask\" href=\"/contests/|/box_score")[[1]][2])

    current_html <- glue("https://stats.ncaa.org/contests/{current_ext}/box_score") %>% readLines()

    url <- current_html[grep("Play by Play",current_html)[1]]

    url_upd <- as.numeric(str_split(url, "\t\t\t<a href=\"/game/play_by_play/|\">Play by Play</a>")[[1]][2])

    url_exts <- append(url_exts, url_upd)

  }

  possible_events <- c("struck out swinging",
                       "struck out looking",
                       "grounded out",
                       "flied out",
                       "infield fly",
                       "hit into double play",
                       "lined out",
                       "out",
                       "reached on a fielder's choice",
                       "reached on an error",
                       "reached on a throwing error",
                       "reached on a fielding error",
                       "lined into double play",
                       "grounded into double play",
                       "hit by pitch",
                       "walked",
                       "intentionally walked",
                       "fouled out",
                       "fouled into double play",
                       "popped up",
                       "singled",
                       "doubled",
                       "tripled",
                       "homered")

  get_pbp <- function(id){

    print(id)

    raw <- glue("https://stats.ncaa.org/game/play_by_play/{id}") %>%
      read_html() %>%
      html_table()

    upd <- raw[(5:length(raw))][c(F,T)]

    df <- do.call(rbind, upd)
    names(df) <- df[1,]

    team_col <- which(df[1,] == team_name)
    opponent_col <- ifelse(team_col == 3, 1, 3)

    filtered <- df %>%
      dplyr::rename(events = team_col) %>%
      select(events) %>%
      mutate(new_inning = events == team_name,
             inning = cumsum(new_inning)) %>%
      filter(str_detect(events, paste(possible_events, collapse = "|")) | str_detect(events, "\\(|H:")) %>%
      select(-new_inning) %>%
      group_by(inning) %>%
      mutate(player = word(events, 1, 2),
             inning_stats = last(events),
             inning_runs = ifelse(!str_detect(inning_stats, "R:"), 0, as.numeric(str_remove(regmatches(inning_stats, regexpr("R:\\s+(\\d+)", inning_stats)),"R: ")))) %>%
      ungroup() %>%
      select(player, inning_runs) %>%
      filter(!str_detect(player, "H: |R: ")) %>%
      mutate(game_id = id)

    print(nrow(filtered))

    return(filtered)

  }

  games <- data.frame()

  for(i in 1:length(url_exts)){

    games <- rbind(games, get_pbp(url_exts[i]))

  }

  stats <- games %>%
    mutate(player = word(player, 2, 2)) %>%
    group_by(player) %>%
    summarise(ab = n(),
              runs_generated = sum(inning_runs),
              avg_runs_generated = runs_generated / ab) %>%
    ungroup() %>%
    mutate(`+/-` = avg_runs_generated - mean(games$inning_runs)) %>%
    filter(ab >= 10) %>%
    arrange(desc(`+/-`))

  return(stats)

}

stats <- get_runs_generated(549186)
