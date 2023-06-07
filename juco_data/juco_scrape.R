library(tidyverse)
library(rvest)
library(janitor)

url <- "https://www.njcaa.org/sports/sball/teams-page"

raw <- url %>%
  readLines()

stat_urls <- data.frame(url = paste0("https://www.njcaa.org",str_match(raw[grep("?view=lineup", raw)], "<a href=\"([^\"]+)\"")[, 2]),
                        team_name = str_match(raw[grep("college-name", raw)], "<a href=\"[^\"]+/[^/]+\" class=\"college-name\">([^<]+)")[, 2])

stat_urls <- stat_urls %>%
  separate(url, into = c(letters[1:7],"division"), sep = "div|\\/", remove = F) %>%
  select(url, team_name, division)

get_stats <- function(url){

  raw <- url %>%
    read_html() %>%
    html_table()

  hitting <- merge(raw[[3]], raw[[5]]) %>%
    clean_names() %>%
    filter(!(name %in% c("Opponent", "Totals")))

  pitching <- raw[[7]] %>%
    clean_names() %>%
    filter(!(name %in% c("Opponent", "Totals")))

  fielding <- raw[[9]] %>%
    clean_names() %>%
    filter(!(name %in% c("Opponent", "Totals")))

  return(list(hitting, pitching, fielding))

}

hitting <- data.frame()
pitching <- data.frame()
fielding <- data.frame()

missing_teams <- setdiff(stat_urls$team_name, hitting$team_name)
missing_urls <- stat_urls %>%
  filter(team_name %in% missing_teams)

for(i in 1:nrow(missing_urls)){

  stats <- try(get_stats(missing_urls$url[i]))

  if("try-error" %in% class(stats)) next

  hitting <- rbind(hitting, stats[[1]] %>% mutate(team_name = missing_urls$team_name[i], division = missing_urls$division[i]))
  pitching <- rbind(pitching, stats[[2]] %>% mutate(team_name = missing_urls$team_name[i], division = missing_urls$division[i]))
  fielding <- rbind(fielding, stats[[3]] %>% mutate(team_name = missing_urls$team_name[i], division = missing_urls$division[i]))

  if(i %% 10 == 0) print(i)

}


hitting_final <- hitting %>%
  distinct() %>%
  filter(name != "No players meet the minimum") %>%
  mutate(
    across(
      .cols = c(1,5:29),
      .fns = \(col) as.numeric(str_replace(col, "-", "0"))
      ),
    yr = case_when(str_detect(tolower(yr), "so") ~ "Sophomore",
                   str_detect(tolower(yr), "fr") ~ "Freshman",
                   TRUE ~ yr)
    ) %>%
  separate(name, c("first_name", "last_name"), sep = "  ")


pitching_final <- pitching %>%
  distinct() %>%
  filter(name != "No players meet the minimum") %>%
  mutate(
    across(
      .cols = c(5:10, 12:19),
      .fns = \(col) as.numeric(str_replace(col, "-", "0"))
    ),
    yr = case_when(str_detect(tolower(yr), "so") ~ "Sophomore",
                   str_detect(tolower(yr), "fr") ~ "Freshman",
                   TRUE ~ yr)
  ) %>%
  separate(name, c("first_name", "last_name"), sep = "  ")

fielding_final <- fielding %>%
  distinct() %>%
  filter(name != "No players meet the minimum") %>%
  mutate(
    across(
      .cols = c(5:16),
      .fns = \(col) as.numeric(str_replace(col, "-", "0"))
    ),
    yr = case_when(str_detect(tolower(yr), "so") ~ "Sophomore",
                   str_detect(tolower(yr), "fr") ~ "Freshman",
                   TRUE ~ yr)
  ) %>%
  separate(name, c("first_name", "last_name"), sep = "  ")


write_csv(hitting_final, "juco_hitting_2023.csv")
write_csv(pitching_final, "juco_pitching_2023.csv")
write_csv(fielding_final, "juco_fielding_2023.csv")
