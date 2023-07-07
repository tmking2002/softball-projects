library(tidyverse)
library(rvest)
library(glue)
library(anytime)
library(lubridate)
library(plotly)
library(ggtext)
library(scales)

get_schedule <- function(url){
  #Fix accents, etc.
  res <- httr::RETRY("GET",url)
  resp <- res %>%
    httr::content(as = "text",encoding = "UTF-8")
  
  #Get data from JSON
  jackpot <- tryCatch(
    expr = {jsonlite::fromJSON(resp, flatten = TRUE)},
    error = function(err){"URL does not exist"}
  )
  
  schedule <- jackpot[[3]][[1]]
  
  return(schedule)
}

division_IDs <- c(830692, 830689, 830691, 830688, 830687)

schedule <- data.frame()

for(i in 1:5){
  
  url <- glue("https://softball.exposureevents.com/206253/2023-tc-nationals/eventgames?divisionId={division_IDs[i]}")
  
  schedule <- rbind(schedule, get_schedule(url))
  
}

teams_to_watch <- c("Aces Fastpitch", "Ga Impact Premier Caymol 18U", "Indiana Magic Gold 04 - Boteler",
                    "TAMPA MUSTANGS RENE", "Mojo Sherrill - 16u", "Texas Blaze-16U National KTX", 
                    "Unity Bridger 2024", "VA Glory National Mertz", "Virginia Unity Johnson/Ross",
                    "Atlanta Vipers 08 Tamborra", "Mojo Lunsford 08", "Mojo-Lewis", 
                    "Rhode Island Thunder 14u National Lotti", "Virginia Unity 14u-Johnson",
                    "AL Sparks Elite 16U - Stewart", "Fury Platinum National Chiles", 
                    "Iowa Premier 16U National", "GA Impact Premier - Sampson/Gusaeff",
                    "Top Gun 18 National", "Fury Platinum X-Hutchins", "Georgia Impact - Holcombe",
                    "Louisville Lady Sluggers - Wathen", "Tampa Mustangs Seymour", "Warrior Academy Jimenez")

important_games <- schedule %>% 
  filter(HomeTeamName %in% teams_to_watch | AwayTeamName %in% teams_to_watch) %>% 
  mutate(DateFormatted = anydate(DateFormatted),
         TimeFormatted = ifelse(str_detect(TimeFormatted, "AM") | word(TimeFormatted, 1, sep = ":| ") == 12, 
                                as.numeric(word(TimeFormatted, 1, sep = ":| ")),
                                as.numeric(word(TimeFormatted, 1, sep = ":| ")) + 12),
         StartTime = as.POSIXct(paste0(DateFormatted, " ", TimeFormatted, ":00")),
         EndTime = StartTime + hours(2),
         AwayTeamName = ifelse(AwayTeamName %in% teams_to_watch, paste0("<b>", str_pad(AwayTeamName, 50, side = "right"), "</b>"), str_pad(AwayTeamName, 50, side = "right")),
         HomeTeamName = ifelse(HomeTeamName %in% teams_to_watch, paste0("<b>", str_pad(HomeTeamName, 50, side = "right"), "</b>"), str_pad(HomeTeamName, 50, side = "right")),
         AwayTeamUnfmtd = trimws(str_remove_all(AwayTeamName, "<b>|</b>")),
         HomeTeamUnfmtd = trimws(str_remove_all(HomeTeamName, "<b>|</b>")),
         Matchup = paste0(AwayTeamName, " | ", HomeTeamName)) %>% 
  select(Id, VenueName, DateFormatted, StartTime, EndTime, Matchup, HomeTeamName, AwayTeamName, AwayTeamUnfmtd, HomeTeamUnfmtd, Venue) %>% 
  arrange(StartTime)

games_fmtd <- rbind(important_games[c(1:4, 6:11)] %>% `names<-`(c("Id", "VenueName", "DateFormatted", "Time", "Matchup", "AwayTeamName", "HomeTeamName", "AwayTeamUnfmtd", "HomeTeamUnfmtd", "Venue")),
                    important_games[c(1:3, 5:11)] %>% `names<-`(c("Id", "VenueName", "DateFormatted", "Time", "Matchup", "AwayTeamName", "HomeTeamName", "AwayTeamUnfmtd", "HomeTeamUnfmtd", "Venue")))

plot <- ggplot(games_fmtd %>% mutate(Time = Time - hours(4))) + 
  geom_line(aes(x = Time, y = reorder(Matchup, Time, decreasing = T), color = VenueName, text = paste0("Matchup: ", paste0(AwayTeamUnfmtd, " @ ", HomeTeamUnfmtd), "\n",
                                                                                                       "Venue: ", Venue, "\n",
                                                                                                       "Start Time: ", format(min(Time) + hours(4), "%I:%M %p"))), size = 3) +
  scale_x_datetime(limits = c(min(games_fmtd$Time - hours(4)), max(games_fmtd$Time) - hours(4)), breaks=date_breaks("2 hour"), labels=date_format("%I:%M %p"), timezone = "UTC") +
  theme(axis.text.y = element_markdown(),
        axis.title.y = element_blank()) +
  labs(color = "Venue Name")

ggplotly(plot, tooltip = "text")
