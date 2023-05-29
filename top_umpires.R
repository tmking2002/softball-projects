library(tidyverse)
library(gt)
library(gtExtras)

total <- read_csv("~/Projects/yakkertech_data/total.csv") %>% 
  drop_na(Umpire, PlateLocSide)

topKzone <- 11/3
botKzone <- 11/6
leftKzone <- -17/24
rightKzone <- 17/24

ballRadius <- (6 / pi) / 12

kZone = data.frame(
  x = c(leftKzone, leftKzone, rightKzone, rightKzone, leftKzone)
  , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

final <- total %>% 
  drop_na(PlateLocHeight) %>% 
  filter(PitchCall %in% c("StrikeCalled","BallCalled")) %>% 
  mutate(trueStrike = ifelse((PlateLocHeight - ballRadius) < topKzone & 
                               (PlateLocHeight + ballRadius) > botKzone & 
                               (PlateLocSide + ballRadius) > leftKzone & 
                               (PlateLocSide - ballRadius) < rightKzone,T,F),
         correctCall = ifelse((trueStrike == TRUE & PitchCall == "StrikeCalled") | (trueStrike == FALSE & PitchCall == "BallCalled"),T,F)) %>% 
  group_by(Umpire) %>% 
  mutate(umpire_id = cur_group_id()) %>% 
  ungroup()

stats <- final %>% 
  group_by(Umpire, umpire_id) %>% 
  summarise(pitches = n(),
            games = length(unique(GameID)),
            correct_perc = mean(correctCall)) %>% 
  ungroup()

top10 <- stats %>% 
  arrange(desc(correct_perc)) %>% 
  head(n=10) %>% 
  `names<-`(paste0("top_", names(stats))) %>% 
  mutate(top_rank = 1:10)

bot10 <- stats %>% 
  arrange(correct_perc) %>% 
  head(n=10) %>% 
  `names<-`(paste0("bot_", names(stats))) %>% 
  mutate(bot_rank = 36:27)

table <- cbind(top10, bot10) %>% 
  select(top_rank, top_pitches, top_correct_perc, bot_rank, bot_pitches, bot_correct_perc) %>% 
  gt() %>% 
  fmt_percent(c(top_correct_perc, bot_correct_perc), decimals = 1) %>% 
  cols_label(top_rank = "Rank",
             top_pitches = "Pitches",
             top_correct_perc = "Correct Call %",
             bot_rank = "Rank",
             bot_pitches = "Pitches",
             bot_correct_perc = "Correct Call %") %>% 
  data_color(columns = c(top_correct_perc, bot_correct_perc),
             colors = scales::col_numeric(
               palette = c("#FF6962", "#e58500", "#92c30c", "#77DE78"),
               domain = c(.65,.9)
             )) %>% 
  tab_style(style = cell_borders(sides = "left", style = "dashed"),
            locations = list(cells_body(columns = bot_rank),
                             cells_column_labels(columns = bot_rank))) %>% 
  tab_header(title = "Top 10 and Bottom 10 Umpires",
             subtitle = "By Strike Zone Accuracy") %>% 
  gt_theme_espn() %>% 
  tab_options(heading.align = "center")
