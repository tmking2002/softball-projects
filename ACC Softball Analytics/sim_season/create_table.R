library(softballR)
library(tidyverse)

setwd("~/Projects/softball-projects/ACC Softball Analytics/sim_season")

source("get_current_rpi.R")
source("get_power_ratings.R")
source("acc_season_sims.R")

scoreboard <- load_ncaa_scoreboard(2023)

standings <- get_power_ratings(scoreboard)

results <- sim_season(scoreboard, 5000)

table <- results[[1]]
records <- results[[2]]
results <- results[[3]]

tenth <- records %>% 
  filter(rank == 10)

ggplot(tenth, aes(x = wins)) +
  geom_histogram(aes(y = after_stat(count / sum(count)))) +
  labs(x = "Wins",
       y = "Percentage",
       title = "Wins Needed to Clinch 10th Place") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

gtsave(table,"acc_predictions_4_10.png")
