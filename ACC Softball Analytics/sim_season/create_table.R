library(softballR)
library(tidyverse)

setwd("~/Desktop/Projects/softball-projects/ACC Softball Analytics/sim_season")

source("get_current_rpi.R")
source("get_power_ratings.R")
source("acc_season_sims.R")

scoreboard <- load_ncaa_scoreboard(2023)

standings <- get_power_ratings(scoreboard)

results <- sim_season(scoreboard, 1000)

table <- results[[1]]
records <- results[[2]]
results <- results[[3]]