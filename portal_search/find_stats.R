library(tidyverse)

source("~/CSE/ResearchAndDevelopment/sos_adjustment.R")
source("~/CSE/outs_above_expected.R")

fielding_stats <- player_stats %>% 
  separate(Player, c("last", "first"), sep = ", ") %>% 
  mutate(name = tolower(paste(first, last))) %>% 
  select(name, OAA, positions)

hitting_stats <- hitter_stats_upd %>% 
  mutate(name = tolower(Full.Name)) %>% 
  select(name, wRAA, adj_wRAA)

pitching_stats <- pitcher_stats_upd %>% 
  mutate(name = tolower(Full.Name)) %>% 
  select(name, FIP, adj_FIP)

hitting_portal <- read_csv("~/Projects/softball-projects/portal_search/transfer_portal.csv") %>% 
  filter(Year == "22-23" & D == "I") %>% 
  mutate(name = tolower(paste(`First Name`, `Last Name`))) %>% 
  select(name, Institution) %>% 
  merge(hitting_stats, by = "name") %>% 
  merge(fielding_stats, by = "name") %>% 
  distinct() %>% 
  arrange(desc(adj_wRAA))

pitching_portal <- read_csv("~/Projects/softball-projects/portal_search/transfer_portal.csv") %>% 
  filter(Year == "22-23") %>% 
  mutate(name = tolower(paste(`First Name`, `Last Name`))) %>% 
  select(name, Institution) %>% 
  merge(pitching_stats, by = "name") %>% 
  distinct() %>% 
  arrange(adj_FIP)

write_csv(hitting_portal, "~/Projects/softball-projects/portal_search/hitters.csv")
write_csv(pitching_portal, "~/Projects/softball-projects/portal_search/pitchers.csv")

by_team <- read_csv("~/Projects/softball-projects/portal_search/transfer_portal.csv") %>% 
  filter(Year == "22-23" & D == "I") %>% 
  group_by(Institution) %>% 
  summarise(count = n()) %>% 
  ungroup()
