library(softballR)
library(nflplotR)
library(ggthemes)

source("~/Projects/softball-projects/get_power_ratings.R")

actual_rankings <- get_rankings("USA Today") %>% 
  select(Team, Rank) %>% 
  mutate(Team = trimws(Team),
         Team = ifelse(Team == "Oklahoma State", "Oklahoma St.", Team),
         Team = ifelse(Team == "Florida State", "Florida St.", Team),
         Team = ifelse(Team == "Wichita State", "Wichita St.", Team),
         Team = ifelse(Team == "Central Arkansas", "Central Ark.", Team))

scoreboard <- load_ncaa_scoreboard(2023)

power_rankings <- get_power_ratings(scoreboard) %>% 
  arrange(desc(power_rating)) %>% 
  mutate(power_rank = rank(-power_rating),
         team = ifelse(team == "Texas A&amp;M", "Texas A&M", team)) %>% 
  select(team, power_rank)

logos <- scoreboard %>% 
  distinct(home_team, home_team_logo) %>% 
  mutate(home_team = ifelse(home_team == "Texas A&amp;M", "Texas A&M", home_team))

rankings <- merge(actual_rankings, power_rankings, by.x = "Team", by.y = "team", all=T) %>% 
  merge(logos, by.x = "Team", by.y = "home_team")

plot <- ggplot(rankings %>% mutate(power_rank = ifelse(!is.na(Rank) & power_rank > 25, 25, power_rank),
                                   Rank = ifelse()), 
       aes(x = power_rank, y = Rank)) +
  geom_from_path(aes(path = home_team_logo), height = .05) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25), limits = c(1,25)) +
  scale_y_continuous(breaks = c(1,5,10,15,20,25), limits = c(1,25)) +
  geom_label(x = 22, y = 3, label = "Overvalued by USA Today", size = 3) +
  geom_label(x = 4, y = 23, label = "Undervalued by USA Today", size = 3) +
  labs(x = "Power Ranking",
       y = "USA Today Ranking",
       title = "D1 NCAA Softball Rankings",
       subtitle = "Through 4/14/23") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_blank())

ggsave(filename = "~/Projects/softball-projects/compare_rankings.png")
