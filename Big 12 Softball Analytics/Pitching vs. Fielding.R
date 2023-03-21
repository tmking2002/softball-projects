source("Scrape Data.R")

rm(list=setdiff(ls(), c("team_hitting","team_pitching","team_fielding","individual_hitting","individual_pitching","individual_fielding")))
                        

library(cfbplotR)
library(hrbrthemes)

pitching_vs_fielding <- merge(team_pitching[1:2], team_fielding[c(1,6)])

labels <- data.frame(x = c(2, 6, 6, 2),
                     y = c(.935, .935, .975, .975),
                     label = c("+ Pitching \n- Fielding",
                               "- Pitching \n- Fielding",
                               "- Pitching \n+ Fielding",
                               "+ Pitching \n+ Fielding"))

ggplot(pitching_vs_fielding, aes(x = ERA, y = `FLD%`)) +
  geom_cfb_logos(aes(team = Team), width = .075) +
  geom_label(data = labels, aes(x = x, y = y, label = label), size = 4, fill = "darkgrey", color = "white") +
  scale_x_reverse() +
  geom_vline(xintercept = mean(pitching_vs_fielding$ERA), linetype = "dashed") +
  geom_hline(yintercept = mean(pitching_vs_fielding$`FLD%`), linetype = "dashed") + 
  labs(title = "Big 12 Pitching vs. Fielding Stats",
       subtitle = paste0("Through ", Sys.Date())) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Pitching vs. Fielding Stats.png")
