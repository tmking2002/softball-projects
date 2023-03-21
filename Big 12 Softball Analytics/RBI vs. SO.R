source("Scrape Data.R")

rm(list=setdiff(ls(), c("team_hitting")))


library(cfbplotR)
library(hrbrthemes)

labels <- data.frame(x = c(150, 50, 150, 50),
                     y = c(25, 25, 125, 125),
                     label = c("+ RBI \n- SO",
                               "- RBI \n- SO",
                               "+ RBI \n+ SO",
                               "- RBI \n+ SO"))

ggplot(team_hitting, aes(x = RBI, y = SO)) +
  geom_cfb_logos(aes(team = Team), width = .075) +
  geom_label(data = labels, aes(x = x, y = y, label = label), size = 4, fill = "darkgrey", color = "white") +
  scale_y_reverse() +
  geom_vline(xintercept = mean(team_hitting$RBI), linetype = "dashed") +
  geom_hline(yintercept = mean(team_hitting$SO), linetype = "dashed") + 
  labs(title = "Big 12 RBIs vs. Strikeouts",
       subtitle = paste0("Through ", Sys.Date())) +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("RBI vs. SO.png")
