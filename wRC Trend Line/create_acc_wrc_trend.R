library(tidyverse)
library(anytime)
library(ggthemes)
library(ggimage)
library(cropcircles)


box <- read_csv("~/Desktop/Projects/softball-projects/ACC Box Scores.csv")
headshots <- read_csv("~/Desktop/Projects/softball-projects/acc_headshots.csv") %>%
  mutate(name = ifelse(name == "Rodriguez, Kat", "Katherine Rodriguez", name)) %>%
  rbind(data.frame(name = c("Daisy Hess","Korbe Otis","Taylor Roby", "Elana Ornelas"),
                   url = c("https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/gocards.com/images/2023/1/23/2023SBHSWeb_Hess_O4qKb.png&width=180&height=270",
                          "https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/gocards.com/images/2021/10/1/Otis_Web_TS_SBC2021_22_HEADSHOTS_0029.jpg&width=180&height=270",
                          "https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/gocards.com/images/2021/10/1/Roby_Web_TS_SBC2021_22_HEADSHOTS_0116.jpg&width=180&height=270",
                          "https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/gocards.com/images/2021/10/1/Ornelas_Web_TS_SBC2021_22_HEADSHOTS_0136.jpg&width=180&height=270"),
                   team = "Louisville"))

dates <- data.frame(week = 1:5,
                    start_date = seq(as.Date("2023-02-06"), as.Date("2023-03-12"), by = 7),
                    end_date = seq(as.Date("2023-02-12"), as.Date("2023-03-18"), by = 7))

box_upd <- box %>%
  crossing(dates) %>%
  separate(game_date, c("month", "day", "year"), sep = "/") %>%
  mutate(game_date_upd = as.Date(paste0(year, "-", month, "-", day))) %>%
  filter(game_date_upd <= end_date & game_date_upd >= start_date)

box_upd[is.na(box_upd)] <- 0

stats_by_week <- data.frame()

for(i in 1:5){

  curr <- box_upd %>%
    filter(week <= i) %>%
    dplyr::mutate(`1B` = H - `2B` - `3B` - HR) %>%
    dplyr::group_by(Player, team) %>%
    dplyr::filter(sum(AB) >= 5 * i) %>%
    dplyr::summarise(wOBA = (.69 * sum(BB) + .72 * sum(HBP) + .89 * sum(`1B`) + 1.27 * sum(`2B`) + 1.62 * sum(`3B`) + 2.1 * sum(HR)) /
                              (sum(AB) + sum(BB) + sum(HBP) + sum(SF) + sum(SH)),
                     week = i)

  stats_by_week <- rbind(stats_by_week, curr)

}

stats_final <- stats_by_week %>%
  group_by(week) %>%
  mutate(rank = rank(-wOBA)) %>%
  filter(rank <= 5) %>%
  separate(Player, c("last", "first"), sep = ", ") %>%
  mutate(name = paste0(first, " ", last)) %>%
  merge(headshots %>% select(-team), by = "name")


ggplot(stats_final, aes(x = week, y = wOBA, group = name)) +
  geom_image(aes(image = circle_crop(url)), size = .05, alpha = .5) +
  theme_few() +
  labs(x = "Week #",
       y = "Cumulative wOBA",
       title = "ACC Top Hitters by Week",
       subtitle = "Through 3-12-23") +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5))
  transition_reveal(through)

anim_save("wRC Trend over Time.gif")


stats <- box_upd %>%
  mutate(`1B` = H - `2B` - `3B` - HR) %>%
  separate(Player, c("last", "first"), sep = ", ") %>%
  mutate(name = paste0(first, " ", last)) %>%
  merge(headshots %>% select(-team), by = "name") %>%
  group_by(name, team, url) %>%
  summarise(games = n(),
            PA = (sum(AB) + sum(BB) + sum(HBP) + sum(SF) + sum(SH)),
            wOBA = (.69 * sum(BB) + .72 * sum(HBP) + .89 * sum(`1B`) + 1.27 * sum(`2B`) + 1.62 * sum(`3B`) + 2.1 * sum(HR)) / PA,
            OBP = (sum(H) + sum(BB) + sum(HBP)) / PA) %>%
  ungroup() %>%
  filter(PA >= 40) %>%
  arrange(desc(wOBA))

avg_obp <- mean(stats$OBP)
avg_woba <- mean(stats$wOBA)

stats$wOBA <- stats$wOBA + (avg_obp - avg_woba)

ggplot(stats[1,], aes(x = PA, y = wOBA)) +
  geom_image(aes(image = circle_crop(url)), size = .1, alpha = .5) +
  xlim(40, max(stats$PA + 10)) +
  ylim(0, 1)
