library(tidyverse)
library(ggimage)
library(cropcircles)
library(ggthemes)
library(gganimate)

team_id <- get_ncaa_teams(2023) %>%
  filter(team_name == "NC State") %>%
  pull(team_id)

box <- get_ncaa_player_box(team_id)

game_no <- box$Hitting %>%
  filter(team == "NC State") %>%
  dplyr::group_by(game_date) %>%
  dplyr::summarise(game_no = n() * 0) %>%
  dplyr::mutate(game_no = game_no + row_number())

hitting <- box$Hitting %>%
  filter(team == "NC State") %>%
  merge(game_no, by = "game_date")

stats <- hitting %>%
  filter(game_no %in% 1:5) %>%
  dplyr::group_by(Player) %>%
  dplyr::filter(sum(AB) >= 5) %>%
  dplyr::summarise(wRC = ((sum(TB) * (sum(H) + sum(BB))) / (sum(AB) + sum(BB))),
                   through = 5)

for(i in 6:21){

  curr <- hitting %>%
    filter(game_no %in% (i - 4):i) %>%
    dplyr::group_by(Player) %>%
    dplyr::filter(sum(AB) >= 5) %>%
    dplyr::summarise(wRC = ((sum(TB) * (sum(H) + sum(BB))) / (sum(AB) + sum(BB))),
                     through = i)

  stats <- rbind(stats, curr)

}

headshots <- data.frame(Player = unique(stats$Player),
                        Headshot = c("https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/44.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/10.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/77.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/5.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/3.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/21.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/15.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/20.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/42.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/27.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/8.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/33.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/1.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/51.png?width=80",
                                     "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/11.png?width=80"))

stats_final <- stats %>%
  merge(headshots, by = "Player") %>%
  group_by(through) %>%
  mutate(rank = rank(-wRC))

ggplot(stats_final, aes(x = through, y = wRC, group = Player)) +
  geom_line() +
  geom_image(aes(image = circle_crop(Headshot)), size = .1, alpha = .8) +
  theme_few() +
  labs(x = "Game #",
       y = "wRC (5 Game Rolling Average)",
       title = "wRC Trends (NCSU 2023)",
       subtitle = "Through 3-23-23") +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5)) +
  transition_reveal(through)

anim_save("wRC Trend over Time.gif")
