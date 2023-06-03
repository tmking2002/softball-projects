table <- coaches %>% 
  group_by(head_coach, team_id) %>% 
  summarise(wins = sum(wins),
            losses = sum(losses),
            win_perc = wins / (wins + losses),
            first_season = min(season),
            last_season = max(season)) %>% 
  ungroup() %>% 
  mutate(seasons = paste(first_season, last_season, sep = "-")) %>% 
  merge(teams, by = "team_id") %>% 
  arrange(desc(wins)) %>% 
  head(n=10) %>% 
  select(team_logo, head_coach, wins, losses, win_perc, seasons) %>% 
  gt() %>% 
  cols_label(team_logo = "",
             head_coach = "Head Coach",
             wins = "W",
             losses = "L",
             win_perc = "Win%",
             seasons = "Seasons") %>% 
  gt_img_rows(team_logo) %>% 
  fmt_percent(win_perc, decimals = 1) %>% 
  tab_style(style = cell_fill(color = "#841617"),
            locations = cells_body(columns = 2:6,
                                   rows = head_coach == "Patty Gasso")) %>% 
  tab_style(style = cell_text(color = "white"),
            locations = cells_body(columns = 2:6,
                                   rows = head_coach == "Patty Gasso")) %>% 
  tab_style(style = cell_borders(sides = "bottom", color = "lightgrey"),
            locations = cells_body(rows = head_coach == "Margo Jonker")) %>% 
  tab_header(title = "Winningest Coaches With One Team All Time",
             subtitle = "(NCAA Softball)") %>% 
  tab_footnote(footnote = "Data from softballR") %>% 
  gt_theme_538() %>% 
  tab_options(heading.align = 'center')

gtsave(table, "~/Desktop/Projects/softball-projects/coaches_wins_table.png")

wins_by_season <- coaches %>% 
  merge(teams, by = "team_id") %>% 
  filter(head_coach %in% c("Carol Hutchins", "Mike Candrea", "Patty Gasso", "Lori Meyer", "Diane Ninemire")) %>% 
  group_by(head_coach, team_logo) %>% 
  arrange(season) %>% 
  summarise(year = row_number(),
            wins = cumsum(wins))

ggplot(wins_by_season, aes(x = year, y = wins, group = head_coach)) +
  geom_from_path(data = wins_by_season %>% group_by(head_coach) %>% filter(year == max(year)),
                 aes(path = team_logo), width = 0.08) + 
  geom_line(aes(color = head_coach),  linewidth = 1) + 
  scale_color_manual(values = c("Patty Gasso" = "#841617")) +
  geom_hline(yintercept = max(wins_by_season$wins), linetype = "dashed") +
  theme_ipsum() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Cumulative Wins by Season",
       subtitle = "(By Coach)",
       caption = "Data from softballR | Viz by @tking0426",
       x = "Season",
       y = "Wins")

ggsave("~/Desktop/Projects/softball-projects/wins_by_season.png")  

ggplot(wins_by_season %>% filter(year >= 25), aes(x = year, y = wins, group = head_coach)) +
  geom_from_path(data = wins_by_season %>% group_by(head_coach) %>% filter(year == max(year)),
                 aes(path = team_logo), width = 0.08) + 
  geom_line(aes(color = head_coach),  linewidth = 1) + 
  scale_color_manual(values = c("Patty Gasso" = "#841617")) +
  geom_hline(yintercept = max(wins_by_season$wins), linetype = "dashed") +
  geom_line(data = data.frame(head_coach = "Patty Gasso",
                              year = 29:35,
                              wins = 1444 + seq(0,300,50)),
            linetype = "dashed",
            color = "#841617") +
  theme_ipsum() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Cumulative Wins by Season",
       subtitle = "(By Coach)",
       caption = "Data from softballR | Viz by @tking0426",
       x = "Season",
       y = "Wins")

ggsave("~/Desktop/Projects/softball-projects/wins_by_season_projected.png")  
