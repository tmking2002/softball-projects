table <- get_power_ratings(scoreboard_2023) %>% 
  merge(logos, by.x = "team", by.y = "home_team_display_name") %>%
  rename(team_logo = home_team_logo) %>% 
  mutate(team_rank = rank(-power_rating)) %>% 
  select(team_logo, team, team_rank, power_rating, offensive_rating, defensive_rating, rank) %>% 
  arrange(desc(power_rating)) %>% 
  head(n = 15) %>% 
  gt() %>% 
  gt_img_rows(1) %>% 
  fmt_number(c(4,5,6), decimals = 2) %>% 
  cols_label(team_rank = "Rank",
             team_logo = "",
             team = "",
             offensive_rating = "Off. Rating",
             defensive_rating = "Def. Rating",
             rank = "SOS Rank",
             power_rating = "Power Rating") %>% 
  tab_header(title = "Strength of Schedule Adjusted Power Ratings",
             subtitle = "Through 4/2/23") %>% 
  tab_style(cell_borders(sides = "right",
                         style = "dashed",
                         color = "grey"),
            cells_body(columns = c(team, power_rating),
                       rows = everything())) %>%
  gt_theme_espn() %>%
  opt_align_table_header(align = "center") %>%
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px")

gtsave(table, "Current Standings.png")
