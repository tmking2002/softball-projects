source("~/Desktop/Softball/Get Current RPI.R")

library(gt)
library(gtExtras)

reversed_scoreboard <- scoreboard_upd %>% 
  mutate(win = case_when(abs(runs - opponent_runs) <= 1 ~ 1 - win,
                         TRUE ~ win),
         reversed = case_when(abs(runs - opponent_runs) <= 1 ~ TRUE,
                              TRUE ~ FALSE))

reversed <- reversed_scoreboard %>% 
  group_by(team_name) %>% 
  summarise(reversed = sum(reversed))

rpi <- get_current_rpi(scoreboard_upd) %>% select(team_name, rpi_rank, record)
reversed_rpi <- get_current_rpi(reversed_scoreboard) %>% select(team_name, rpi_rank, record)

table <- merge(reversed_rpi, rpi, by = "team_name") %>% 
  rename(actual_record = record.y,
         actual_rpi = rpi_rank.y,
         reversed_record = record.x,
         reversed_rpi = rpi_rank.x) %>% 
  mutate(rpi_diff = sprintf("%+d",actual_rpi - reversed_rpi)) %>% 
  arrange(reversed_rpi) %>% 
  head(n=10) %>% 
  gt(rowname_col = "team_name") %>% 
  cols_label(reversed_record = "Record",
             reversed_rpi = "RPI",
             actual_record = "Record",
             actual_rpi = "RPI",
             rpi_diff = "Diff") %>% 
  tab_spanner(columns = c(reversed_rpi, reversed_record),
              label = "'Alternate Universe'") %>% 
  tab_spanner(columns = c(actual_rpi, actual_record),
              label = "Actual") %>% 
  tab_style(style = cell_borders(sides = "right",
                                 color = "lightgrey",
                                 style = "solid"),
            locations = cells_body(columns = c(reversed_record, actual_record),
                                   rows = everything())) %>% 
  tab_style(style = cell_borders(sides = "right",
                                 color = "lightgrey",
                                 style = "solid"),
            locations = cells_column_labels(columns = c(reversed_record, actual_record))) %>% 
  tab_header(title = "RPI Rankings",
             subtitle = "(After reversing result of 1 run games)") %>% 
  gt_theme_espn() %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px")

gtsave(table, "~/Desktop/Softball/Alternate Universe RPI.png")
