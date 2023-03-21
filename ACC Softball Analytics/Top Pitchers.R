library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(gt)

setwd("~/Desktop/Softball/ACC Softball Analytics")

#### Clemson + Virginia ####

clemson_virginia_1 <- "https://data.clemsontigers.com/Stats/Softball/2023/cusb2823.htm" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

clemson_virginia_2 <- "https://data.clemsontigers.com/Stats/Softball/2023/cusb2923.htm" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)
  
clemson_virginia_3 <- "https://data.clemsontigers.com/Stats/Softball/2023/cusb3023.htm" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

clemson_virginia <- rbind(clemson_virginia_1, clemson_virginia_2, clemson_virginia_3) %>% 
  `names<-`(c("Player", clemson_virginia_1[1,2:16])) %>% 
  drop_na(ip) %>% 
  filter(!(Player %in% c("Virginia", "Clemson"))) %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

##### FSU + Syracuse ####

fsu_syracuse_1_1 <- "https://cuse.com/sports/softball/stats/2023/florida-state/boxscore/16447" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

fsu_syracuse_1_2 <- "https://cuse.com/sports/softball/stats/2023/florida-state/boxscore/16447" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

fsu_syracuse_1 <- rbind(fsu_syracuse_1_1, fsu_syracuse_1_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

fsu_syracuse_2_1 <- "https://cuse.com/sports/softball/stats/2023/florida-state/boxscore/16448" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

fsu_syracuse_2_2 <- "https://cuse.com/sports/softball/stats/2023/florida-state/boxscore/16448" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

fsu_syracuse_2 <- rbind(fsu_syracuse_2_1, fsu_syracuse_2_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

fsu_syracuse_3_1 <- "https://cuse.com/sports/softball/stats/2023/florida-state/boxscore/16449" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

fsu_syracuse_3_2 <- "https://cuse.com/sports/softball/stats/2023/florida-state/boxscore/16449" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

fsu_syracuse_3 <- rbind(fsu_syracuse_3_1, fsu_syracuse_3_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

fsu_syracuse <- rbind(fsu_syracuse_1, fsu_syracuse_2, fsu_syracuse_3)

##### Duke + UNC ####

duke_unc_1_1 <- "https://goduke.com/sports/softball/stats/2023/north-carolina/boxscore/21028" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

duke_unc_1_2 <- "https://goduke.com/sports/softball/stats/2023/north-carolina/boxscore/21028" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

duke_unc_1 <- rbind(duke_unc_1_1, duke_unc_1_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

duke_unc_2_1 <- "https://goduke.com/sports/softball/stats/2023/north-carolina/boxscore/21029" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

duke_unc_2_2 <- "https://goduke.com/sports/softball/stats/2023/north-carolina/boxscore/21029" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

duke_unc_2 <- rbind(duke_unc_2_1, duke_unc_2_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

duke_unc_3_1 <- "https://goduke.com/sports/softball/stats/2023/north-carolina/boxscore/21030" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

duke_unc_3_2 <- "https://goduke.com/sports/softball/stats/2023/north-carolina/boxscore/21030" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

duke_unc_3 <- rbind(duke_unc_3_1, duke_unc_3_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

duke_unc <- rbind(duke_unc_1, duke_unc_2, duke_unc_3)

#### Louisville + Pitt ####

louisville_pitt_1_1 <- "https://pittsburghpanthers.com/sports/softball/stats/2023/louisville/boxscore/13602" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

louisville_pitt_1_2 <- "https://pittsburghpanthers.com/sports/softball/stats/2023/louisville/boxscore/13602" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

louisville_pitt_1 <- rbind(louisville_pitt_1_1, louisville_pitt_1_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

louisville_pitt_2_1 <- "https://pittsburghpanthers.com/sports/softball/stats/2023/louisville/boxscore/13603" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

louisville_pitt_2_2 <- "https://pittsburghpanthers.com/sports/softball/stats/2023/louisville/boxscore/13603" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

louisville_pitt_2 <- rbind(louisville_pitt_2_1, louisville_pitt_2_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

louisville_pitt_3_1 <- "https://pittsburghpanthers.com/sports/softball/stats/2023/louisville/boxscore/13604" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

louisville_pitt_3_2 <- "https://pittsburghpanthers.com/sports/softball/stats/2023/louisville/boxscore/13604" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

louisville_pitt_3 <- rbind(louisville_pitt_3_1, louisville_pitt_3_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

louisville_pitt <- rbind(louisville_pitt_1, louisville_pitt_2, louisville_pitt_3)

#### VT + GT ####

vt_gt_1_1 <- "https://hokiesports.com/sports/softball/stats/2023/georgia-tech/boxscore/25010" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

vt_gt_1_2 <- "https://hokiesports.com/sports/softball/stats/2023/georgia-tech/boxscore/25010" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

vt_gt_1 <- rbind(vt_gt_1_1, vt_gt_1_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

vt_gt_2_1 <- "https://hokiesports.com/sports/softball/stats/2023/georgia-tech/boxscore/25011" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

vt_gt_2_2 <- "https://hokiesports.com/sports/softball/stats/2023/georgia-tech/boxscore/25011" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

vt_gt_2 <- rbind(vt_gt_2_1, vt_gt_2_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

vt_gt_3_1 <- "https://hokiesports.com/sports/softball/stats/2023/georgia-tech/boxscore/25012" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

vt_gt_3_2 <- "https://hokiesports.com/sports/softball/stats/2023/georgia-tech/boxscore/25012" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

vt_gt_3 <- rbind(vt_gt_3_1, vt_gt_3_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

vt_gt <- rbind(vt_gt_1, vt_gt_2, vt_gt_3)

#### NCSU + BC ####

ncsu_bc_1_1 <- "https://gopack.com/sports/softball/stats/2023/boston-college/boxscore/22606" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

ncsu_bc_1_2 <- "https://gopack.com/sports/softball/stats/2023/boston-college/boxscore/22606" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

ncsu_bc_1 <- rbind(ncsu_bc_1_1, ncsu_bc_1_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

ncsu_bc_2_1 <- "https://gopack.com/sports/softball/stats/2023/boston-college/boxscore/22607" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

ncsu_bc_2_2 <- "https://gopack.com/sports/softball/stats/2023/boston-college/boxscore/22607" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

ncsu_bc_2 <- rbind(ncsu_bc_2_1, ncsu_bc_2_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

ncsu_bc_3_1 <- "https://gopack.com/sports/softball/stats/2023/boston-college/boxscore/22608" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(5)

ncsu_bc_3_2 <- "https://gopack.com/sports/softball/stats/2023/boston-college/boxscore/22608" %>% 
  read_html() %>% 
  html_table() %>% 
  extract2(6)

ncsu_bc_3 <- rbind(ncsu_bc_3_1, ncsu_bc_3_2) %>% 
  filter(Player != "Totals") %>% 
  mutate(`W/L` = case_when(str_detect(Player, "W,") ~ "W",
                           str_detect(Player, "L,") ~ "L",
                           TRUE ~ NA),
         Player = word(Player, 1, 2))

ncsu_bc <- rbind(ncsu_bc_1, ncsu_bc_2, ncsu_bc_3)


rm(list = setdiff(ls(), c("clemson_virginia", "fsu_syracuse", "duke_unc", "louisville_pitt", "vt_gt", "ncsu_bc")))

#### Create Stats ####

total <- rbind(clemson_virginia %>% `names<-`(names(fsu_syracuse)), fsu_syracuse, duke_unc, louisville_pitt, vt_gt, ncsu_bc) %>% 
  separate(IP, c("total","part"), "\\.") %>% 
  mutate(IP = ifelse(is.na(part), as.numeric(total), as.numeric(total) + as.numeric(part) / 3)) %>% 
  select(-c(total, part)) %>% 
  mutate(W = ifelse(`W/L` == "W" & !is.na(`W/L`), 1, 0),
         across(c(2:15,18), as.numeric))
  
stats <- total %>% 
  group_by(Player) %>% 
  summarise(across(is.numeric, sum)) %>% 
  ungroup() %>% 
  mutate(ERA = ER / IP * 7,
         WHIP = (BB + HBP + H) / IP,
         Kr = SO / IP * 7,
         BBr = (BB + HBP) / IP * 7) %>% 
  filter(IP >= 7) %>% 
  mutate(Player = case_when(Player == "Thompson,Millie " ~ "Millie Thompson",
                            Player == "Cagle,Valerie " ~ "Valerie Cagle",
                            Player == "Inscoe, Madison" ~ "Madison Inscoe",
                            Player == "Weixlmann, Aisha" ~ "Aisha Weixlmann",
                            Player == "Lemley, Emma" ~ "Emma Lemley",
                            TRUE ~ Player)) %>% 
  arrange(ERA) %>% 
  head(n = 8)

headshots <- c("https://clemsontigers.com/wp-content/uploads/2022/10/CagleValerie.png",
               "https://seminoles.com/wp-content/uploads/2022/09/Sandercock-Katherine-22-174RL-311-768x960.jpg",
               "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/21.png?width=300",
               "https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/gocards.com/images/2023/1/23/Zabala_QoWDF.png&width=180&height=270&type=webp",
               "https://clemsontigers.com/wp-content/uploads/2022/10/ThompsonMillie.png",
               "https://d1qkubt1nbt8w1.cloudfront.net/images/2022/9/11/24.png?width=300",
               "https://d1cv0f55ge5i54.cloudfront.net/images/2022/9/8/Lemley_Emma_23SB_HEAD_46.jpg?width=275",
               "https://bceagles.com/images/2023/1/19/QUA_8518.jpg?width=300")

teams <- c("Clemson", "FSU", "NCSU", "Louisville", "Clemson", "NCSU", "VT", "BC")

table <- stats %>% 
  mutate(headshot = headshots,
         team = teams) %>% 
  select(headshot, Player, team, IP, W, ERA, WHIP, Kr, BBr) %>% 
  gt() %>% 
  gt_img_circle(headshot, height = 60) %>% 
  fmt_number(IP, decimals = 0) %>% 
  fmt_number(6:9, decimals = 2) %>%
  cols_label(headshot = "--------",
             team = "Team",
             Kr = "K Rate",
             BBr = "BB Rate") %>% 
  data_color(columns = ERA,
             palette = c("#98FB98","#ff6961"),
             domain = c(max(stats$ERA), min(stats$ERA))) %>% 
  data_color(columns = WHIP,
             palette = c("#98FB98","#ff6961"),
             domain = c(max(stats$WHIP), min(stats$WHIP))) %>% 
  data_color(columns = BBr,
             palette = c("#98FB98","#ff6961"),
             domain = c(max(stats$BBr), min(stats$BBr))) %>% 
  data_color(columns = Kr,
             palette = c("#ff6961","#98FB98"),
             domain = c(max(stats$Kr), min(stats$Kr))) %>% 
  tab_header(title = "ACC Top Pitchers",
             subtitle = "3/18 - 3/20 Weekend") %>%
  gt_theme_espn() %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px") %>% 
  tab_style(style = cell_borders(sides = c("left","right"),
                                 color = "grey"),
            locations = cells_body(columns = 6:9)) %>% 
  tab_footnote(footnote = "**Hit walk off single to complete sweep",
               locations = cells_body(columns = Player,
                                      row = Player == "Madison Inscoe"))

gtsave(table, "Top Pitchers.png")  
  
  
