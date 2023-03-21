source("Scrape Data.R")

library(gt)
library(gtExtras)

individual_pitching_new <- individual_pitching %>% 
  separate(Player, c("Player","Team"), "\\(") %>% 
  separate(IP, c("Innings","Fraction"), "\\.") %>% 
  mutate(Team = str_remove(Team,"\\)"),
         Innings = as.numeric(Innings),
         Fraction = as.numeric(Fraction),
         IP = case_when(is.na(Fraction) ~ Innings,
                        TRUE ~ (Innings * 3 + Fraction) / 3),
         BB = BB + HBP,
         `K/7` = SO / IP * 7,
         `BB/7` = BB / IP * 7,
         FIP = (13*HR + 3*BB - 2*SO)/IP)

constant <- mean(individual_pitching_new$ERA) - mean(individual_pitching_new$FIP)

individual_pitching_new$FIP <- individual_pitching_new$FIP + constant


top_pitchers <- individual_pitching_new %>% 
  group_by(Team) %>% 
  filter(IP == max(IP)) %>% 
  ungroup() %>% 
  mutate() %>% 
  mutate(Player = case_when(Player == "Maxwell, Kelly " ~ "Kelly Maxwell",
                            Player == "Hoover, Sage " ~ "Sage Hoover",
                            Player == "Hamilton, Kasey " ~ "Kasey Hamilton",
                            Player == "Morgan, Mac " ~ "Mac Morgan",
                            Player == "Swain, Saya " ~ "Saya Swain",
                            Player == "WILKEY,Kenna " ~ "Kenna Wilkey",
                            Player == "Bahl, Jordy " ~ "Jordy Bahl",
                            Player == "Charles, Karlie " ~ "Karlie Charles",
                            Player == "Kaitlyn Felton " ~ "Kaitlyn Felton",
                            TRUE ~ Player),
         IP = round(IP, 1)) %>% 
  arrange(FIP)

headshots <- data.frame(Player = top_pitchers$Player,
                        Headshot = c("https://d1nnrx9kca53zl.cloudfront.net/images/2022/11/8/Dariana_Orme_Cropped.jpg?width=275",
                                     "https://d1yllc564ye8is.cloudfront.net/images/2022/9/6/MaxwellK02.jpg?width=80",
                                     "https://d30vqmatbr0w9y.cloudfront.net/images/2023/1/30/Hoover_Sage.png?width=80&quality=90",
                                     "https://ucfknights.com/images/2023/1/23/Kaitlyn.Felton._18.jpg?width=300",
                                     "https://dxa7m90h2v1am.cloudfront.net/images/2022/9/21/Morgan_Mac_web.jpg?width=80",
                                     "https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/soonersports.com/images/2021/11/17/Bahl.jpg&width=180&height=270&type=webp",
                                     "https://dtnbgpzadn69x.cloudfront.net/images/2022/8/29/Wilkey_Kenna.jpg?width=80",
                                     "https://dgqq6p65x0r0z.cloudfront.net/images/2022/12/5/Charles_Karlie.jpg?width=275",
                                     "https://kuathletics.com/wp-content/uploads/2019/11/Hamilton_Kasey_155-600x400.jpg"))
table <- top_pitchers  %>%
  merge(headshots) %>% 
  arrange(FIP) %>% 
  separate(IP, c("Innings","Fraction"), "\\.") %>% 
  mutate(IP = case_when(is.na(Fraction) ~ Innings,
                        Fraction == 3 ~ paste0(Innings,".1"),
                        Fraction == 7 ~ paste0(Innings,".2"))) %>% 
  select(Headshot, Player, Team, IP, `B/AVG`, HR, `BB/7`, `K/7`, FIP) %>% 
  gt() %>% 
  gt_img_circle(Headshot, height = 60) %>% 
  fmt_number(4, decimals = 1) %>% 
  fmt_number(7:9, decimals = 2) %>% 
  cols_label(Headshot = "--------") %>% 
  tab_header(title = "Big 12 Most Used Pitchers (By Team)",
             subtitle = paste0("Through ", Sys.Date())) %>%
  gt_theme_espn() %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px")

gtsave(table, "Most Used Pitchers.png")
