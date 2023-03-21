source("Scrape Data.R")

library(gt)
library(gtExtras)

rm(list=setdiff(ls(), "individual_hitting"))

proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

individual_hitting_new <- individual_hitting %>% 
  separate(Player, c("Player","Team"), "\\(") %>% 
  separate(Player, c("Last", "First"), ",") %>% 
  mutate(Player = paste(proper(str_remove_all(First," ")), proper(str_remove_all(Last," "))),
         Team = str_remove(Team, "\\)"),
         XBH = `2B` + `3B` + HR,
         RC = (TB * (H + BB)) / (AB + BB + HBP))

top_hitters <- individual_hitting_new %>% 
  arrange(desc(RC)) %>% 
  head(n = 10)

headshots <- data.frame(Player = top_hitters$Player,
                        Headshot = c("https://dxa7m90h2v1am.cloudfront.net/images/2022/9/21/Goode_Leighann_web.jpg?width=300",
                                     "https://d30vqmatbr0w9y.cloudfront.net/images/2023/1/30/Wyckoff_Kailey.png?width=300",
                                     "https://images.sidearmdev.com/crop?url=https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/soonersports.com/images/2022/12/1/Erickson_Jocelyn.jpg&width=180&height=270&type=webp",
                                     "https://d1e6xbptvrg0sy.cloudfront.net/images/2022/1/21/HERZOG_Makinzy.JPG?width=300",
                                     "https://dxa7m90h2v1am.cloudfront.net/images/2022/1/26/scott_mia_sss_TexSoftball2299.jpg?width=300",
                                     "https://d30vqmatbr0w9y.cloudfront.net/images/2023/1/30/Villa_Arriana.png?width=300",
                                     "https://d1yllc564ye8is.cloudfront.net/images/2022/9/6/NaomiK03.jpg?width=300",
                                     "https://d30vqmatbr0w9y.cloudfront.net/images/2023/1/30/Bailey_Ellie.png?width=300",
                                     "https://dtnbgpzadn69x.cloudfront.net/images/2022/8/29/Coleman_Turiya.jpg?width=300",
                                     "https://dxa7m90h2v1am.cloudfront.net/images/2022/9/21/Martinez_Viviana_web.jpg?width=300"))


table <- top_hitters %>% 
  merge(headshots) %>% 
  select(Headshot, Player, Team, AVG, OPS, AB, XBH, HR, RBI, RC) %>% 
  arrange(desc(RC)) %>% 
  gt() %>% 
  gt_img_circle(Headshot, height = 60) %>% 
  cols_label(Headshot = "--------") %>% 
  tab_header(title = "Big 12 Top Hitters",
             subtitle = paste0("Through ", Sys.Date())) %>% 
  fmt_number(RC, decimals = 2) %>% 
  gt_theme_espn() %>% 
  opt_align_table_header(align = "center") %>% 
  tab_options(heading.title.font.weight = "bold",
              heading.title.font.size = "24px")

gtsave(table, "Top Hitters.png")
