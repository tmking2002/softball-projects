library(tidyverse)
library(magrittr)

#### BC ####

bc <- "https://bceagles.com/sports/softball/roster" %>%
  readLines()

bc_headshots_raw <- bc[grep("img class=\"lazyload\" data-src=\"",bc)] %>% str_split("\"")

bc_headshots <- data.frame()

for(i in 1:23){

  bc_headshots <- rbind(bc_headshots, data.frame(name = bc_headshots_raw[[i]][6] %>% str_remove(" Headshot"),
                                                     url = paste0("https://bceagles.com/",bc_headshots_raw[[i]][4]),
                                                     team = "Boston College"))

}

#### CLEMSON ####

clemson <- "https://clemsontigers.com/sports/softball/roster" %>%
  readLines()

clemson_headshots_raw <- clemson[38] %>% str_split("person__image\"> <a href=") %>% extract2(1) %>% str_split("\"")

clemson_headshots <- data.frame()

for(i in 2:25){

  clemson_headshots <- rbind(clemson_headshots, data.frame(name = clemson_headshots_raw[[i]][6],
                                                 url = clemson_headshots_raw[[i]][4],
                                                 team = "Clemson"))

}

#### DUKE ####

duke <- "https://goduke.com/sports/softball/roster" %>%
  readLines()

duke_headshots_raw <- duke[grep("img class=\"lazyload\" data-src=\"",duke)] %>% str_split("\"")

duke_headshots <- data.frame()

for(i in 1:20){

  duke_headshots <- rbind(duke_headshots, data.frame(name = duke_headshots_raw[[i]][6],
                                                     url = duke_headshots_raw[[i]][4],
                                                     team = "Duke"))

}

#### FSU ####

fsu <- "https://seminoles.com/sports/softball/roster/season/2022-23/" %>%
  readLines()

fsu_locs <- grep("<div class=\"seminoles-bio-archive--list-item-top\"", fsu)

fsu_headshots <- data.frame()

for(i in 1:22){

  fsu_headshots <- rbind(fsu_headshots, data.frame(name = trimws((str_remove_all(fsu[fsu_locs[i]+1],"title=\"|\""))),
                                                   url = trimws(str_remove_all(fsu[fsu_locs[i]+2],"style=\"background-image: url\\(|\\)\"></div>")),
                                                   team = "Florida State"))

}



#### GT ####

gt <- "https://ramblinwreck.com/sports/w-softbl/roster" %>%
  readLines()

gt_headshots_raw <- gt[29] %>% str_split("<div class=\"thumb\" title=\"") %>% extract2(1) %>% str_split("\"")

gt_headshots <- data.frame()

for(i in 2:23){

  gt_headshots <- rbind(gt_headshots, data.frame(name = gt_headshots_raw[[i]][1],
                                                 url = gt_headshots_raw[[i]][3] %>% str_remove_all("background-image: url\\(|\\)"),
                                                 team = "Georgia Tech"))

}

#### LOUISVILLE ####

#### NC STATE ####

ncsu <- "https://gopack.com/sports/softball/roster" %>%
  readLines()

ncsu_headshots_raw <- ncsu[grep("img class=\"lazyload\" data-src=\"",ncsu)] %>% str_split("\"")

ncsu_headshots <- data.frame()

for(i in 1:21){

  ncsu_headshots <- rbind(ncsu_headshots, data.frame(name = ncsu_headshots_raw[[i]][6],
                                                     url = ncsu_headshots_raw[[i]][4],
                                                     team = "NC State"))

}

#### UNC ####

unc <- "https://goheels.com/sports/softball/roster" %>%
  readLines()

unc_headshots_raw <- unc[grep("img class=\"lazyload\" data-src=\"",unc)] %>% str_split("\"")

unc_headshots <- data.frame()

for(i in 1:21){

  unc_headshots <- rbind(unc_headshots, data.frame(name = unc_headshots_raw[[i]][6],
                                                     url = unc_headshots_raw[[i]][4],
                                                     team = "UNC"))

}

#### PITT ####
pitt <- "https://pittsburghpanthers.com/sports/softball/roster" %>%
  readLines()

pitt_headshots_raw <- pitt[grep("img class=\"lazyload\" data-src=\"",pitt)] %>% str_split("\"")

pitt_headshots <- data.frame()

for(i in 1:20){

  pitt_headshots <- rbind(pitt_headshots, data.frame(name = pitt_headshots_raw[[i]][6],
                                                     url = pitt_headshots_raw[[i]][4],
                                                     team = "Pittsburgh"))

}

pitt_headshots <- pitt_headshots %>%
  separate(name, c("last", "first"), sep = " |_") %>%
  mutate(name = str_remove(paste0(first, " ", last),",|\\.")) %>%
  select(name, url, team)


#### SYRACUSE ####

syracuse <- "https://cuse.com/sports/softball/roster" %>%
  readLines()

syracuse_headshots_raw <- syracuse[87] %>% str_split("><div class=\"s-person-details__thumbnail") %>% extract2(1) %>% str_split("\"")

syracuse_headshots <- data.frame()

for(i in 2:25){

  syracuse_headshots <- rbind(syracuse_headshots, data.frame(name = syracuse_headshots_raw[[i]][7] %>% str_remove(" full bio"),
                                                             url = syracuse_headshots_raw[[i]][17],
                                                             team = "Syracuse"))

}

#### UVA ####

virginia <- "https://virginiasports.com/sports/softball/roster" %>%
  readLines()

virginia_locs <- grep("<span itemprop=\"name\" class=\"sr-only\"", virginia)

virginia_headshots <- data.frame()

for(i in 1:25){

  virginia_headshots <- rbind(virginia_headshots, data.frame(name = trimws(str_remove_all(virginia[virginia_locs[i]],"<span itemprop=\"name\" class=\"sr-only\" content=\"|\"></span>")),
                                                             url = substr(trimws(str_remove_all(virginia[virginia_locs[i]+1],"<span itemprop=\"image\" class=\"sr-only\" content=\"|\"></span>")),28,500),
                                                             team = "Virginia"))

}

#### VT ####

vt <- "https://hokiesports.com/sports/softball/roster" %>%
  readLines()

vt_headshots_raw <- vt[2445] %>% str_split("\"") %>% extract2(1)

vt_locs <- grep("first_name", vt_headshots_raw)

vt_headshots <- data.frame()

for(i in 1:22){

  url_loc <- grep("filename", vt_headshots_raw)[i] + 2

  vt_headshots <- rbind(vt_headshots, data.frame(name = paste0(vt_headshots_raw[vt_locs[i]+2]," ", vt_headshots_raw[vt_locs[i]+6]),
                                                   url = paste0("https://hokiesports.com/images/2022/9/8/",vt_headshots_raw[url_loc]),
                                                   team = "Virginia Tech"))

}

#### ACCUMULATE HEADSHOTS ####

headshots <- rbind(bc_headshots, clemson_headshots, duke_headshots, fsu_headshots,
                   gt_headshots, ncsu_headshots, unc_headshots, pitt_headshots,
                   syracuse_headshots, virginia_headshots, vt_headshots)

write_csv(headshots, "~/Desktop/Projects/softball-projects/acc_headshots.csv")
