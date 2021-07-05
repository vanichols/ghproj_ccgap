# goal: combine results into something useful
# created: 12/8/2020
# updated: 
#
# notes: 

rm(list = ls())

library(tidyverse)
library(tidysawyer2)
library(saapsim)


#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())



# set descriprtions for merging -------------------------------------------

setdesc <- read_csv("../01_create-parameter-grids/01_set-params-round2.csv")


# helper function ---------------------------------------------------------

fun_PreProcDat2 <- function(my.path, my.setdesc = setdesc){
  
  fun.df <- read_csv(my.path)
  
  fun.df2 <- 
    fun.df %>% 
    filter(soy_buac == 0) %>%
    separate(outfile, into = c("site", "rot_trt")) %>%
    mutate(
      rot = tolower(rot_trt),
      rot2 = ifelse(rot == "cs", "sc", rot)) %>% 
    left_join(my.setdesc %>% select(set_id:set_pcat) %>% distinct()) %>% 
    select(set_id, set_script, set_pcat, set_pdes, site, rot, rot2, everything(), -rot_trt)
  
  return(fun.df2)
}

# read in sims ------------------------------------------------------------

#--test
amestill <- fun_PreProcDat2(my.path = "../00_apsim-data/dat_ames-noscripts.csv",
                            my.setdesc = setdesc)

#--loop it
sim.dir <- "../00_apsim-data/"
xfiles <-list.files(sim.dir)


# ames --------------------------------------------------------------------

amesfiles <- xfiles[grepl("ames", xfiles)]
  
for (i in 1:length(amesfiles)) {

  tmp.file <- amesfiles[i]
  
  if (i == 1) {
    d1 <- fun_PreProcDat2(my.path = paste0(sim.dir, tmp.file), my.setdesc = setdesc)
  }
  
  else {
    d2 <- fun_PreProcDat2(my.path = paste0(sim.dir, tmp.file), my.setdesc = setdesc)
    d1 <- bind_rows(d1, d2)
  }
  
  i <- i + 1
}

d1 %>% 
  arrange(set_id) %>% 
  filter(year > 2000) %>% 
  write_csv("02_ames-round2.csv")

# nash --------------------------------------------------------------------

nashfiles <- xfiles[grepl("nash", xfiles)]

for (i in 1:length(nashfiles)) {
  
  tmp.file <- nashfiles[i]
  
  if (i == 1) {
    d1 <- fun_PreProcDat2(my.path = paste0(sim.dir, tmp.file), my.setdesc = setdesc)
  }
  
  else {
    d2 <- fun_PreProcDat2(my.path = paste0(sim.dir, tmp.file), my.setdesc = setdesc)
    d1 <- bind_rows(d1, d2)
  }
  
  i <- i + 1
}

d1 %>% 
  arrange(set_id) %>% 
  filter(year > 2004, year < 2017) %>% #--something funny happened in 2017, maize in all plots? 
  write_csv("02_nash-round2.csv")


# combine -----------------------------------------------------------------

#--misc data
setdesc <- read_csv("../01_create-parameter-grids/01_set-params-round2.csv")

#--sim data
ames <- 
  read_csv("02_ames-round2.csv") 

nash <- 
  read_csv("02_nash-round2.csv")

sims <- 
  bind_rows(ames, nash) %>% 
  select(-(set_script:set_pdes), -rot, -Date, -CropYield) %>% 
  select(set_id, till, site, everything())

sims %>% write_csv("02_sims.csv")

s.gaps <- 
  sims %>% 
  select(set_id, till, site, rot2, year, corn_buac) %>% 
  pivot_wider(names_from = rot2, values_from = corn_buac) %>% 
  mutate(sgap_buac = sc - cc,
         sgap_kgha = saf_buac_to_kgha_corn(sgap_buac)) 

s.gaps %>% write_csv("02_sims-gaps.csv")

#--obs data

obs <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
#  filter(site %in% c("ames", "nash")) %>% 
  mutate(rot2 = rotation) %>% 
  select(-state, -crop, -nrate_kgha, -rotation)

obs %>% write_csv("02_obs.csv")


o.gaps <- 
  obs %>% 
  pivot_wider(names_from = rot2, values_from = yield_kgha) %>% 
  mutate(ogap_kgha = sc - cc,
         ogap_buac = saf_kgha_to_buac_corn(ogap_kgha)) %>% 
  select(site, year, ogap_kgha, ogap_buac)

o.gaps %>% 
  write_csv("02_obs-gaps.csv")

