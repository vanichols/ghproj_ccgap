# Created:       4/1/2020
# 
# purpose: Create pred summary (soil and wea)
#
# notes: 
# last edited:  4/20/2020 cleaning up code, separating data things from stats things
#               4/30/2020 trying new folder structure
#               6/4/2020 add drainage
#               12/1/2020 add IL


#--make sure data is up-to-date, if you want
#source("01_create-features/1_code_preds-wea.R")
#source("01_create-features/2_code_preds-soil.R")

rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2", force = T)
library(tidysawyer2) #--has wea data
library(lubridate)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
library(tidyr)


# what else should we add? ------------------------------------------------

#--add previous year's continuous corn yield at max nrate (indicative of residue amt)
prev_yield <- 
  ilia_yields %>%
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "cc") %>% 
  mutate(prevyrccyield_kgha = lag(yield_kgha)) %>% 
  select(site, year, prevyrccyield_kgha)

#--use 'site production index' of cs at max n rate
avg_yieldsc <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "sc") %>% 
  summarise(avescyield_kgha = mean(yield_kgha, na.rm = T)) %>% 
  select(site, avescyield_kgha)

avg_yieldcc <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  #--make sure it's taken over the same time frame?
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "cc") %>% 
  summarise(aveccyield_kgha = mean(yield_kgha, na.rm = T)) %>% 
  select(site, aveccyield_kgha)


avg_yield <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  #--make sure it's taken over the same time frame
  select(site, year, rotation, yield_kgha) %>%
  summarise(aveyield_kgha = mean(yield_kgha, na.rm = T)) %>% 
  select(site, aveyield_kgha)

#--is one better than the other? are they different?
avg_yield %>% 
  left_join(aveccyield_kgha) %>% 
  left_join(avescyield_kgha) %>%
  pivot_longer(3:4) %>% 
  ggplot(aes(aveyield_kgha, value)) + 
  geom_point(aes(color = name))

#--I think the overall average is the best


#--include # of years in corn

yrs_corn <- 
  ilia_yields %>% 
  select(-state) %>% 
  filter(rotation == "cc") %>% 
  group_by(site) %>% 
  group_modify(~{
    .x %>% mutate(yearsincorn = group_indices(., year))
  }) %>% 
  select(site, year, yearsincorn) %>% 
  distinct()
  
#--tiled or not?
#--don't have info on IL right now
drainage <- 
  ia_siteinfo %>% 
  select(site_name, site, drainage)

il_siteinfo


# put it all together -----------------------------------------------------

# independence?

dat <-  
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(pen_kgha = sc - cc,
         pen_pct = pen_kgha/sc * 100) %>% 
  filter(pen_kgha > -1500) %>% #--that one lewis point, just seems weird
  rename(cc_kgha = cc,
         sc_kgha = sc) %>% 
  left_join(prev_yield) %>% 
  left_join(avg_yield) %>% 
  left_join(yrs_corn) %>% 
  filter(!is.na(prevyrccyield_kgha))

#--I'm fine?
dat %>% 
  ungroup() %>% 
  select(prevyrccyield_kgha, aveyield_kgha, yearsincorn) %>% 
  cor(., use="complete.obs")



#--does pct gap dec over years? yes.
dat %>% 
  ggplot(aes(yearF, pen_pct)) + 
  geom_point() + 
  facet_grid(.~state, scales = "free_x")

# save data ---------------------------------------------------------------

dat %>% 
  write_csv("01_create-features/3_dat_preds-other.csv")


