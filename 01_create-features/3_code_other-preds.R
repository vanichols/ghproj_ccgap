# Created:       4/1/2020
# 
# purpose: Create pred summary (soil and wea)
#
# notes: 
# last edited:  4/20/2020 cleaning up code, separating data things from stats things
#               4/30/2020 trying new folder structure
#               6/4/2020 add drainage
#               12/1/2020 add IL
#               7/7/2021 fixing yields to be aonr-max yields and their diff

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



# yields est via aonr -----------------------------------------------------

aonr_preds <- read_csv("00_empirical-n-cont/dat_preds.csv")
aonr <- read_csv("00_empirical-n-cont/dat_aonrs.csv")


aonr_gaps <- 
  aonr %>% 
  separate(aonr_rot, into = c("aonr", "rotation")) %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  left_join(aonr_preds) %>% 
  select(-nrate_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  filter(!is.na(gap_kgha)) %>% 
  mutate(gap_kgha = ifelse(gap_kgha < 0, 0, gap_kgha)) %>% 
  mutate(gap_pct = gap_kgha/sc)


# prev years cc yield, indicative of res amt ------------------------------


prev_yield <- 
  aonr_gaps %>% 
  mutate(prevyrccyield_kgha = lag(cc)) %>% 
  select(site, year, prevyrccyield_kgha)

#--use 'site production index' of cs at max n rate
avg_yieldsc <- 
  aonr_gaps%>% 
  group_by(site) %>% 
  summarise(avescyield_kgha = mean(sc, na.rm = T)) %>% 
  select(site, avescyield_kgha)

avg_yieldcc <- 
  aonr_gaps%>% 
  group_by(site) %>% 
  summarise(aveccyield_kgha = mean(cc, na.rm = T)) %>% 
  select(site, aveccyield_kgha)


avg_yield <- 
  aonr_gaps %>% 
  group_by(site) %>% 
  summarise(a_sc_kgha = mean(sc, na.rm = T),
            a_cc_kgha = mean(cc, na.rm = T)) %>% 
  mutate(aveyield_kgha = (a_sc_kgha + a_cc_kgha)/2) %>% 
  select(site, aveyield_kgha)

#--is one better than the other? are they different?
avg_yield %>% 
  left_join(avg_yieldcc) %>% 
  left_join(avg_yieldsc) %>%
  pivot_longer(2:ncol(.)) %>% 
  ggplot(aes(name, value)) + 
  geom_point(aes(color = name))

#--They represent different things. Is it even meaningful? No


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
drainage <- 
  ia_siteinfo %>% 
  bind_rows(il_siteinfo) %>% 
  select(site, drainage)




# put it all together -----------------------------------------------------

# independence?

dat <- 
  aonr_gaps %>%  
  #filter(pen_kgha > -1500) %>% #--that one lewis point, just seems weird
  rename(cc_kgha = cc,
         sc_kgha = sc) %>% 
  left_join(drainage) %>%
  left_join(prev_yield) %>% 
  left_join(avg_yield) %>% 
  left_join(yrs_corn) %>% 
  filter(!is.na(prevyrccyield_kgha)) %>% 
  left_join(ilia_siteinfo %>% select(site, state)) %>% 
  mutate(gap_pct = gap_pct*100)

dat

#--I'm fine?
dat %>% 
  ungroup() %>% 
  select(prevyrccyield_kgha, aveyield_kgha, yearsincorn) %>% 
  cor(., use="complete.obs")


# save data ---------------------------------------------------------------

dat %>% 
  write_csv("01_create-features/3_dat_preds-other.csv")


