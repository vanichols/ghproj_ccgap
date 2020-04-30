# Created:       3/30/2020
# last edited:   
# 
# purpose: Create pred tibble for soil variables
#
# notes: 

rm(list = ls())

#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has 'sad_xxx' data
library(lubridate)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)

# soil data, from package ----------------------------------------------

#--these things come from ssurgo
sc <- 
  sad_soilchar %>% #--from saapsim pkg
  as_tibble() %>% 
  select(-cropprodindex_maj, -cropprodindex_wgt)

#--these from measurements
sprof <- sad_soilprof %>% as_tibble()

#--this might be a problem, and might be why IACSR is not related to SOC%

# wrangle -----------------------------------------------------------------

# sot wants clay 0-60cm
sclay <- 
  sprof %>% 
  filter(profile_cm %in% c("0-30", "30-60")) %>% 
  group_by(site) %>% 
  summarise(clay_pct = mean(clay_pct))


# sot wants soc 0-30
ssoc <- 
  sprof %>% 
  filter(profile_cm %in% c("0-30", "30-60")) %>% 
  group_by(site) %>% 
  summarise(soc_pct = mean(soc_pct))

# sot wants 1.5 m paw sum
spaw <- 
  sprof %>% 
  filter(!profile_cm %in% c("150-180")) %>% 
  group_by(site) %>% 
  summarise(paw_mm = sum(paw_mm))


# combine -----------------------------------------------------------------

soil_dat <- 
  sc %>% 
  left_join(sclay) %>% 
  left_join(ssoc) %>% 
  left_join(spaw) %>% 
  select(site, wtdepth_cm,
         iacsr_wgt,
         bhz_wt,
         clay_pct,
         soc_pct,
         paw_mm) %>% 
  rename(iacsr = iacsr_wgt,
         bhzdepth_cm = bhz_wt,
         clay_60cm_pct = clay_pct,
         soc_30cm_pct = soc_pct,
         paw_150cm_mm = paw_mm) %>% 
  mutate(wtdepth_cm = as.numeric(wtdepth_cm))


soil_dat %>% 
  ggplot(aes(soc_30cm_pct)) + 
  geom_histogram()

# write -------------------------------------------------------------------

soil_dat %>% write_csv("01_create-features/cf_pred-soil.csv")
