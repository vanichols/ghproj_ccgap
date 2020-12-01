# Created:       3/30/2020
# last edited:   12/1/2020 (add il data)
# 
# purpose: Create pred tibble for soil variables
#
# notes: 

rm(list = ls())

library(tidysawyer2)
library(lubridate)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
library(tidyr)

# soil data, from package ----------------------------------------------

#--these things come from ssurgo (except WT depth)
sc <- 
  ia_soilchar %>%
  bind_rows(il_soilchar) %>% 
  as_tibble() %>% 
  select(site, wtdepth_cm, bhz_maj, om_maj)

#--these from measurements
sprof <- 
  ia_soilprof %>%
  bind_rows(il_soilprof) %>% 
  as_tibble() %>% 
  ungroup()

#--old note: (not sure what it means)
#--this might be a problem, and might be why IACSR is not related to SOC%

# I eliminated iacsr bc that doesn't exist in IL

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

# I want paw in top 30 cm
spaw30 <- 
  sprof %>% 
  filter(profile_cm %in% c("0-30")) %>% 
  group_by(site) %>% 
  summarise(paw30_mm = sum(paw_mm))


# combine -----------------------------------------------------------------

soil_dat <- 
  sc %>% 
  left_join(sclay) %>% 
  left_join(ssoc) %>% 
  left_join(spaw) %>% 
  left_join(spaw30) %>% 
  rename(bhzdepth_cm = bhz_maj,
         clay_60cm_pct = clay_pct,
         soc_30cm_pct = soc_pct,
         paw_150cm_mm = paw_mm) 


# qc -------------------------------------------------------------------

#--does soc data agree with ssurgo?
soil_dat %>%
  mutate(soc_maj = om_maj/1.58) %>% 
  ggplot(aes(soc_30cm_pct, soc_maj)) + 
  geom_point()
#--kind of

soil_dat %>% 
  pivot_longer(2:ncol(.)) %>% 
  ggplot(aes(value)) + 
  geom_histogram() + 
  facet_wrap(~name, scales = "free")
  


# independence? -----------------------------------------------------------
library(corrplot)

soil_dat %>% 
  select_if(is.numeric) %>% 
  cor(., use="complete.obs") %>% 
  corrplot::corrplot.mixed(.)

#--let's just keep bhzdepth_cm
#--clay vs paw. keep paw?
 
soil_dat_sub <- 
  soil_dat %>% 
  select(-om_maj, -soc_30cm_pct, -paw_150cm_mm, -clay_60cm_pct)

soil_dat_sub %>% 
  select_if(is.numeric) %>% 
  cor(., use="complete.obs") %>% 
  corrplot::corrplot.mixed(.)



# write -------------------------------------------------------------------

soil_dat_sub %>% write_csv("01_create-features/2_dat_preds-soil.csv")
