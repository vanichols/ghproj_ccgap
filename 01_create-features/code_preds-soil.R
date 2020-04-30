# Created:       3/30/2020
# last edited:   
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

# soil data, from package ----------------------------------------------

#--these things come from ssurgo
sc <- 
  saw_soilchar %>% #--from tidysawyer2 pkg
  as_tibble() %>% 
  select(-cropprodindex_maj, -cropprodindex_wgt, -om_wgt)

#--these from measurements
sprof <- 
  saw_soilprof %>% 
  as_tibble()

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
         om_maj,
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
  mutate(soc_maj = om_maj/1.58) %>% 
  ggplot(aes(soc_30cm_pct, soc_maj)) + 
  geom_point()

soil_dat %>%
  mutate(soc_maj = om_maj/1.58) %>% 
  ggplot(aes(iacsr, soc_maj)) + 
  geom_point(size = 3) + 
  labs(x = "Iowa Corn Suitability Rating",
       y = "% SOC",
       title = "Why isn't ICSR related to SOC %")

ggsave("01_create-features/fig_soil-iacsr-soc.png")

# write -------------------------------------------------------------------

soil_dat %>% write_csv("01_create-features/cf_pred-soil.csv")
