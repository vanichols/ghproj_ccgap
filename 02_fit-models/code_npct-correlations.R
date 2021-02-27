# Created:       2/27/2021
# 
# purpose: relationship btwn ncpt and weather/soil?
#
# notes: 
#
# last edited:   2/27/2021
#

rm(list = ls())
library(tidyverse)
library(tidysawyer2)



# data --------------------------------------------------------------------

npct <- read_csv("00_empirical-n-cont/fits-npct.csv")

wp <- read_csv("01_create-features/1_dat_candidate-preds-wea.csv")


dat <- 
  wp %>% 
  left_join(npct %>% select(site, year, ngap_frac)) %>% 
  select(state, site, year, ngap_frac, everything()) 
  
dat_site <- 
  wp %>% 
  pivot_longer(-(state:year)) %>% 
  group_by(site, name) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  left_join(npct %>% group_by(site) %>% summarise(ngap_frac = mean(ngap_frac, na.rm = T))) %>% 
  filter(!is.na(ngap_frac)) 
  

# correlations ------------------------------------------------------------
#--test
tst <- 
  dat %>% 
  pivot_longer(wyprecip_mm:gs_precip_mm_tot) %>%
  filter(name == "wyprecip_mm") %>% 
  filter(!is.na(ngap_frac))

tst

cor(tst$ngap_frac, tst$value)

tst %>%
  nest() %>% 
  mutate(corr =data %>% map_dbl(~cor(.$ngap_frac, .$value)))


dcor <- 
  dat %>% 
  pivot_longer(wyprecip_mm:gs_precip_mm_tot) %>% 
  filter(!is.na(ngap_frac)) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(corr =data %>% map_dbl(~cor(.$ngap_frac, .$value))) %>% 
  arrange(corr) 

#--look at correlations
dcor %>% 
  ggplot(aes(reorder(name, corr), corr)) + 
  geom_point() +
  coord_flip()

#--just look at top corrs
tops <- 
  dcor %>%
  filter(abs(corr) > 0.2) %>%
  pull(name)

dat %>% 
  pivot_longer(wyprecip_mm:gs_precip_mm_tot) %>%
  filter(name %in% tops) %>% 
  ggplot(aes(ngap_frac, value)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~name, scales = "free_y")

#--warm april temps, july precip --> more of gap being closed by N
#--more winter cold days --> less gap closed by n?


# do site avgs ------------------------------------------------------------

dcor_site <- 
  dat_site %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(corr =data %>% map_dbl(~cor(.$ngap_frac, .$value))) %>% 
  arrange(corr) 

#--look at correlations
dcor_site %>% 
  ggplot(aes(reorder(name, corr), corr)) + 
  geom_point() +
  coord_flip()

#--just look at top corrs
tops_site <- 
  dcor_site %>%
  filter(abs(corr) > 0.5) %>%
  pull(name)

dat_site %>% 
  filter(name %in% tops_site) %>% 
  ggplot(aes(ngap_frac, value)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~name, scales = "free_y")

#--is higher npct assoc w smaller gaps?
#--yes, but it separates by state, too, kind of
ilia_gaps %>% 
  group_by(state, site) %>% 
  summarise(gap_kgha = mean(gap_kgha, na.rm = T)) %>% 
  left_join(
    npct %>% 
      group_by(site) %>% 
      summarise(ngap_frac = mean(ngap_frac, na.rm = T))
    ) %>% 
  ggplot(aes(gap_kgha, ngap_frac)) + 
  geom_point(aes(color = site, shape = state), size = 5)
