# Created:      1/14/2021
# last edited:   
#
# purpose: 
#
# notes: 


rm(list = ls())
#devtools::install_github("wilkelab/ungeviz")
library(tidysawyer2) 
library(tidyverse)
library(ungeviz)

ia_yields_se


ylds <- 
  ia_yields_se %>% 
  select(site, year, rotation, nrate_kgha, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  filter(!is.na(sc))


ses <- 
  ia_yields_se %>% 
  select(site, year, rotation, nrate_kgha, sd_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = sd_kgha) %>% 
  rename("cc_sd" = cc,
         "sc_sd" = sc) %>% 
  filter(!is.na(sc_sd))

ylds %>% 
  left_join(ses) %>% 
  mutate(term1 = (cc_sd)^2/4,
         term2 = (sc_sd)^2/4,
         sd = sqrt(term1 + term2))
