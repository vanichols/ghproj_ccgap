# Created:       nov 16 2020
# last edited:   
#
# purpose: compare obs and simulated data
#
# notes: 


rm(list = ls())
#devtools::install_github("wilkelab/ungeviz")
library(tidysawyer2) 
library(tidyverse)
library(ungeviz)


obs_ylds <- 
  ilia_yields %>% 
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rot2 = ifelse(rotation == "cs", "sc", rotation),
         datatype = "obs")

sim_ylds <- 
  saw_tidyapsim %>%
  mutate(state = "IA") %>% 
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rot2 = ifelse(rotation == "cs", "sc", rotation),
         datatype = "sim")

ylds <- 
  obs_ylds %>% 
  bind_rows(sim_ylds)

ylds %>%
#  filter(state != "IL") %>% 
  group_by(state, rot2, datatype) %>% 
  mutate(yield_mean = mean(yield_kgha, na.rm = T)) %>% 
  ggplot(aes(rot2, yield_kgha)) +
  geom_violin() +
  geom_hpline(width = 0.05, size = 0.5, color = "gray20") +
  geom_hpline(aes(rot2, yield_mean), width = 0.2, size = 2, color = "red3") +
  facet_grid(state~datatype)


