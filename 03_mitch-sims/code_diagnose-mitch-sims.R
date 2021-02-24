# purpose: explore mitch's sims
# created: 2/23/2021
# updated:


library(tidyverse)
library(tidysawyer2)
library(naniar)

ilia_sims <- 
  ilia_simsyields

ilia_simsl <- 
  ilia_sims %>% 
  pivot_longer(cal_nosc:cal_scripts, names_to = "sim_type", values_to = "yield_kgha") 
  
# viz ---------------------------------------------------------------------


ilia_simsl %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation2)) + 
  facet_grid(sim_type ~ site )

#--1991, deka had 0 yields in noscripts  
ilia_simsl %>% 
  filter(yield_kgha == 0, site == "deka")

#--but has NAs in scripts
ilia_simsl %>% 
  filter(year == 1991, site == "deka", sim_type == "cal_scripts")


#--sc rotation values shouldn't change much
ilia_sims %>% 
  ggplot(aes(cal_nosc, cal_scripts)) + 
  geom_miss_point() +
  geom_abline() +
  facet_grid(.~rotation2)

#--ummm, hmm

# 69 missing
ilia_sims %>% 
  filter(is.na(cal_nosc))

# 907 missing
ilia_sims %>% 
  filter(is.na(cal_scripts))
