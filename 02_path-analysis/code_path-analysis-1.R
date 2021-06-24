# purpose: try to do path analysis
# date: 6/10/21
# 

library(tidysawyer2)
library(lavaan)
library(semPlot)
#library(OpenMx)
library(tidyverse)

# Organizing package information for table
mtcars
str(mtcars)

#--I want ag bio, residue, 0sc yield, gap

#--gap
gap <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap)

#--ag bio of prev yr crop
agbio <- read_csv("00-lit-explore/../01_create-features/4_dat_preds-all.csv") %>% 
  select(state, site, year, prevyrccyield_kgha) %>% 
  rename(prevccbio_kgha = prevyrccyield_kgha)

#--ag bio of prev yr crop
sc0 <- 
 ilia_yields %>% 
   filter(nrate_kgha == 0,
          rotation == "sc") %>% 
   select(site, year, yield_kgha) %>% 
   rename(sc0yield_kgha = yield_kgha)
   

#--residue
res <- 
  ilia_simsall %>% 
  filter(rotation == "cc",
         sim_type == "cal_scripts") %>% 
  group_by(site, year) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(sim_type, site, year, residue_w_tat_sowing) 

#--combine
dat <- 
  gap %>% 
  left_join(agbio) %>% 
  left_join(sc0) %>% 
  left_join(res) %>% 
  filter(!is.na(prevccbio_kgha),
         !is.na(nonngap)) %>% 
  select(-state, -sim_type) %>% 
  distinct() %>% 
  pivot_longer(3:ncol(.)) %>% 
  mutate(value = value/1000) %>% 
  pivot_wider(names_from = name, values_from = value)

model <-
'nonngap ~ residue_w_tat_sowing + sc0yield_kgha
residue_w_tat_sowing ~ prevccbio_kgha'

# model <-'
# mpg ~ hp + gear + cyl + disp + carb + am + wt
# hp ~ cyl + disp + carb
# '

fit <- cfa(model, data = dat)
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit, 'std', layout = 'circle')
