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

#--I want ag bio, residue, ngap, 0sc yield, gap

#--gap
gap <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, ngap, nonngap)

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
   

# cc0-max

diff_cc <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == 0 | nrate_kgha == max(nrate_kgha)) %>% 
  filter(rotation == "cc") %>% 
  mutate(nrate_kgha = ifelse(nrate_kgha != 0, "max", "zero")) %>% 
  pivot_wider(names_from = nrate_kgha, values_from = yield_kgha) %>% 
  mutate(diff_cc = max - zero) %>% 
  select(state, site, year, diff_cc)

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
  left_join(diff_cc) %>% 
  filter(!is.na(prevccbio_kgha),
         !is.na(nonngap)) %>% 
  select(-state, -sim_type) %>% 
  distinct() %>% 
  pivot_longer(3:ncol(.)) %>% 
  mutate(value = value/1000) %>% 
  pivot_wider(names_from = name, values_from = value)


# example -----------------------------------------------------------------
dat

#--fake, just to test
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



# models -----------------------------------------------------------------

#--could try including a random effect for site (1|site)  (1|yearF) (this is 'allowed' w/my data)

dat

#--try w/ngap
model1 <-
  'ngap ~ prevccbio_kgha
  residue_w_tat_sowing ~ prevccbio_kgha
  nonngap ~ ngap + sc0 + residue_w_tat_sowing' 
  

fit1 <- cfa(model1, data = dat)
summary(fit1, fit.measures = TRUE, standardized=T,rsquare=T)

#--try w/diff_cc. Problem is we don't have good estimate of residue wt at sowing
model2 <-
  'diff_cc ~ prevccbio_kgha + residue_w_tat_sowing
  nonngap ~ diff_cc + residue_w_tat_sowing' 


fit2 <- cfa(model2, data = dat)
summary(fit2, fit.measures = TRUE, standardized=T,rsquare=T)




# another apporach --------------------------------------------------------

model2 <-
  'ngap ~ prevccbio_kgha
  nonngap ~ ngap + residue_w_tat_sowing' 


fit2 <- cfa(model2, data = dat)
summary(fit2, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit1, 'std', layout = 'circle')



#--what if surf residue also affects ngap? is this redundant?
model2 <-
  'ngap ~ prevccbio_kgha + residue_w_tat_sowing
  residue_w_tat_sowing ~ prevccbio_kgha
  nonngap ~ ngap + residue_w_tat_sowing' 


fit2 <- cfa(model2, data = dat)
summary(fit2, fit.measures = TRUE, standardized=T,rsquare=T)
semPaths(fit2, 'std', layout = 'circle')
