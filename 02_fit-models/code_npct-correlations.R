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

npct <- 
  read_csv("00_empirical-n-cont/fits-npct-manual-adds.csv") 

#--see if I use the curated batch instead
# wp <- 
#   read_csv("01_create-features/1_dat_candidate-preds-wea.csv") %>% 
#   #--add years in corn
#   group_by(site) %>% 
#   mutate(yrs_in_corn = year - min(year) + 1) %>% 
#   dplyr::select(state:year, yrs_in_corn, everything()) %>% 
#   ungroup()

wp <- read_csv("01_create-features/1_dat_preds-wea.csv") %>%
#--add years in corn
  group_by(site) %>%
  mutate(yrs_in_corn = year - min(year) + 1) %>%
  dplyr::select(state:year, yrs_in_corn, everything()) %>%
  ungroup()



#--w/o manual edits to ngap_frac
dat <- 
  wp %>% 
  left_join(npct %>% dplyr::select(site, year, ngap_frac, manual_edit)) %>% 
  filter(is.na(manual_edit)) %>% 
  dplyr::select(state, site, year, ngap_frac, yrs_in_corn, everything(), -manual_edit)  
  
dat_gap <- 
  wp %>% 
  left_join(npct %>% dplyr::select(site, year, gap_at_rotaonr_kgha)) %>% 
  dplyr::select(state, site, year, gap_at_rotaonr_kgha, yrs_in_corn, everything()) 

#--w/manual edits to ngap_frac
dat_man <- 
  wp %>% 
  left_join(npct %>% dplyr::select(site, year, ngap_frac)) %>% 
  dplyr::select(state, site, year, ngap_frac, yrs_in_corn, everything())  


#--includes manual edit values
dat_site <-
  wp %>%
  pivot_longer(-(state:year)) %>%
  group_by(site, name) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  left_join(npct %>% group_by(site) %>% summarise(ngap_frac = mean(ngap_frac, na.rm = T))) %>%
  filter(!is.na(ngap_frac))
  

# correlations ------------------------------------------------------------

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
dcor %>% 
  filter(abs(corr) > 0.2) %>%
  ggplot(aes(reorder(name, corr), corr)) + 
  geom_point(size = 3) +
  geom_segment(aes(x = name, xend = name, y = 0, yend = corr)) +
  coord_flip()

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


# simple stepwise regression -------------------------------------------------------

#library(MASS) #--messes up select, has stepAIC but don't know diff btwn that and step

#--no interactions
#--can I include site?
m1 <- lm(ngap_frac*100 ~ ., data = dat %>% dplyr::select(-state, 
                                                         #-site, 
                                                         -year))
sm1 <- step(m1, k = log(nrow(dat))) #--gives BIC instead of AIC, this seriously limits what gets in
sm1$coefficients %>%  
  enframe() %>% 
  filter(name != "(Intercept)") %>% 
  mutate(respvar = "n_pct") %>% 
  arrange(value)

#--does this change if I include the 'manual' assignments? No. 
m1a <- lm(ngap_frac*100 ~ ., data = dat_man %>% dplyr::select(-state, -site, -year))
sm1a <- step(m1a, k = log(nrow(dat_man))) #--gives BIC instead of AIC, this seriously limits what gets in
sm1a$coefficients %>%  
  enframe() %>% 
  filter(name != "(Intercept)") %>% 
  mutate(respvar = "n_pct") %>% 
  arrange(value)



#--compare this to the results for the gap itself
m2 <- lm(gap_at_rotaonr_kgha ~ ., data = dat_gap %>% dplyr::select(-state, -site, -year))
sm2 <- step(m2, k = log(nrow(dat_gap))) #--gives BIC instead of AIC, this seriously limits what gets in

sm2$coefficients %>%  
  enframe() %>% 
  filter(name != "(Intercept)") %>% 
  mutate(respvar = "gap") %>% 
  arrange(value)


# do site avgs ------------------------------------------------------------

#--not in love with this

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

