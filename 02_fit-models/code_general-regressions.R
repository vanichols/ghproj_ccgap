# Created:       6/4/2020
# 
# purpose: stats on pct gap and raw gap
#
# notes: 
#
# last edited:   
#

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

datraw <- read_csv("01_create-features/cf_preds-all.csv") %>% 
  filter(years_in_corn > 1)


datraw %>% 
  filter(year > 2005, year < 2016) %>% 
  ggplot(aes(reorder(year, cgap_max, mean), cgap_max)) + 
  geom_point(aes(color = site, size = p2wk_precip_mm_tot)) + 
  #facet_grid(.~drainage) +
  coord_flip() 


datraw %>% 
  filter(year > 2005, year < 2016) %>% 
  ggplot(aes(reorder(year, cgap_max, mean), cgap_max)) + 
  geom_point(aes(color = site, size = wyprecip_mm)) + 
  #facet_grid(.~drainage) +
  coord_flip() 

#--correct yield?
datraw %>% 
#  filter(year > 2005, year < 2016) %>% 
  mutate(yield_corrected = cgap_max - 0.605*years_in_corn) %>% 
  ggplot(aes(reorder(year, yield_corrected, mean), yield_corrected)) + 
  geom_jitter(aes(color = site, size = p2wk_precip_mm_tot)) + 
  coord_flip()

  

mdat <- datraw %>% 
  dplyr::select(-crop, -site_name, -year, -yearF, -iacsr) 

# simple regression -------------------------------------------------------
#library(MASS) #--messes up select, has stepAIC but don't know diff btwn that and step
#--no interactions

#--with %cgap as response
# m1 <- lm(cgap_max_pct ~ ., 
#          data = mdat %>% select(-cgap_max, -soc_30cm_pct))
# sm1 <- step(m1, ,
#             k = log(nrow(mdat))) #--gives BIC instead of AIC, this seriously limits what gets in
# 
# m1vars <- sm1$coefficients %>% enframe() %>% filter(name != "(Intercept)")
# 
# summary(sm1)
# anova(sm1)

#--with rawgap as response
m2 <- lm(cgap_max ~ ., 
         data = mdat %>% dplyr::select(-cgap_max_pct))
         
sm2 <- step(m2, k = log(nrow(mdat)))
summary(sm2)

m2vars <- sm2$coefficients %>% enframe()

#resid(sm2)
library(broom)
library(ggResidpanel)
resid_panel(sm2)

#--what about weighting??

#--let's try with interactions, only including things from m1a

#--on percentage
m1a <- lm(cgap_max_pct ~ .*., 
         data = pctgap %>% select_at(vars(m1vars$name, cgap_max_pct)))
sm1a <- step(m1a)
sm1a
summary(sm1a) %>% 
  tidy() %>% 
  arrange(p.value)


#--on raw diff
m1a <- lm(cgap_max ~ .*., 
          data = mdat %>% select(-cgap_max_pct))
# 
# m1a <- lm(cgap_max ~ .*., 
#           data = pctgap %>% select_at(vars(m1vars$name, cgap_max_pct)))
sm1a <- step(m1a)
sm1a
summary(sm1a) %>% 
  tidy() %>% 
  arrange(p.value)


# extract parms
m1avars <- 
  sm1a$coefficients %>% 
  enframe() %>% 
  filter(name != "(Intercept)")

pctgap %>% 
  ggplot(aes(wintcolddays_n, cgap_max_pct)) + 
  geom_point()
