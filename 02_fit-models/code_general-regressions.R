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

datraw <- read_csv("01_create-features/cf_preds-all.csv")



mdat <- datraw %>% 
  dplyr::select(-crop, -site, -site_name, -year, -yearF, -iacsr) %>% 
  filter(years_in_corn > 1)


# simple regression -------------------------------------------------------
#library(MASS) #--messes up select, has stepAIC but don't know diff btwn that and step
#--no interactions

#--with %cgap as response
m1 <- lm(cgap_max_pct ~ ., 
         data = mdat %>% select(-cgap_max))
sm1 <- step(m1, ,
            k = log(nrow(pctgap))) #--gives BIC instead of AIC, this seriously limits what gets in

m1vars <- sm1$coefficients %>% enframe() %>% filter(name != "(Intercept)")

summary(sm1)
anova(sm1)

#--with rawgap as response
m2 <- lm(cgap_max ~ ., data = rawgap %>% dplyr::select(-yearF))
sm2 <- step(m2)
summary(sm2)

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
          data = pctgap %>% select_at(vars(m1vars$name, cgap_max_pct)))
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
