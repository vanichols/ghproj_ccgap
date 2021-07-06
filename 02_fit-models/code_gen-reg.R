# Created:       6/4/2020
# 
# purpose: stats on pct gap and raw gap
#
# notes: 
#
# last edited:   12/2/2020
#

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

datraw <- read_csv("01_create-features/4_dat_preds-all.csv")


mdat <- 
  datraw %>% 
  unite(state, site, year, col = "site_year") %>% 
  dplyr::select(-crop, -yearF, -nrate_kgha, -cc_kgha, -sc_kgha, -drainage)

# simple regression -------------------------------------------------------
library(MASS) #--messes up select, has stepAIC but don't know diff btwn that and step
#--no interactions

#--with pen% as response
 m1 <- lm(pen_pct ~ .,
          data = mdat %>% dplyr::select(-site_year, -pen_kgha))

sm1 <- step(m1, ,
             k = log(nrow(mdat))) #--gives BIC instead of AIC, this seriously limits what gets in

m1vars <- 
  sm1$coefficients %>% 
  enframe() %>% 
  filter(name != "(Intercept)") %>% 
  mutate(respvar = "pen_pct") %>% 
  arrange(value)

summary(sm1)
anova(sm1)

#--with rawgap as response
m2 <- lm(pen_kgha ~ ., 
         data = mdat %>% dplyr::select(-site_year, -pen_pct))
         
sm2 <- step(m2, k = log(nrow(mdat)))
summary(sm2)
anova(sm2)

m2vars <- 
  sm2$coefficients %>% 
  enframe() %>% 
  filter(name != "(Intercept)") %>% 
  mutate(respvar = "pen_kgha") %>% 
  arrange(value)

m1vars %>% 
  bind_rows(m2vars) %>% 
  write_csv("02_fit-models/02_stepwise-selections.csv")


#resid(sm2)
library(broom)
library(ggResidpanel)
resid_panel(sm2)

#--what about weighting??

#--let's try with interactions, only including things from m1a

#--on percentage
m1a <- lm(pen_pct ~ .*.,
         data = mdat %>% dplyr::select(-site_year, -pen_kgha))
sm1a <- step(m1a)
sm1a

summary(sm1a)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>%
  janitor::clean_names() %>% 
  arrange(pr_t)

#--on raw diff
m1b <- lm(pen_kgha ~ .*.,
          data = mdat %>% dplyr::select(-site_year, -pen_pct))
sm1b <- step(m1b)
sm1b

summary(sm1b)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>%
  janitor::clean_names() %>% 
  arrange(pr_t)


# extract parms
m1avars <- 
  sm1a$coefficients %>% 
  enframe() %>% 
  filter(name != "(Intercept)")

pctgap %>% 
  ggplot(aes(wintcolddays_n, cgap_max_pct)) + 
  geom_point()
