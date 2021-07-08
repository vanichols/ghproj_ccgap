# Created:       6/4/2020
# 
# purpose: stats on pct gap and raw gap
#
# notes: 
#
# last edited:   12/2/2020
#                 july 7 2021 (trying to rerun things w/anor things)
#

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

datraw <- read_csv("01_create-features/4_dat_preds-all.csv")


datraw %>% 
  ggplot(aes(yearsincorn, gap_kgha)) + 
  geom_point()+ 
  geom_smooth(method = "lm")

mdat <- 
  datraw %>% 
  unite(state, site, year, col = "site_year") %>% 
  dplyr::select(-yearF, -cc_kgha, -sc_kgha, -drainage)

# simple regression -------------------------------------------------------
library(MASS) #--messes up select, has stepAIC but don't know diff btwn that and step

#--with rawgap as response
m2 <- lm(gap_kgha ~ ., 
         data = mdat %>% dplyr::select(-site_year, -gap_pct))
         
sm2 <- step(m2, k = log(nrow(mdat)))

m2vars <- 
  summary(sm2) %>% 
  broom::tidy() %>% 
  arrange(p.value) %>% 
  mutate(respvar = "gap_kgha")

# what if I don't include yearsincorn?
m2b <- lm(gap_kgha ~ ., 
         data = mdat %>% dplyr::select(-site_year, -gap_pct, -yearsincorn))

sm2b <- step(m2b, k = log(nrow(mdat)))
summary(sm2b)

#--should I do a leave-one-predictor out analysis? probably

m2vars %>% 
  write_csv("02_fit-models/02_stepwise-selections-gap.csv")


#resid(sm2)
library(broom)
library(ggResidpanel)
resid_panel(sm2)

#--what about weighting??

#--let's try with interactions, only including things from m1a

#--on percentage
m1a <- lm(gap_pct ~ .*.,
         data = mdat %>% dplyr::select(-site_year, -gap_kgha))
sm1a <- step(m1a)
sm1a

summary(sm1a)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>%
  janitor::clean_names() %>% 
  arrange(pr_t)

#--on raw diff
m1b <- lm(gap_kgha ~ .*.,
          data = mdat %>% dplyr::select(-site_year, -gap_pct))
sm1b <- step(m1b, k = log(nrow(mdat)))
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
