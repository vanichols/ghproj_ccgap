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

pctgap <- datraw %>% 
  dplyr::select(-crop, -site, -site_name, -year, -cgap_max, -iacsr) %>% 
  filter(years_in_corn > 1)

rawgap <- datraw %>% 
  dplyr::select(-crop, -site, -site_name, -year, -cgap_max_pct, -iacsr) %>% 
  filter(years_in_corn > 1)


# simple regression -------------------------------------------------------
#library(MASS) #--messes up select, has stepAIC but don't know diff btwn that and step
#--no interactions

#--with %cgap as response
m1 <- lm(cgap_max_pct ~ ., data = pctgap)
sm1 <- step(m1) #--gives me an error about number of rows? there aren't any NAs...
summary(sm1)

#--with rawgap as response
m2 <- lm(cgap_max ~ ., data = rawgap %>% dplyr::select(-yearF))
sm2 <- stepAIC(m1) #--gives me an error about number of rows? there aren't any NAs...
summary(sm1)


# no interactions
m1 <- lm(cgap_max_pct ~ ., data = pctgap %>% dplyr::select(-yearF))
sm1 <- stepAIC(m1) #--gives me an error about number of rows? there aren't any NAs...
summary(sm1)

m2 <- lm(cgap_max_pct ~ .*., data = pctgap %>% select(-yearF))
step(m2)
