# Created:       3/30/2020
# last edited:   5/28/2020 (adding il data, showing Sot/Isaiah probs
# 
# purpose: Create pred tibble for soil variables
#
# notes: 

rm(list = ls())

library(lubridate)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
library(tidysawyer2)

# ia ssurgo data -------------------------------------------------------------

ia_sites <- tidysawyer2::saw_siteinfo

ia <- 
  read_csv("01_create-features/raw_ssurgo-vals-ia.csv") %>% 
  as_tibble() %>% 
  left_join(ia_sites) %>% 
  mutate(state = "IA") %>%
  select(state, site_name, site, lat, lon, everything(), -drainage, -irrigation)
  

# il ssurgo data ----------------------------------------------------------

il_sites <- tidysawyer2::il_siteinfo

il <- 
  read_csv("01_create-features/raw_ssurgo-vals-il.csv") %>% 
  as_tibble() %>% 
  left_join(il_sites) %>% 
  mutate(state = "IL") %>%
  select(state, site_name, site, lat, lon, everything(), -site_abbv)


# see what is missing -----------------------------------------------------

ia %>% bind_rows(il) %>% write_csv("01_create-features/cf_ssurgo-problems.csv")
