# created 3/17/2021
# gina
# purpose: assign values if I can guess them visually (a little sketchy...)
# updated:

library(saapsim)
library(tidysawyer2)
library(tidyverse)
library(scales)

theme_set(theme_bw())


# data --------------------------------------------------------------------
all <- read_csv("00_empirical-n-cont/fits-npct.csv") 

#--note, some are NAs, some are straight up missing (ones where neither quadplat converged)
dat <- 
  all %>%  
  filter(is.na(ngap_frac)) %>% 
  arrange(site, year) %>% 
  unite(site, year, col = "site_year", remove = F)


all %>% 
  unite(site, year, col = "site_year", remove = F) %>% 
  filter(site_year == "urba_2005")


dat

#--for the NA reassignment
dat_nas <- 
  dat %>%
  mutate(ngap_frac = case_when(
    grepl("craw_2007", site_year) ~ 1,
    grepl("craw_2011", site_year) ~ 0,
    grepl("deka_2005", site_year) ~ 1,
    grepl("dsup_2002", site_year) ~ 1,
    grepl("dsup_2006", site_year) ~ 1,
    grepl("kana_2014", site_year) ~ 0,
    grepl("lewi_2010", site_year) ~ 1,
    grepl("mcna_2004", site_year) ~ 1,
    grepl("mcna_2014", site_year) ~ 1,
    grepl("monm_2005", site_year) ~ 0,
    grepl("nash_2007", site_year) ~ 0,
    grepl("nash_2008", site_year) ~ 0,
    grepl("nash_2010", site_year) ~ 1,
    grepl("nash_2016", site_year) ~ 1),
    manual_edit = "NA-replace") %>% 
  filter(!is.na(ngap_frac))


#--for the missing ones
dat_miss <- 
  tibble(site = "craw",
       year = 2004,
       gap_at_rotaonr_kgha = NA,
       gap_at_contaonr_kgha = NA,
       ngap_frac = 1) %>% 
  add_row(site = "orcc",
          year = 2003,
          ngap_frac = 1) %>% 
  add_row(site = "urba",
          year = 2005, 
          ngap_frac = 0) %>% 
  mutate(manual_edit = "add") %>% 
  unite(site, year, col = "site_year", remove = F) 


#--which ones got new values? filter those out
newvals <- 
  dat_nas %>% 
  bind_rows(dat_miss) %>% 
  pull(site_year)


#--add new data
newdat <- 
  all %>% 
  unite(site, year, col = "site_year", remove = F) %>% 
  filter(!(site_year %in% newvals)) %>% 
  bind_rows(dat_nas) %>% 
  bind_rows(dat_miss)


newdat %>% 
  write_csv("00_empirical-n-cont/fits-npct-manual-adds.csv")  

