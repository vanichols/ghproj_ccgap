# Created:     1/11/2021
#
# purpose: read in sims that had no crop growing
# last edited:   
# notes: didn't run anything, just moved from Box


rm(list = ls())
library(saapsim) #--has some functions
library(tidyverse)
library(janitor)


# read in .out files ------------------------------------------------------

my_dir <- "01_sims-oat-by-hand/sims-no-crop/sim-nocrops/"

apraw <-
  saf_readapout(my_dir) 

apmin <- 
  apraw %>% 
  select(path, year, doy, dlt_n_min) %>% 
  separate(path, into = c("x1", "x2", "x3", "x4"), sep = "/") %>% 
  separate(x4, into = c("site", "x5"), sep = "-") %>% 
  select(!starts_with("x")) 

apmin_annual <- 
  apmin %>% 
  group_by(site, year) %>% 
  summarise(n_min_annual = sum(dlt_n_min, na.rm = T))

apmin_grow <- 
  apmin %>% 
  left_join(ia_planting) %>% 
  filter(doy > plant_doy) %>% 
  filter(doy < saf_date_to_doy("2001-09-01")) %>% 
  group_by(site, year) %>% 
  summarise(n_min_grow = sum(dlt_n_min, na.rm = T))

apmin_annual %>% 
  left_join(apmin_grow) %>% 
  write_csv("01_sims-oat-by-hand/sims-no-crop/dat_no-crops.csv")
