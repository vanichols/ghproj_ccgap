# Created:       6/4/2020
# last edited:   
#
# purpose: understand how to take standard deviations to std dev of difference
#
# notes: 

rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2")
library(tidysawyer2) #--saw_xx data
library(readr)

#--ex for qinglong
exdat <- 
  saw_tidysawyer %>% 
  filter(site == "ames", nrate_kgha > 250, year == 2001) %>% 
  select(site, year, rotation, yield_kgha, sd_kgha, nreps) 


exdat
