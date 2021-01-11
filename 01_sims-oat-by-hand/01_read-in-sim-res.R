# Created:      3/12/2020
#
# purpose: evaluate each factor individually and in combination using CALIBRATED APSIM sims using CC sim as base, tweaking from that
#
# author: gina vnichols@iastate.edu
#
# last edited:   3/31/2020 (I was confused...)
#                5/20/2020 separating reading in sims and looking at them (reading in takes too long)
#                11/23/2020 created oat22 for rfv50%, updated file structure
#                 1/11/2021 Revisted. THe outputs are Mitch's, they are a mess, but I don't want to break anything. 
#
# notes: keep apsim sims in box, all r code in github


rm(list = ls())
library(saapsim) #--has some functions
library(tidyverse)
library(janitor)


# read in .out files ------------------------------------------------------

my_dir <- "01_sims-oat-by-hand/sims-ames-CCbase/"

apraw <-
  saf_readapout(my_dir) %>% 
  separate(file, into = c("rot", "Nrate", "apsim_oat")) %>%
  # remove years during spin up
  filter(!is.na(apsim_oat), 
         year > 2000, #--exp data starts in 1999, first year is 'bad', valid for ames only
         year < 2017) #--exp data stops in 2016 

#--there are some files that should be ignored?
apraw %>% filter(!is.na(apsim_oat))

apraw %>% 
  write_csv("01_sims-oat-by-hand/sims-ames-CCbase/dat-ames-CCbase-raw.csv")
