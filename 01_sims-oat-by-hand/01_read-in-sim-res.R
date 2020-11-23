# Created:      3/12/2020
#
# purpose: evaluate each factor individually and in combination using CALIBRATED APSIM sims using CC sim as base, tweaking from that
#
# author: gina vnichols@iastate.edu
#
# last edited:   3/31/2020 (I was confused...)
#                5/20/2020 separating reading in sims and looking at them (reading in takes too long)
#                11/23/2020 created oat22 for rfv50%, updated file structure
#
# notes: keep apsim sims in box, all r code in github


rm(list = ls())
library(saapsim) #--has some functions
library(tidyverse)
library(janitor)


# read in .out files ------------------------------------------------------

my_dir <- "../../../Box/Gina_APSIM_modeling/sims-explore-by-hand/sims-ames-CCbase/"

apraw <-
  saf_readapout(my_dir) %>%
  select(file, year, corn_buac) %>%
  mutate(yield_kgha = saf_buac_to_kgha_corn(corn_buac)) %>%
  separate(file, into = c("rot", "Nrate", "apsim_oat")) %>%
  # remove years during spin up
  filter(!is.na(apsim_oat), 
         year > 1998, #--exp data starts in 1999
         year < 2017) #--exp data stops in 2016 


apraw %>% 
  write_csv("01_sims-oat-by-hand/sims_apsim-hand-oat-raw.csv")
