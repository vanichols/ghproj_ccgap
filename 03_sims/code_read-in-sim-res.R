# Created:      3/12/2020
#
# purpose: evaluate each factor individually and in combination using CALIBRATED APSIM sims using CC sim as base, tweaking from that
#
# author: gina vnichols@iastate.edu
#
# last edited:   3/31/2020 (I was confused...)
#                5/20/2020 separating reading in sims and looking at them (reading in takes too long)
#
# notes: keep apsim sims in box, all r code in github


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--has some functions
library(tidysawyer2) #--has sawyer data
library(tidyverse)
library(janitor)


# keeping track of oats ---------------------------------------------------

oat_key <-
  read_csv("../../../Box/Gina_APSIM_modeling/sims-ames-CCbase/data-raw/rd_oats-calibrated-v1CC.csv") %>%  
  #read_csv("sims_Mitch-cal-factor-analysis/data-raw/rd_oats-calibrated-v1.csv") %>% 
  separate_rows(category, sep = ",") %>% 
  remove_empty("rows") %>% 
  remove_empty("cols")


# read in .out files ------------------------------------------------------

my_dir <- "../../../Box/Gina_APSIM_modeling/sims-ames-CCbase/"

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
  write_csv("03_sims/se_apsim-sims-raw.csv")
