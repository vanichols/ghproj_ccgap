# Created:      5/13/2020
#
# purpose: see what apsim is already doing . It seems nothing.
#
# author: gina vnichols@iastate.edu
#
# notes: keep apsim sims in box, all r code in github
#
# last edited:   
#
#######################

rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--has some functions
library(tidysawyer2) #--has sawyer data
library(tidyverse)
library(janitor)


# read in .out files ------------------------------------------------------

my_dir <- "../../../Box/Gina_APSIM_modeling/sims-what-is-it-already-doing/"

apraw <-
  saf_readapout(my_dir) %>%
  select(-path, -date, -(heat_days_in_season:cold_sep)) %>% 
  filter(!grepl("weather", file)) %>% 
  separate(file, into = c("rot", "Nrate", "apsim_oat")) %>%
  # remove years during spin up
  filter(year > 2000, #--exp data starts in 1999, 2000 has weird values
         year < 2017) #--exp data stops in 2016 


# keep base files separate ------------------------------------------------

ap_cc <- 
  apraw %>% 
  filter(rot == "CC")

ap_rot <- 
  apraw %>% 
  filter(rot != "CC") %>% 
  filter(soy_buac == 0) %>% 
  mutate(rot = "rotated")

ap_dat <- 
  ap_cc %>% 
  bind_rows(ap_rot) %>% 
  select_at(vars(-contains("soy")))



ap_dat %>% 
  pivot_longer(crop_yield:nhi_crop) %>% 
  ggplot(aes(value)) + 
  geom_density(aes(fill = rot), alpha = 0.5) +
  labs(title = "ames, 2001-2016") + 
  facet_wrap(~name, scales = "free")

ggsave("00_sims-explore/fig_all-vars.png")
ggsave("../../../Box/Gina_APSIM_modeling/sims-what-is-it-already-doing/diag-figs/all-vars.png")


# write them individually -------------------------------------------------


plots <- 
  ap_dat %>% 
  pivot_longer(crop_yield:nhi_crop) %>% 
  split(.$name) %>% 
  map(~ggplot(., aes(value)) + 
        geom_density(aes(fill = rot), alpha = 0.5) +
        labs(title = "ames, 2001-2016") + 
        facet_wrap(~name, scales = "free")
  )

paths <- stringr::str_c("fig_", names(plots), ".png")

pwalk(list(paths, plots), ggsave, path = "00_sims-explore/")

pwalk(list(paths, plots), ggsave, path = "../../../Box/Gina_APSIM_modeling/sims-what-is-it-already-doing/diag-figs/")

