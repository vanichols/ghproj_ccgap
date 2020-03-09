# Created:      3/9/2020
# last edited:   
# notes: keep apsim sims in box, all r code in github
# purpose: evaluate each factor individually and in combination using CALIBRATED APSIM sims
# author: gina vnichols@iastate.edu


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has exp data in it
library(tidyverse)
library(janitor)

# keeping track of oats ---------------------------------------------------

oat_key <-
  read_csv("../../../Box/Gina_APSIM_modeling/sims_Mitch-cal-factor-analysis/data-raw/rd_oats-calibrated-v1.csv") %>% 
  #read_csv("sims_Mitch-cal-factor-analysis/data-raw/rd_oats-calibrated-v1.csv") %>% 
  separate_rows(category, sep = ",") %>% 
  remove_empty("rows") %>% 
  remove_empty("cols")


# read in .out files ------------------------------------------------------

my_dir <- "../../../Box/Gina_APSIM_modeling/sims_Mitch-cal-factor-analysis/"

apraw <-
  saf_readapout(my_dir) %>%
  select(file, year, corn_buac) %>%
  mutate(yield_kgha = saf_buac_to_kgha_corn(corn_buac)) %>%
  separate(file, into = c("rot", "Nrate", "apsim_oat")) %>%
  # remove years during spin up
  filter(!is.na(apsim_oat), 
         year > 1998, #--exp data starts in 1999
         year < 2017) #--exp data stops in 2016 




# keep base files separate ------------------------------------------------

#--this is just for reference, isn't used in any yield gap calcs
base_contc <- 
  apraw %>%
  mutate(oat_nu = parse_number(apsim_oat)) %>%
  filter(rot == "CC", apsim_oat == "base") %>% 
  mutate(dtype = "ap_contc", 
         oat_nu = 0) %>% 
  select(dtype, oat_nu, year, yield_kgha)

base_rotc <- 
  apraw %>%
  mutate(oat_nu = parse_number(apsim_oat)) %>%
  filter(rot != "CC", apsim_oat == "base") %>% 
  filter(yield_kgha != 0) %>% #--remove soybean years
  mutate(dtype = "ap_rotc",
         oat_nu = 0) %>% 
  select(dtype, oat_nu, year, yield_kgha)


# working apd ------------------------------------------------------------------

apw <- 
  apraw %>% 
  filter(!grepl("base", apsim_oat), 
         yield_kgha != 0) %>% 
  mutate(oat_nu = parse_number(apsim_oat),
         dtype = "ap_rotcoat") %>% 
  select(dtype, oat_nu, year, yield_kgha) %>% 
  arrange(oat_nu, year)

# calc yield gaps ---------------------------------------------------------

ewgap <-
  sad_tidysawyer %>% 
  filter(site == "ames", nrate_kgha == max(nrate_kgha)) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(gap_kgha = sc - cc,
         dtype = "exp_gap", 
         oat_nu = 0) %>% 
  filter(!is.na(gap_kgha)) %>% 
  select(dtype, oat_nu, year, gap_kgha)

agap <- 
  apw %>% 
  left_join(select(base_rotc, - oat_nu), by = "year") %>% 
  mutate(gap_kgha = yield_kgha.y - yield_kgha.x,
         dtype = "oat_gap") %>% 
  select(dtype, oat_nu, year, gap_kgha)


gaps <- 
  ewgap %>% 
  bind_rows(agap) %>% 
  left_join(oat_key)



# look at gaps ------------------------------------------------------------

gaps %>% 
  ggplot(aes(oat_what, gap_kgha)) + 
  geom_point() + 
  coord_flip()


# get exp data in same form -----------------------------------------------

ew <- 
  sad_tidysawyer %>% 
  filter(site == "ames", nrate_kgha == max(nrate_kgha)) %>% 
  mutate(dtype = ifelse(rotation == "cc", "exp_contc", "exp_rotc"),
         oat_nu = 0) %>% 
  select(dtype, oat_nu, year, yield_kgha) 
  


# combine and look at exp vs apsim ----------------------------------------

apw %>% 
  bind_rows(ew) %>% 
  bind_rows(base_rotc) %>% 
  bind_rows(base_contc) %>% 
  filter(!grepl("oat", dtype)) %>% 
  separate(dtype, into = c("source", "rot_type")) %>% 
  ggplot(aes(source, yield_kgha)) +
  geom_point(aes(color = as.factor(year), shape = as.factor(oat_nu))) + 
  facet_grid(.~ rot_type)

#---it looks fine


  