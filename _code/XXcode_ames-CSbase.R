# Created:      3/31/2020
# notes: keep apsim sims in box, all r code in github
# purpose: evaluate each factor individually and in combination using CALIBRATED APSIM sims
# author: gina vnichols@iastate.edu
#
# notes: just using ames soil/weather
# last edited:   3/31/2020
# notes: shit. we decided this isn't the way to go---tweak cc instead. Go to ames-CCbase

rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
#library(saapsim) #--my package, has soils data and some functions, not needed?
library(tidysawyer2) #--has sawyer data
library(tidyverse)
library(janitor)


# keeping track of oats ---------------------------------------------------

oat_key <-
  read_csv("../../../Box/Gina_APSIM_modeling/sims_ames-CSbase/data-raw/rd_oats-calibrated-v1.csv") %>% 
  #read_csv("sims_Mitch-cal-factor-analysis/data-raw/rd_oats-calibrated-v1.csv") %>% 
  separate_rows(category, sep = ",") %>% 
  remove_empty("rows") %>% 
  remove_empty("cols")


# read in .out files ------------------------------------------------------

# don't need to do unless things were updated

# my_dir <- "../../../Box/Gina_APSIM_modeling/sims_ames-CSbase/"
# 
# apraw <-
#   saf_readapout(my_dir) %>%
#   select(file, year, corn_buac) %>%
#   mutate(yield_kgha = saf_buac_to_kgha_corn(corn_buac)) %>%
#   separate(file, into = c("rot", "Nrate", "apsim_oat")) %>%
#   # remove years during spin up
#   filter(!is.na(apsim_oat), 
#          year > 1998, #--exp data starts in 1999
#          year < 2017) #--exp data stops in 2016 
# 
#write_rds(apraw, "_data/sims_rds/rds_ames-csbase.rds")

apraw <- read_rds("_data/sims_rds/rds_ames-csbase.rds")


# keep base files separate ------------------------------------------------

#--un-altered apsim cc sims
base_contc <- 
  apraw %>%
  mutate(oat_nu = parse_number(apsim_oat)) %>%
  filter(rot == "CC", apsim_oat == "base") %>% 
  mutate(dtype = "ap_contc", 
         oat_nu = 0) %>% 
  select(dtype, oat_nu, year, yield_kgha)


#--un-altered apsim cs/sc sims
base_rotc <- 
  apraw %>%
  mutate(oat_nu = parse_number(apsim_oat)) %>%
  filter(rot != "CC", apsim_oat == "base") %>% 
  filter(yield_kgha != 0) %>% #--remove soybean years
  mutate(dtype = "ap_rotc",
         oat_nu = 0) %>% 
  select(dtype, oat_nu, year, yield_kgha)

# oat apd ------------------------------------------------------------------

apoat <- 
  apraw %>% 
  filter(!grepl("base", apsim_oat), #--remove the base sims
         yield_kgha != 0) %>% 
  mutate(oat_nu = parse_number(apsim_oat),
         dtype = "ap_rotcoat") %>% 
  select(dtype, oat_nu, year, yield_kgha) %>% 
  arrange(oat_nu, year)

# calc yield gaps ---------------------------------------------------------

#--current apsim cs - cc
apcurr_gap <- 
  base_contc %>% 
  rename(cc_yield_kgha = yield_kgha) %>% 
  select(-dtype) %>% 
  left_join(base_rotc) %>% 
  mutate(gap_kgha = yield_kgha - cc_yield_kgha,
         dtype = "ap_currentgap") %>% 
  select(dtype, oat_nu, year, gap_kgha)


#--experimental yield gap (cs - cc) in ames
ewgap <-
  saw_cgap %>% #--tidysawyer2 data
  ungroup() %>% 
  filter(site == "ames") %>% 
  mutate(gap_kgha = cgap_max, #--just renaming
         dtype = "exp_gap", 
         oat_nu = 0) %>% 
  filter(!is.na(gap_kgha)) %>% 
  select(dtype, oat_nu, year, gap_kgha)


#--tweaked CS minus base CC
aptweakgap <- 
  apoat %>% 
  rename(cstweak_yield_kgha = yield_kgha) %>% 
  select(-dtype) %>% 
  left_join(select(base_contc, - oat_nu), by = "year") %>% 
  rename(csbase_yield_kgha = yield_kgha) %>% 
  mutate(gap_kgha = yield_kgha.y - yield_kgha.x, #--tweaked minus base (rotc)
         dtype = "oat_gap") %>% 
  select(dtype, oat_nu, year, gap_kgha)

agap <- 
  apw %>% 
  left_join(select(base_rotc, - oat_nu), by = "year") %>% #--I know this is messy but it's fine 
  mutate(gap_kgha = yield_kgha.y - yield_kgha.x, #--tweaked rotc minus base rotc
         dtype = "oat_gap") %>% 
  select(dtype, oat_nu, year, gap_kgha)




gaps <- 
  ewgap %>% 
  bind_rows(agap) %>% 
  left_join(oat_key)



# look at gaps ------------------------------------------------------------

gaps %>% 
  ggplot(aes(reorder(oat_what, gap_kgha), gap_kgha)) + 
  geom_boxplot(aes(color = oat_what == "exp gap")) +
  geom_point() + 
  coord_flip() + 
  guides(color = F) + 
  labs(title = "Gap using CS as base sim with tweaks")


# get exp data in same form -----------------------------------------------

ew <- 
  saw_tidysawyer %>% 
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


  