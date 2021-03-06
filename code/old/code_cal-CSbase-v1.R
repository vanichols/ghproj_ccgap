# Created:      3/9/2020
# purpose: evaluate each factor individually and in combination using CALIBRATED APSIM sims
# author: gina vnichols@iastate.edu
#
# notes: keep apsim sims in box, all r code in github
#
# last edited:   3/30/2020 (first time looking at it since covid-19)


rm(list = ls())
#remotes::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has exp data in it
library(tidyverse)
library(janitor)

# keeping track of oats ---------------------------------------------------

oat_key <-
  read_csv("../../../Box/Gina_APSIM_modeling/sims_cal-factor-analysis-CSbase/data-raw/rd_oats-calibrated-v1.csv") %>% 
  #read_csv("sims_Mitch-cal-factor-analysis/data-raw/rd_oats-calibrated-v1.csv") %>% 
  separate_rows(category, sep = ",") %>% 
  remove_empty("rows") %>% 
  remove_empty("cols")


# read in .out files ------------------------------------------------------

my_dir <- "../../../Box/Gina_APSIM_modeling/sims_cal-factor-analysis-CSbase/"

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
  select(-sd_kgha) %>% #--this needs updated w/new pkg
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
  ggplot(aes(reorder(oat_what, gap_kgha), gap_kgha)) + 
  geom_boxplot(aes(color = oat_what == "exp gap")) +
  geom_point() + 
  coord_flip() + 
  guides(color = F) + 
  labs(title = "Gap using CS as base sim with tweaks in Ames")

ggsave("_figs/fctrs_cs-base-oat-ames.png")



  