# Created:       2/4/2020
# last edited:   2/17/2010 added apsim cc/sc versus sawyer cc/sc
# 
# purpose: look at how apsim simulations of sc rotations compare to sawyer's data
# author: gina vnichols@iastate.edu


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package
library(tidyverse)



# keeping track of oats ---------------------------------------------------
oat_key <- read_csv("sims_prelim-testing-gina/data-raw/rd_oat-descriptions-v1.csv") %>% 
  separate_rows(category, sep = ",")


# read in .out files ------------------------------------------------------

my_dir <- "sims_prelim-testing-gina/"

apd <- 
  saf_readapout(my_dir) %>% 
  select(file, year, crop_yield) %>% 
  mutate(yield_kgha = saf_buac_to_kgha_corn(crop_yield)) %>% 
  separate(file, into = c("crop", "Nrate", "oat_desc")) %>% 
  # remove old files w/o oat desc
  filter(!is.na(oat_desc)) %>% 
  mutate(oat = parse_number(oat_desc)) %>% 
  select(-oat_desc, -crop_yield) %>% 
  # merge wi/oat_key
  left_join(oat_key) %>% 
  mutate(nrate_kgha = parse_number(Nrate),
         crop = "corn",
         rotation = "cc", 
         site = "ames") 

#--look at it
apd %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = oat_what))

apd %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = oat_what)) +
  guides(color = F) +
  facet_wrap(~oat_what)


# fit curves and get preds ------------------------------------------------

#--sawyer data for ames cc ans sc separately
saw_fits_cc <- 
  sad_tidysawyer %>% 
  filter(site == "ames",
         rotation == "cc"
         ) %>% 
  group_by(site) %>% 
  nest() %>% 
  saf_fitNresp() %>% 
    filter(model == "QP") %>% 
    saf_predNresp() %>% 
    select(site, nrate_kgha, pred_kgha) %>%
    rename(jsCC_kgha = pred_kgha)

saw_fits_sc <- 
  sad_tidysawyer %>% 
  filter(site == "ames",
         rotation == "sc"
  ) %>% 
  group_by(site) %>% 
  nest() %>% 
  saf_fitNresp() %>% 
  filter(model == "QP") %>% 
  saf_predNresp() %>% 
  select(site, nrate_kgha, pred_kgha) %>%
  rename(jsSC_kgha = pred_kgha)

    
#--apsim simulations for ames sc w/alterations
ap_fits <- 
  apd %>% 
  group_by(oat_what, category, site) %>% 
  nest() %>% 
  saf_fitNresp() %>%  #--fit the curves
  filter(model == "QP") %>% 
  saf_predNresp() %>%  #--predict based on fits
  select(oat_what, site, nrate_kgha, pred_kgha) 
  
#--join sawyer and apsim fits/preds
res_fits <- 
  ap_fits %>% 
  left_join(saw_fits_cc) %>% 
  left_join(saw_fits_sc) %>% 
  #--the absolute yields are very different
  #--make all curves start at 0?
  group_by(oat_what, site) %>% 
  mutate(min_jsCC = min(jsCC_kgha),
         min_jsSC = min(jsSC_kgha),
         minpred = min(pred_kgha),
         scl_pred_kgha = pred_kgha - minpred,
         scl_jsCC_kgha = jsCC_kgha - min_jsCC,
         scl_jsSC_kgha = jsSC_kgha - min_jsSC)


# make things more graph-friendly -----------------------------------------

#--get baseline CS on it's own

res_oats <- res_fits %>% 
  filter(oat_what != "base_cs")

catsuniq <- res_oats %>% ungroup() %>%  select(category) %>% distinct()

res_base <- 
  res_fits %>% 
  ungroup() %>% 
  filter(oat_what == "base_cs") %>% 
  select(-category) %>% 
  crossing(catsuniq)



# look at sawyer and others -----------------------------------------------

#--very manual, scaled
ggplot() + 
  #--base apsim cs
  geom_line(data = filter(res_fits, oat_what == "base_cs"),
            aes(nrate_kgha, scl_pred_kgha), color = "gray50", size = 2) + 
  #--apsim things
  geom_line(data = filter(res_fits, oat_what != "base_cs"),
            aes(nrate_kgha, scl_pred_kgha, color = oat_what), size = 2) + 
  #--sawyer cc
  geom_line(data = res_fits, 
            aes(nrate_kgha, scl_jsCC_kgha),
            color = "gray50", linetype = "dotted", size = 2) +
  #--sawyer sc
  geom_line(data = res_fits, 
            aes(nrate_kgha, scl_jsSC_kgha),
            color = "white", linetype = "dotted", size = 2) +
  facet_grid(.~category) + 
  scale_color_brewer(palette = "Spectral") + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")+ 
  labs(title = "white = sawyer sc, gray = sawyer cc")


ggsave("_figs/diag_scl_curves.png")

#--very manual, not scaled
ggplot() + 
  #--base apsim cs
  geom_line(data = filter(res_fits, oat_what == "base_cs"),
            aes(nrate_kgha, pred_kgha), color = "gray50", size = 2) + 
  #--apsim things
  geom_line(data = filter(res_fits, oat_what != "base_cs"),
            aes(nrate_kgha, pred_kgha, color = oat_what), size = 2) + 
  #--sawyer cc
  geom_line(data = res_fits, 
            aes(nrate_kgha, jsCC_kgha),
            color = "gray50", linetype = "dotted", size = 2) +
  #--sawyer sc
  geom_line(data = res_fits, 
            aes(nrate_kgha, jsSC_kgha),
            color = "white", linetype = "dotted", size = 2) +
  facet_grid(.~category) + 
  scale_color_brewer(palette = "Spectral") + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal") + 
  labs(title = "white = sawyer sc, gray = sawyer cc")


ggsave("_figs/diag_unscl_curves.png")


# look at sawyer sc verus cc and apsim baseline sc and cc-----------------

my_dir <- "sims_apsim-CC-vs-CS/"

apd2 <- 
  saf_readapout(my_dir) %>% 
  select(file, year, crop_yield) %>% 
  mutate(yield_kgha = saf_buac_to_kgha_corn(crop_yield)) %>% 
  separate(file, into = c("crop", "Nrate", "rotation")) %>% 
  mutate(nrate_kgha = parse_number(Nrate),
         crop = tolower(crop),
         site = "ames") 

#--fit data and get predictions
apf2 <- 
  apd2 %>% 
  group_by(site, crop, rotation) %>% 
  nest() %>% 
  saf_fitNresp() %>% 
  filter(model == "QP") %>% 
  saf_predNresp() %>% 
  select(crop, rotation, site, nrate_kgha, pred_kgha) %>% 
  mutate(yield_kgha = pred_kgha)

sawdcc <- saw_fits_cc %>% 
  mutate(rotation = "cc",
         yield_kgha = jsCC_kgha)

sawdsc <- saw_fits_cc %>% 
  mutate(rotation = "sc",
         yield_kgha = jsCC_kgha)

#--look at it
apd2 %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) +  
  geom_jitter(data = sad_tidysawyer, 
             aes(nrate_kgha, yield_kgha),
             pch = 19, color = "red") +
  geom_jitter(color = "black", pch = 19, size = 2, aes(color = rotation)) +
  geom_line(data = sawdcc, color = "darkred", size = 2) +
  geom_line(data = sawdsc, color = "darkred", size = 2) +
  geom_line(data = sawdcc, color = "darkred", size = 2) +
  #geom_line(data = apf2, color = "gray10", size = 2) +
  #facet_grid(.~rotation) + 
  labs(title = "black = apsim; red = sawyer")

ggsave("_figs/diag_rawapsim_curves.png")


# fit cuves, save parms ---------------------------------------------------

#--parms from apsim sims
ap_prms <- 
  apd %>% 
  group_by(oat_what, category, rotation, site) %>% 
  nest() %>% 
  saf_fitNresp() %>%  #--fit the curves
  saf_smyNfits() %>% 
  filter(model == "QP") %>% 
  pivot_longer(a:r2, names_to = "parms", values_to = "apsim_vals")

saw_prms <- 
  sad_tidysawyer %>% 
  filter(site == "ames",
         rotation == "cc") %>% 
  group_by(site, rotation) %>% 
  nest() %>% 
  saf_fitNresp() %>% 
  saf_smyNfits() %>% 
  filter(model == "QP") %>% 
  pivot_longer(a:r2, names_to = "parms", values_to = "sawyer_vals")

#--add sawyer cs parms
saw_prms_cs <- 
  sad_tidysawyer %>% 
  filter(site == "ames",
         rotation == "cs") %>% 
  group_by(site, rotation) %>% 
  nest() %>% 
  saf_fitNresp() %>% 
  saf_smyNfits() %>% 
  filter(model == "QP") %>% 
  pivot_longer(a:r2, names_to = "parms", values_to = "sawyer_vals")

#--join sawyer and apsim parms
res_prms <- 
  ap_prms %>%  
  left_join(saw_prms)


res_prms  %>%
  pivot_longer(apsim_vals:sawyer_vals) %>% 
  filter(parms != "a") %>% 
  ggplot(aes(parms, value, shape = name, color = oat_what)) + 
  geom_point(size = 4) + 
  facet_wrap(parms~category, scales = "free")

