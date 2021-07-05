# Created:      7/5/2021
#
# purpose: evaluate each factor individually and in combination using CALIBRATED APSIM sims 
#          use CC sim as base, tweaking from that
#          changed apsim files by hand (ames only)
#
# author: gina vnichols@iastate.edu
#
# notes: keep apsim sims in box, all r code in github
# last edited:   3/31/2020 (I was confused...)
#                11/23/2020 cleaning, added rfv50% (oat22)
#                1/11/2021 moved sims to github, much easier
#                7/5/2021 trying to figure out what I did...
#                7/5/2021 moving fig creation to separate folder for manu figs

rm(list = ls())
library(saapsim) #--has some functions
library(tidysawyer2) #--has sawyer data
library(tidyverse)
library(janitor)

source("05_manu-figs/palettes.R")

#--calculated in the named folder
gaps_filt <- read_csv("01_sims-oat-by-hand/dat_tidy-hand-oats.csv")

#--note, I want to get the errors on the experimentally observed ones


#--wrote it, edited by hand
# gaps_filt %>% 
#   select(dtype, oat_nu, oat_what) %>% 
#   distinct() %>% 
#   write_csv("05_manu-figs/oats_nice.csv")
# 

oats_nice <- read_csv("05_manu-figs/oats_nice.csv")


# windmill ----------------------------------------------------------------

yrs_ord <- 
  gaps_filt %>% 
  filter(oat_what == "exp gap", category == "1 factor") %>% 
  arrange(gap_kgha) %>% 
  select(year) %>% 
  pull()

gaps_filt_wind <- 
  gaps_filt %>%
  left_join(oats_nice) %>% 
  arrange(oat_what_order) %>% 
  mutate(year = factor(year, levels = yrs_ord),
         oat_what = fct_inorder(oat_what),
         oat_what_nice = fct_inorder(oat_what_nice)) %>% 
  group_by(dtype, oat_what, category) %>% 
  mutate(mngap = mean(gap_kgha, na.rm = T))

wind_theme_V <-    theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

wind_theme_H <-    theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# get 1 factor and 5 factor together

f1 <- 
  gaps_filt_wind %>%
  filter(category %in% c("1 factor"), oat_what != "dec RFV10%") 

f5 <- 
  gaps_filt_wind %>%
  filter(category %in% c("5 factor"), oat_what != "dec RFV10%") %>%
  filter(!grepl("gap", oat_what))
  



f1 %>% 
  bind_rows(f5) %>% 
  mutate(
    year = factor(year, levels = yrs_ord),
    col1 = case_when(
      oat_what == "exp gap" ~ "A",
      oat_what == "current apsim gap" ~ "B",
      oat_what == "late emergence" ~ "C",
      TRUE ~ "D")
  ) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = col1, color = col1),
           position = "dodge", 
           stat = "identity", 
           #color = "black"
  ) + 
  facet_grid(.~oat_what_nice, labeller = label_wrap_gen(width = 10)) + 
  guides(fill = F, color = F) +
  scale_fill_manual(values = c("A" = dkbl1, "B" = grn1, "C" = ylw1, "D" = dkpnk1)) +
  scale_color_manual(values = c("A" = dkbl1, "B" = grn1, "C" = ylw1, "D" = dkpnk1)) +
  geom_hline(aes(yintercept = mngap), size = 1, type = "dashed") +
  labs(title = "Ames",
       x = NULL,
       y = "Continuous Maize Penalty (kg ha-1)") + 
  wind_theme_H

