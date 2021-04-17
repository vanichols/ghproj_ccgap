# Created:    april 2 2021
#
# purpose: make Penalty over time fig
#
# notes: 
# last edited:   

rm(list = ls())
#devtools::install_github("femiguez/nlraa")
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(fancycut)
library(patchwork)
library(scales)

theme_set(theme_bw())
sc_color <- "#2a89c3" 
cc_color <- "#fdb462"



# what is driving Penalty --------------------------------------------------------------------

dat <- 
  tibble(scen = c(rep(c("Penalty driven by\ncontinuous maize yields", 
                            "Penalty driven by\nrotated maize yields", 
                            "Penalty driven by both"), each = 2)), 
       rot = c(rep(c("Rotated maize", "Continuous maize"), times = 3)),
       yield = c(10, 8, 12, 10, 11, 9)) %>% 
  mutate(scen = factor(scen, levels = c("Penalty driven by\nrotated maize yields",
                                        "Penalty driven by\ncontinuous maize yields",
                                        "Penalty driven by both")),
         rot = factor(rot, levels = c("Rotated maize", "Continuous maize")))

dat %>% 
  ggplot(aes(scen, yield)) + 
  geom_col(aes(fill = rot), position = position_dodge(width = 0.8)) + 
  geom_hline(yintercept = 10, color = "red", size = 7) + 
  geom_text(x = 0.7, y = 10, label = "Yield potential", check_overlap = T, fontface = "italic", color = "white") + 
  scale_fill_manual(values = c("Rotated maize" = sc_color,
                               "Continuous maize" = cc_color)) + 
  labs(x = NULL,
       fill = NULL,
       y = "Maize grain yield (Mg ha-1)") + 
  theme(legend.position = "top")

ggsave("05_manu-figs/fig_conceptual-Penalty.png")



# aonr conceptual ---------------------------------------------------------

tst.tib <- 
  ilia_yields

tst.aonrs <- read_csv("00_empirical-n-cont/dat_aonrs.csv")
tst.prds <- read_csv("00_empirical-n-cont/dat_preds.csv")



# viz ---------------------------------------------------------------------

#--create one for a site-year (as example of methodology)

viz.aonr <- 
  tst.aonrs %>% 
  separate(aonr_rot, into = c("x", "rotation")) %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  select(-x) %>% 
  left_join(tst.prds) %>% 
  filter(site == "ames",
         year == 2003) %>% 
  mutate(rot = ifelse(rotation == "cc", "Continuous maize AONR", "Rotated maize AONR"))

viz.prds <- 
  tst.prds %>% 
  filter(site == "ames",
         year == 2003) %>% 
  filter(nrate_kgha < 300) %>% 
  mutate(rot = ifelse(rotation == "cc", "Continuous maize AONR", "Rotated maize AONR"))

viz.obs <- 
  ilia_yields %>% 
  mutate(nrate_kgha = round(nrate_kgha, 0)) %>% 
  filter(site == "ames",
         year == 2003) %>% 
  mutate(rot = ifelse(rotation == "cc", "Continuous maize AONR", "Rotated maize AONR"))

ggplot() + 
  geom_point(data = viz.obs, aes(x = nrate_kgha, y = yield_kgha/1000, color = rot)) + 
  geom_line(data = viz.obs, aes(x = nrate_kgha, y = yield_kgha/1000, color = rot), linetype = "dashed") + 
  geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield/1000, color = rot), size = 2) + 
  geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield/1000, fill = rot), pch = 23, size = 3, stroke = 2) + 
  scale_color_manual(values = c("Continuous maize AONR" = cc_color, 
                                "Rotated maize AONR" = sc_color)) +
  scale_fill_manual(values = c("Continuous maize AONR" = cc_color, 
                                "Rotated maize AONR" = sc_color)) +
  labs(x = "Nitrogen fertilization rate\n(kg N ha-1)",
       y = "Maize grain yield\n(dry Mg ha-1)",
       color = NULL,
       fill = NULL) +
  theme_bw() + 
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(color= "black"))
  
  
ggsave("05_manu-figs/fig_conceptual-calc-ames03.png")



