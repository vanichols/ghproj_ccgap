# Created:    april 17 2021
#
# purpose: comparing n- and non-n gaps
#
# notes: 
# last edited: july 16 2025   

rm(list = ls())
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(fancycut)
library(patchwork)
library(scales)
library(naniar)


theme_set(theme_bw())

source("05_manu-figs/palettes.R")




# gap components --------------------------------------------------------------------

dat <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap, ngap)

dat %>%
  arrange(nonngap, ngap) %>% 
  filter(!is.na(nonngap)) %>% 
  mutate(id = 1:n()) %>% 
  filter(nonngap > 0)


# aonr conceptual ---------------------------------------------------------

tst.tib <- 
  ilia_yields

tst.aonrs <- read_csv("00_empirical-n-cont/dat_aonrs.csv")
tst.prds <- read_csv("00_empirical-n-cont/dat_preds.csv")

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

viz.aonr

#--sc aonr line
n_1 <- viz.aonr %>% filter(rotation == "sc") %>% pull(nrate_kgha)

sc_1 <- 
  viz.prds %>% 
  filter(rotation == "sc") %>% 
  filter(nrate_kgha == viz.aonr %>% filter(rotation == "sc") %>% pull(nrate_kgha)) %>% 
  pull(pred_yield)

cc_1 <- 
  viz.prds %>% 
  filter(rotation == "cc") %>% 
  filter(nrate_kgha == viz.aonr %>% filter(rotation == "sc") %>% pull(nrate_kgha)) %>% 
  pull(pred_yield)

#--at cc aonr
n_2 <- viz.aonr %>% filter(rotation == "cc") %>% pull(nrate_kgha)

sc_2 <- 
  viz.prds %>% 
  filter(rotation == "sc") %>% 
  filter(nrate_kgha == viz.aonr %>% filter(rotation == "cc") %>% pull(nrate_kgha)) %>% 
  pull(pred_yield)


cc_2 <- 
  viz.prds %>% 
  filter(rotation == "cc") %>% 
  filter(nrate_kgha == viz.aonr %>% filter(rotation == "cc") %>% pull(nrate_kgha)) %>% 
  pull(pred_yield)

fig_conc <- 
  ggplot() + 
  geom_point(data = viz.obs, aes(x = nrate_kgha, y = yield_kgha/1000, color = rot)) + 
  #--vertical dashes
  geom_segment(aes(yend = sc_1/1000, y = cc_1/1000, x = n_1, xend = n_1), 
               size = 1.2, linetype = "dashed", color = "gray80") +
  geom_line(data = viz.obs, aes(x = nrate_kgha, y = yield_kgha/1000, color = rot), linetype = "dashed") + 
  geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield/1000, color = rot), size = 2) + 
  geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield/1000, fill = rot, shape = rot), size = 4, stroke = 2) + 
  # geom_segment(aes(yend = sc_2/1000, y = cc_2/1000, x = n_2, xend = n_2),
  #              size = 1.2, linetype = "dashed", color = "gray80") +
  #--arrows
  geom_segment(aes(x = 310, xend = 330, 
                   y = sc_1/1000, yend = sc_1/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  geom_segment(aes(x = n_1, xend = 330, 
                   y = cc_1/1000, yend = cc_1/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  geom_segment(aes(x = 310, xend = 330, 
                   y = cc_2/1000, yend = cc_2/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
    #--rectangles
  geom_rect(aes(xmin = 340, xmax = 360,
                ymin = cc_1/1000, ymax = cc_2/1000),
            fill = "gray80", color = "black") +
  geom_rect(aes(xmin = 340, xmax = 360,
                ymin = cc_2/1000, ymax = sc_2/1000),
            fill = grn1, color = "black") +
  #--text for agronom yield gap
  geom_text(aes(x = 330, y = ((cc_2 + cc_1)/2)/1000+0.2, label = "Agronomic yield gap"),
            hjust = 1, fontface = "bold") +
  geom_text(aes(x = 330, y = ((cc_2 + cc_1)/2)/1000, label = "Yield from ecological services that can be replaced by inputs"),
            hjust = 1, fontface = "italic") +
    #--text for ecolog yield gap
  geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000+0.2, label = "Ecological yield gap"),
            hjust = 1, fontface = "bold") +
  geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000, label = "Yield from ecological services that cannot be provided by inputs"),
            hjust = 1, fontface = "italic") +
    scale_color_manual(values = c("Continuous maize AONR" = ylw1, 
                                "Rotated maize AONR" = dkbl1)) +
  scale_fill_manual(values = c("Continuous maize AONR" = ylw1, 
                               "Rotated maize AONR" = dkbl1)) +
  scale_shape_manual(values = c("Continuous maize AONR" = 24, 
                         "Rotated maize AONR" = 22)) +
  labs(x = expression(Nitrogen~fertilization~rate~(kg~N~ha^{-1})),
       y = expression(Maize~grain~yield~(dry~Mg~ha^{-1})),
       
       #expression(flux*phantom(x)*(g~CO[2]~m^{-2}~h^{-1})))
       shape = NULL,
       color = NULL,
       fill = NULL) +
  theme_bw() + 
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(color= "black"),
        axis.title.y = element_text(angle = 90, vjust = 0.5))



fig_conc


ggsave("05_manu-figs/fig1_conceptual-gaps.png", 
       width = 8, height = 6)


