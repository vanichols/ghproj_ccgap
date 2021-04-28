# Created:    april 17 2021
#
# purpose: comparing n- and non-n gaps
#
# notes: 
# last edited:   

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

fig_comp <- 
  dat %>%
  arrange(nonngap, ngap) %>% 
  filter(is.na(nonngap)) %>% 
  bind_rows(
    dat %>%
      arrange(nonngap, ngap) %>% 
      filter(!is.na(nonngap))
  ) %>% 
  mutate(id = 1:n(),
         nonngap = ifelse(nonngap == 0 & ngap == 0, 10, nonngap),
         ngap = ifelse(is.na(nonngap), 0, ngap),
         nonngap = ifelse(is.na(nonngap), 10, nonngap)) %>% 
  pivot_longer(nonngap:ngap) %>%
  mutate(name = ifelse(name == "ngap", "Yield gap from N factors", "Yield gap from other factors")) %>% 
  ggplot(aes(id, value)) + 
  geom_col(aes(fill = name), width = 1, color = "black") + 
  geom_vline(xintercept = 0, color = "gray50") +
  geom_vline(xintercept = 36.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 42.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 48.5, linetype = "dashed", color = "gray50") +
  geom_text(x = 18, y = 2000, label = "36 site-years, undetermined components",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 40, y = 2000, label = "6 site-years, no yield gap",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 46, y = 2000, label = "6 site-years, yield gap from N factors",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 90, y = 2000, label = "109 site-years, yield gap from\n  mixture of factors",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  scale_fill_manual(values = c(ylw1, ltbl1)) +
  scale_y_continuous(limits = c(0, 6000)) +
  theme(legend.justification = c(0,0),
        legend.position = c(0.05, 0.05),
        legend.background = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  #  theme(legend.position = "top") +
  labs(fill = NULL,
       y = "Yield gap between\ncontinuous- and rotated-maize (kg ha-1)",
       x = "Site-year") +
  coord_flip()


fig_comp

ggsave("05_manu-figs/fig_gap-components-windmill.png", height = 7)



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
  geom_line(data = viz.obs, aes(x = nrate_kgha, y = yield_kgha/1000, color = rot), linetype = "dashed") + 
  geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield/1000, color = rot), size = 2) + 
  geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield/1000, fill = rot), pch = 23, size = 4, stroke = 2) + 
  #--vertical dashes
  geom_segment(aes(yend = sc_1/1000, y = cc_1/1000, x = n_1, xend = n_1), 
               size = 1.2, linetype = "dashed", color = "gray80") +
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
            fill = ylw1, color = "black") +
  geom_rect(aes(xmin = 340, xmax = 360,
                ymin = cc_2/1000, ymax = sc_2/1000),
            fill = ltbl1, color = "black") +
  geom_text(aes(x = 350, y = ((cc_2 + cc_1)/2)/1000, label = "Yield gap from N factors"),
            hjust = 1, fontface = "italic") +
    geom_text(aes(x = 350, y = ((sc_2 + cc_2)/2)/1000, label = "Yield gap from other factors"),
              hjust = 1, fontface = "italic") +
    scale_color_manual(values = c("Continuous maize AONR" = pnk1, 
                                "Rotated maize AONR" = dkbl1)) +
  scale_fill_manual(values = c("Continuous maize AONR" = pnk1, 
                               "Rotated maize AONR" = dkbl1)) +
  labs(x = "Nitrogen fertilization rate\n(kg N ha-1)",
       y = "Maize grain yield\n(dry Mg ha-1)",
       color = NULL,
       fill = NULL) +
  theme_bw() + 
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(color= "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))



fig_conc
ggsave("05_manu-figs/fig_conceptual-calc-ames03.png")



# combibne them -----------------------------------------------------------


fig_conc / fig_comp + plot_layout(heights = c(1, 2))
ggsave("05_manu-figs/fig_conceptual-and-components.png", height = 12, width = 6.5)



