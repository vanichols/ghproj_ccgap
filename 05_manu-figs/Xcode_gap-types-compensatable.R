# Created:    april 17 2021
#
# purpose: comparing n- and non-n gaps
#
# notes: 
# last edited:   7/6/2021

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



# vertical--------------------------------------------------------------


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
  ggplot(aes(id, value/1000)) + 
  geom_col(aes(fill = name), width = 1, color = "black") + 
  geom_vline(xintercept = 0, color = "gray50") +
  geom_vline(xintercept = 36.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 42.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 48.5, linetype = "dashed", color = "gray50") +
  geom_text(x = 18, y = 2, label = "36 site-years, undetermined components",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 40, y = 2, label = "6 site-years, no yield gap",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 46, y = 2, label = "6 site-years, yield gap from N factors only",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 90, y = 2, label = "109 site-years, yield gap from\n  mixture of factors",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  scale_fill_manual(values = c(ylw1, ltbl1)) +
  scale_y_continuous(limits = c(0, 6)) +
  theme(
    #legend.justification = c(0,0),
     #   legend.position = c(0.05, 0.05),
    legend.position = "bottom",
        legend.background = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  #  theme(legend.position = "top") +
 # guides(fill = F) +
  labs(fill = NULL,
       y = "Yield gap between\ncontinuous- and rotated-maize (Mg ha-1)",
       x = "Site-year") +
  coord_flip()


fig_comp

ggsave("05_manu-figs/fig_gap-components-windmill.png", height = 7)


# vertical, pyramid--------------------------------------------------------------


fig_comp2 <- 
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
         nonngap = ifelse(is.na(nonngap), 10, nonngap),
         ngap = -ngap) %>% 
  pivot_longer(nonngap:ngap) %>%
  mutate(name = ifelse(name == "ngap", "Gap closed through N fert", "Remaining gap")) %>% 
  ggplot(aes(id, value/1000)) + 
  geom_col(aes(fill = name), width = 1, color = "black") + 
  geom_vline(xintercept = 0, color = "gray50") +
  geom_vline(xintercept = 36.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 42.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 48.5, linetype = "dashed", color = "gray50") +
  geom_text(x = 18, y = 1.5, label = "36 site-years, undetermined components",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 40, y = 1.5, label = "6 site-years, no yield gap",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 46, y = 1.5, label = "6 site-years, yield gap closed through N fert",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 90, y = 1.5, label = "109 site-years, yield gap\n  not closed with N fert",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  scale_fill_manual(values = c(ylw1, ltbl1)) +
  #scale_y_continuous(limits = c(0, 6)) +
  theme(
    #legend.justification = c(0,0),
    #   legend.position = c(0.05, 0.05),
    legend.position = "bottom",
    legend.background = element_blank(),
    axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  #  theme(legend.position = "top") +
  # guides(fill = F) +
  labs(fill = NULL,
       y = "Yield gap between\ncontinuous- and rotated-maize (Mg ha-1)",
       x = "Site-year") +
  coord_flip()


fig_comp2

ggsave("05_manu-figs/fig_gap-components-windmill.png", height = 7)


# horizontal--------------------------------------------------------------


fig_comp_horiz <- 
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
  ggplot(aes(id, value/1000)) + 
  geom_col(aes(fill = name), width = 1, color = "black") + 
#  geom_vline(xintercept = 0, color = "gray50") +
  geom_vline(xintercept = 36.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 42.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 48.5, linetype = "dashed", color = "gray50") +
  geom_text(x = 18, y = 3.8, label = "36 site-years, undetermined components",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray10") +
  geom_text(x = 39, y = 4.200, label = "6 site-years, no yield gap",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray10") +
  geom_text(x = 45, y = 4.600, label = "6 site-years, yield gap from N factors only",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray10") +
  geom_text(x = 65, y = 5.00, label = "109 site-years, yield gap from mixture of factors",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray10") +
  scale_fill_manual(values = c(ylw1, ltbl1)) +
  scale_y_continuous(limits = c(0, 6.200)) +
  theme(legend.justification = c(0,1),
        legend.position = c(0.01, 0.99),
        legend.background = element_rect(color = "black"),
        axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  #  theme(legend.position = "top") +
  labs(fill = NULL,
       y = "Yield gap between continuous- and rotated-maize\n(Mg ha-1)",
       x = "Site-year")


fig_comp_horiz

ggsave("05_manu-figs/fig_gap-components-windmill-horiz.png", width = 7)



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
        axis.title.y = element_text(angle = 90, vjust = 0.5))



fig_conc
ggsave("05_manu-figs/fig_conceptual-calc-ames03.png")

fig_conc2 <- 
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
               size = 1, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  geom_segment(aes(x = n_1, xend = 330, 
                   y = cc_1/1000, yend = cc_1/1000), 
               size = 1, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  geom_segment(aes(x = 310, xend = 330, 
                   y = cc_2/1000, yend = cc_2/1000), 
               size = 1, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  #--rectangles
  geom_rect(aes(xmin = 340, xmax = 360,
                ymin = cc_1/1000, ymax = cc_2/1000),
            fill = ylw1, color = "black") +
  geom_rect(aes(xmin = 340, xmax = 360,
                ymin = cc_2/1000, ymax = sc_2/1000),
            fill = ltbl1, color = "black") +
  geom_text(aes(x = 350, y = ((cc_2 + cc_1)/2)/1000, label = "Yield gap closed through N fert"),
            hjust = 1, fontface = "italic") +
  geom_text(aes(x = 350, y = ((sc_2 + cc_2)/2)/1000, label = "Yield gap remaining"),
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
        axis.title.y = element_text(angle = 90, vjust = 0.5))



fig_conc2
ggsave("05_manu-figs/fig_conceptual-calc-ames03.png")


#--horizontal (y axis not rotated)

fig_conc_horiz <- 
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
        #legend.background = element_rect(color= "black"),
        legend.background = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5))



fig_conc_horiz


#--horizontal (y axis not rotated)

fig_conc_nolabs <- 
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
  # geom_text(aes(x = 350, y = ((cc_2 + cc_1)/2)/1000, label = "Yield gap from N factors"),
  #           hjust = 1, fontface = "italic") +
  # geom_text(aes(x = 350, y = ((sc_2 + cc_2)/2)/1000, label = "Yield gap from other factors"),
  #           hjust = 1, fontface = "italic") +
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
        #legend.background = element_rect(color= "black"),
        legend.background = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 0.5))



# gap type densities ------------------------------------------------------

#--histo

fig_histo <- 
  dat %>% 
  select(site, year, nonngap, ngap) %>% 
  pivot_longer(nonngap:ngap) %>% 
  mutate(name = ifelse(name == "ngap", "Yield gap from N factors", "Yield gap from other factors")) %>% 
  ggplot(aes(value/1000)) + 
  geom_density(aes(fill = name), alpha = 0.6) + 
  scale_fill_manual(values = c(ylw1, ltbl1)) +
  labs(x = "Yield gap between\ncontinuous- and rotated-maize (Mg ha-1)",
       y = "Data density",
       fill = NULL) +
  guides(fill = F) +
  theme(axis.text.y = element_blank(),
        #legend.justification = c(1, 1),
        #legend.position = c(0.9, 0.9),
        legend.background = element_blank(),
        legend.position = "bottom")

fig_histo

# combibne them -----------------------------------------------------------

#--vertical
(fig_conc_horiz / fig_histo) | fig_comp

#--fancy legend work
((fig_conc / fig_histo + plot_layout(guides = "keep")) | fig_comp) + 
  plot_layout(guides = "collect") & theme(legend.position = 'top')

((fig_conc_nolabs / fig_histo) | fig_comp) + 
  plot_layout(guides = "collect") & theme(legend.position = 'top')



ggsave("05_manu-figs/fig_conceptual-and-components.png", height = 12, width = 6.5)


#--horizontal
(fig_conc_horiz + fig_histo) /fig_comp_horiz + plot_layout(widths =  c(1, 2))

ggsave("05_manu-figs/fig_conceptual-and-components-horiz.png", height = 5, width = 15)
ggsave("05_manu-figs/fig_conceptual-and-components-horiz.png", height = 4.5, width = 13)


#--just concept and pyramid
fig_conc2 + fig_comp2 + plot_layout(widths = c(1.5, 1))

ggsave("05_manu-figs/fig_conceptual-and-components-horiz.png", height = 5, width = 15)
ggsave("05_manu-figs/fig_conceptual-and-components-horiz.png", height = 4.5, width = 13)

