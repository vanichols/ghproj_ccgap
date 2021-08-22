# Created:    aug 17 2021
#
# purpose: make conceptual fig for talk
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

source("05_talk-figs/talk-palette3.R")


yld_lab <- (expression(atop("Corn grain yield", paste("(dry Mg "~ha^-1*")"))))

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
  mutate(rot = ifelse(rotation == "cc", "Continuous corn AONR", "Rotated corn AONR"))

viz.prds <- 
  tst.prds %>% 
  filter(site == "ames",
         year == 2003) %>% 
  filter(nrate_kgha < 300) %>% 
  mutate(rot = ifelse(rotation == "cc", "Continuous corn AONR", "Rotated corn AONR"))

viz.obs <- 
  ilia_yields %>% 
  mutate(nrate_kgha = round(nrate_kgha, 0)) %>% 
  filter(site == "ames",
         year == 2003) %>% 
  mutate(rot = ifelse(rotation == "cc", "Continuous corn AONR", "Rotated corn AONR"))

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



# all the goodies ---------------------------------------------------------


ggplot() + 
  geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield/1000, color = rot), size = 2) + 
  geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield/1000, fill = rot), pch = 23, size = 4, stroke = 2) + 
  #--arrows
  geom_segment(aes(x = 310, xend = 330, 
                   y = sc_1/1000, yend = sc_1/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  geom_segment(aes(x = 310, xend = 330, 
                   y = cc_2/1000, yend = cc_2/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
    #--rectangles
  geom_rect(aes(xmin = 340, xmax = 360,
                ymin = cc_2/1000, ymax = sc_2/1000),
            fill = clr_red, color = "black") +
    geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000+0.3, 
                  label = "Continuous corn penalty"),
              hjust = 1, fontface = "italic", size = 7) +
  geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000-0.3, 
                label = paste(expression(2~Mg~ha^-1))),
            hjust = 1, fontface = "italic", size = 7, parse = T) +
    scale_color_manual(values = c("Continuous corn AONR" = clr_cc, 
                                "Rotated corn AONR" = clr_rot)) +
  scale_fill_manual(values = c("Continuous corn AONR" = clr_cc, 
                               "Rotated corn AONR" = clr_rot)) +
  labs(x = expression(Nitrogen~fertilization~rate~(kg~N~ha^{-1})),
       y = yld_lab,
       color = NULL,
       fill = NULL,
       title = "Ames, 2008") +
  scale_x_continuous(limits = c(0, 360)) +
  theme_bw() + 
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(color= "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.text = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)),
        axis.title = element_text(size =rel(1.5)),
        panel.grid = element_blank(),
        plot.title = element_text(size = rel(2)))


ggsave("05_talk-figs/fig_concep3.png", width = 8.73, height = 4.7)


# no rectange/labels ---------------------------------------------------------


ggplot() + 
  geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield/1000, color = rot), size = 2) + 
  geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield/1000, fill = rot), pch = 23, size = 4, stroke = 2) + 
  #--arrows
  geom_segment(aes(x = 310, xend = 330, 
                   y = sc_1/1000, yend = sc_1/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  geom_segment(aes(x = 310, xend = 330, 
                   y = cc_2/1000, yend = cc_2/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  # #--rectangles
  # geom_rect(aes(xmin = 340, xmax = 360,
  #               ymin = cc_2/1000, ymax = sc_2/1000),
  #           fill = clr_red, color = "black") +
  # geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000+0.3, 
  #               label = "Continuous corn penalty"),
  #           hjust = 1, fontface = "italic", size = 7) +
  # geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000-0.3, 
  #               label = paste(expression(2~Mg~ha^-1))),
  #           hjust = 1, fontface = "italic", size = 7, parse = T) +
  scale_color_manual(values = c("Continuous corn AONR" = clr_cc, 
                                "Rotated corn AONR" = clr_rot)) +
  scale_fill_manual(values = c("Continuous corn AONR" = clr_cc, 
                               "Rotated corn AONR" = clr_rot)) +
  labs(x = expression(Nitrogen~fertilization~rate~(kg~N~ha^{-1})),
       y = yld_lab,
       color = NULL,
       fill = NULL,
       title = "Ames, 2008") +
  scale_x_continuous(limits = c(0, 360)) +
  theme_bw() + 
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(color= "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.text = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)),
        axis.title = element_text(size =rel(1.5)),
        panel.grid = element_blank(),
        plot.title = element_text(size = rel(2)))


ggsave("05_talk-figs/fig_concep2.png", width = 8.73, height = 4.7)


# no aonr diamond ---------------------------------------------------------

ggplot() + 
  geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield/1000, color = rot), size = 2) + 
  # geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield/1000, fill = rot), pch = 23, size = 4, stroke = 2) + 
  #--arrows
  geom_segment(aes(x = 310, xend = 330, 
                   y = sc_1/1000, yend = sc_1/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  geom_segment(aes(x = 310, xend = 330, 
                   y = cc_2/1000, yend = cc_2/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  #--rectangles
  # geom_rect(aes(xmin = 340, xmax = 360,
  #               ymin = cc_2/1000, ymax = sc_2/1000),
  #           fill = clr_red, color = "black") +
  # geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000+0.3, 
  #               label = "Continuous corn penalty"),
  #           hjust = 1, fontface = "italic", size = 7) +
  # geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000-0.3, 
  #               label = paste(expression(2~Mg~ha^-1))),
  #           hjust = 1, fontface = "italic", size = 7, parse = T) +
  scale_color_manual(values = c("Continuous corn AONR" = clr_cc, 
                                "Rotated corn AONR" = clr_rot)) +
  scale_fill_manual(values = c("Continuous corn AONR" = clr_cc, 
                               "Rotated corn AONR" = clr_rot)) +
  labs(x = expression(Nitrogen~fertilization~rate~(kg~N~ha^{-1})),
       y = yld_lab,
       color = NULL,
       fill = NULL,
       title = "Ames, 2008") +
  theme_bw() + 
  scale_x_continuous(limits = c(0, 360)) +
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(color= "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.text = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)),
        axis.title = element_text(size =rel(1.5)),
        panel.grid = element_blank(),
        plot.title = element_text(size = rel(2)))


ggsave("05_talk-figs/fig_concep1-no-aonr.png", width = 8.73, height = 4.7)




# labelled as ames 2008 ---------------------------------------------------------


ggplot() + 
  geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield/1000, color = rot), size = 2) + 
  geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield/1000, fill = rot), pch = 23, size = 4, stroke = 2) + 
  #--arrows
  geom_segment(aes(x = 310, xend = 330, 
                   y = sc_1/1000, yend = sc_1/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  geom_segment(aes(x = 310, xend = 330, 
                   y = cc_2/1000, yend = cc_2/1000), 
               size = 1.2, arrow = arrow(length = unit(0.2,"cm")), color = "gray80") +
  #--rectangles
  geom_rect(aes(xmin = 340, xmax = 360,
                ymin = cc_2/1000, ymax = sc_2/1000),
            fill = clr_red, color = "black") +
  geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000+0.3, 
                label = "Continuous corn penalty"),
            hjust = 1, fontface = "italic", size = 7) +
  geom_text(aes(x = 330, y = ((sc_2 + cc_2)/2)/1000-0.3, 
                label = paste(expression(2~Mg~ha^-1))),
            hjust = 1, fontface = "italic", size = 7, parse = T) +
  scale_color_manual(values = c("Continuous corn AONR" = clr_cc, 
                                "Rotated corn AONR" = clr_rot)) +
  scale_fill_manual(values = c("Continuous corn AONR" = clr_cc, 
                               "Rotated corn AONR" = clr_rot)) +
  labs(x = expression(Nitrogen~fertilization~rate~(kg~N~ha^{-1})),
       y = yld_lab,
       color = NULL,
       fill = NULL,
       title = "Ames, 2008") +
  scale_x_continuous(limits = c(0, 360)) +
  theme_bw() + 
  theme(legend.position = c(0.95, 0.05),
        legend.justification = c(1, 0),
        legend.background = element_rect(color= "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.text = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.4)),
        axis.title = element_text(size =rel(1.5)),
        panel.grid = element_blank(),
        plot.title = element_text(size = rel(2)))


ggsave("05_talk-figs/fig_concep-ames.png", width = 7.26, height = 4.76)

