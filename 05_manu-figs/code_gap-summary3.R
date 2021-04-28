# Created:    april 2 2021
#
# purpose: make gap over time fig
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

source("05_manu-figs/palettes.R")

#--need to think about this more. SHould have windmill of non-N gaps. Maybe?

# 4/26/2021 trying taht


# gap driven by cc yields -------------------------------------------------


maxsc <- 
  ilia_yields %>% 
  filter(rotation == "sc") %>% 
  group_by(state, site, year) %>% 
  filter(yield_kgha == max(yield_kgha)) %>% 
  select(state, site, year, yield_kgha) %>% 
  rename("scyield_kgha" = yield_kgha)

maxcc <- 
  ilia_yields %>% 
  filter(rotation == "cc") %>% 
  group_by(state, site, year) %>% 
  filter(yield_kgha == max(yield_kgha)) %>% 
  select(state, site, year, yield_kgha) %>% 
  rename("ccyield_kgha" = yield_kgha)


nonngap <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap) %>% 
  left_join(maxsc) %>% 
  mutate(ogap_pct = nonngap/scyield_kgha, 
         state = 
           case_when(
             grepl("IA", state) ~ "Iowa",
             grepl("IL", state) ~ "Illinois"
           )) %>% 
  filter(!is.na(ogap_pct))


mn_ogap <- 
  nonngap %>% 
  filter(ogap_pct >= 0) %>% 
  summarise(mean_ogap = mean(ogap_pct)) %>% 
  pull(mean_ogap)


thng <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap) %>% 
  left_join(maxsc) %>% 
  left_join(maxcc)


thng %>% 
  pivot_longer(scyield_kgha:ccyield_kgha) %>% 
  ggplot(aes(value, nonngap)) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  facet_grid(.~name)


f_drive <- 
  thng %>% 
  pivot_longer(scyield_kgha:ccyield_kgha) %>% 
  rename("rotation" = name) %>% 
  mutate(rot_nice = case_when(
    grepl("sc", rotation) ~ "Rotated maize",
    grepl("cc", rotation) ~ "Continuous maize"),
    #grepl("gap", rotation) ~ "Continuous maize penalty"),
    rot_nice = factor(rot_nice, levels = c("Rotated maize", "Continuous maize"))
  ) %>% 
  ggplot(aes(value/1000, nonngap/1000)) + 
  #geom_point(aes(color = rot_nice, shape = rot_nice, stroke = 1.1)) + 
  geom_point(aes(color = rot_nice, 
                 shape = rot_nice, stroke = 1.1)) + 
  geom_smooth(method = "lm", se = F, color = "black") +
  guides(shape = F,
         color = F) +
  labs(y = "Continuous maize\nyield penalty\n(dry Mg ha-1)",
       x = "Maize grain yield (dry Mg ha-1)") +
  scale_color_manual(values = c("Continuous maize" = pnk1, 
                                "Rotated maize" = dkbl1)) +
  scale_shape_manual(values = c("Continuous maize" = 17, 
                                "Rotated maize" = 19)) +
  facet_grid(. ~ rot_nice) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5), 
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)))


f_drive


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
         rot = factor(rot, levels = c("Rotated maize", "Continuous maize"))) %>% 
  filter(scen != "Penalty driven by both")

f_conc <- 
  dat %>% 
  ggplot(aes(scen, yield)) + 
  geom_col(aes(fill = rot, alpha = scen), 
           position = position_dodge(width = 0.5), 
           width = 0.5,
           color = "black") + 
  geom_hline(yintercept = 10, color = "black", size = 1.2, linetype = "dashed") + 
  geom_segment(x = 0.875, xend = 0.875, 
               y = 10, yend = 12,
               arrow = arrow(length = unit(0.5,"cm")),
               color = ylw1, size = 2, alpha = 0.4) +
  geom_segment(x = 2.125, xend = 2.125, 
               y = 10, yend = 8,
               arrow = arrow(length = unit(0.5,"cm")),
               color = ylw1, size = 2) +
  scale_fill_manual(values = c("Rotated maize" = dkbl1,
                               "Continuous maize" = pnk1)) + 
  scale_alpha_manual(values = c(0.4, 1)) +
  guides(fill = F, alpha = F) +
  labs(x = NULL,
       fill = NULL,
       y = "Maize grain yield\n(Mg ha-1)") + 
  theme(legend.position = "top",
        axis.title.y = element_text(angle = 0, vjust = 0.5), 
        axis.title = element_text(size = rel(1.1)),
        axis.text.x = element_text(size = rel(1.1)))

f_conc
ggsave("05_manu-figs/fig_conceptual-penalty.png")


# gaps and cc driving them ------------------------------------------------


f_drive / f_conc + plot_layout(heights = c(1, 1.5))

ggsave("05_manu-figs/fig_conc-drivers.png")
