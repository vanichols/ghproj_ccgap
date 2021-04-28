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

f2 <- 
  nonngap %>% 
  arrange(-ogap_pct) %>% 
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, ogap_pct)) +
  geom_col(aes(fill = state)) +
  #geom_segment(aes(color = state, x = n, xend = n, y = 0, yend = ogap_pct)) +
  #geom_point(aes(color = state), size = 3) +
  geom_hline(yintercept = mn_ogap, linetype = "dashed") +
  geom_text(x = 125, y = 0.14, label = "Mean non-N yield reduction of 10%", check_overlap = T, hjust = 1, fontface = "italic") +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Site year",
       y = "Continuous maize\nyield penalty\n(% rotated maize\ngrain yield)",
       color = NULL,
       fill = NULL) +
  scale_fill_manual(values = c("Iowa" = ylw1,
                               "Illinois" = grn1)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        #axis.text.x = element_blank(),
        legend.justification = c(1, 1),
        legend.position = c(0.95, 0.95),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "black"))


f2




# gap driven by cc yields -------------------------------------------------




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


f3 <- 
  thng %>% 
  pivot_longer(scyield_kgha:ccyield_kgha) %>% 
  rename("rotation" = name) %>% 
  mutate(rot_nice = case_when(
    grepl("sc", rotation) ~ "Rotated maize",
    grepl("cc", rotation) ~ "Continuous maize"),
    #grepl("gap", rotation) ~ "Continuous maize penalty"),
    rot_nice = factor(rot_nice, levels = c("Continuous maize", "Rotated maize"))
  ) %>% 
  ggplot(aes(value/1000, nonngap/1000)) + 
  geom_point(aes(color = rot_nice, shape = rot_nice, stroke = 1.1)) + 
  geom_smooth(method = "lm", se = F, color = "black") +
  guides(shape = F,
         color = F) +
  labs(y = "Continuous maize\nyield penalty\n(dry Mg ha-1)",
       x = "Maize grain yield\n(dry Mg ha-1)") +
  scale_color_manual(values = c("Continuous maize" = pnk1, 
                                "Rotated maize" = dkbl1)) +
  scale_shape_manual(values = c("Continuous maize" = 24, 
                                "Rotated maize" = 21)) +
  facet_grid(. ~ rot_nice) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5), 
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)))


f3



# gaps and cc driving them ------------------------------------------------


f2 / f3 + plot_layout(heights = c(1.2, 1))

ggsave("05_manu-figs/fig_gap-summary.png")
