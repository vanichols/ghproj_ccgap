# Created:       6/5/2020
# last edited:   
# 
# purpose: Look at tillage x rotation effects
#
# notes: 


rm(list = ls())
library(tidyverse)
library(readxl)
library(janitor)


# dat ---------------------------------------------------------------------

dat <- read_excel("00_exp-explore/alkaisi-2015-AJ-tillage-rot.xlsx")



# viz ---------------------------------------------------------------------

p1 <- 
  dat %>%
  pivot_longer(notill:moldboard, names_to = "tillage", values_to = "yield") %>%
  pivot_wider(names_from = rotation, values_from = yield) %>% 
  mutate(ccpen = sc - cc,
         tillage = factor(tillage, levels = c("notill", "striptill", "chisel", "deeprip", "moldboard"))) %>% 
  ggplot(aes(tillage, ccpen, group = site)) + 
  geom_point(aes(color = site), size = 1) +
  geom_line(aes(color = site)) + 
  stat_summary(fun = "mean", geom = "point", aes(group = tillage), size = 4) +
  coord_flip() + 
  labs(y = "Contin. Corn Penalty (Mg/ha)",
       title = "Penalty is lowest with moldboard plowing\nMcNay is weird\nBut why do intermediate tillages have higher penalties than no-till?\nN-rates are moderate (190 kg/ha)")

p2 <- 
  dat %>%
  filter(site != "mcna") %>% 
  pivot_longer(notill:moldboard, names_to = "tillage", values_to = "yield") %>%
  pivot_wider(names_from = rotation, values_from = yield) %>% 
  mutate(ccpen = sc - cc,
         tillage = factor(tillage, levels = c("notill", "striptill", "chisel", "deeprip", "moldboard"))) %>% 
  ggplot(aes(tillage, ccpen, group = site)) + 
  geom_point(aes(color = site), size = 1) +
  geom_line(aes(color = site)) + 
  stat_summary(fun = "mean", geom = "point", aes(group = tillage), size = 4) +
  coord_flip() + 
  labs(y = "Contin. Corn Penalty (Mg/ha)",
       title = "Pattern is\nmore dramatic if we exclude McNay")

library(patchwork)
p1 / p2

ggsave("00_exp-explore/fig_alkaisi-tillage.png", height = 7, width = 5)
