# Created:       6/5/2020
# last edited:    1/13/2021
# 
# purpose: Look at tillage x rotation effects
#
# notes: 


rm(list = ls())
library(tidyverse)
library(readxl)
library(janitor)


# dat ---------------------------------------------------------------------

dat <- read_excel("00-lit-explore/alkaisi-2015-AJ-tillage-rot.xlsx")

fig_dat <- 
  dat %>%
  pivot_longer(notill:moldboard, names_to = "tillage", values_to = "yield") %>%
  pivot_wider(names_from = rotation, values_from = yield) %>% 
  mutate(gap_kgha = sc - cc,
         gap_pct = gap_kgha/sc,
         tillage = factor(tillage, levels = c("notill", "striptill", "chisel", "deeprip", "moldboard")),
         tillage2 = as.numeric(tillage)) 

# viz ---------------------------------------------------------------------

fig_dat %>%
#  filter(site != "mcna") %>% 
  ggplot(aes(tillage2, gap_kgha)) + 
  geom_line(aes(color = site, group = site), alpha= 0.5, size = 2) + 
  stat_summary(fun = "mean", geom = "line",  size = 4) +
  scale_x_continuous(labels = c("notill", "striptill", "chisel", "deeprip", "moldboard")) +
  labs(y = "Contin. Corn Penalty (Mg/ha)",
       title = "Higher tillage intensity relieves penalty?",
       subtitle = "Not really. Data from Al-Kaisi et al. 2015 table 4 ",
       x = "Tillage intensity (low to high)")

ggsave("00-lit-explore/fig_alkaisi-tillage-absolute.png")

fig_dat %>%
    #filter(site != "mcna") %>% 
  ggplot(aes(tillage2, gap_pct)) + 
  geom_line(aes(color = site, group = site), alpha= 0.5, size = 2) + 
  stat_summary(fun = "mean", geom = "line",  size = 4) +
  scale_x_continuous(labels = c("No-till", "Strip-till", "Chisel", "Deep rip", "Moldboard")) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(y = "Continuous maize penalty (%)",
       title = "Higher tillage intensity relieves penalty?",
       subtitle = "Inconclusive. Data from Al Kaisi et al. 2015",
       x = "Tillage intensity (low to high)") + 
  theme_bw()

ggsave("00-lit-explore/fig_alkaisi-tillage-pct.png")
