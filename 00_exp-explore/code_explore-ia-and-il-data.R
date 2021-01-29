# Created:       5/20/2020
# last edited:   
# 
# purpose: diagnose john sawyer and emmerson's data
#
# notes: 

rm(list = ls())
library(tidysawyer2) #--saw_xx data, il_xx data
library(tidyverse)



# data --------------------------------------------------------------------

cgap <- ilia_gaps %>% filter(year > 2000, gap_kgha > -1000)
ylds <- ilia_yields %>% filter(year > 2000)


# look at things ----------------------------------------------------------


cgap %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept = 0) +
  facet_grid(.~state, scales = "free")


ylds %>% 
  ggplot(aes(nrate_kgha, yield_kgha, color = site, linetype = rotation, 
             group = interaction(year, site, rotation))) + 
  geom_point() + 
  geom_line() + 
  facet_grid(state ~ .)

ylds %>% 
 
  ggplot(aes(year, yield_kgha, color = site, linetype = rotation, 
             group = interaction(nrate_kgha, site, rotation))) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method = "lm", se = F, color = "black", aes(group = state)) +
  guides(color = F) +
  facet_wrap(~state, ncol = 2, scales = "free")

ylds %>%
  filter(nrate_kgha != 67.38, nrate_kgha != 202.14) %>%
  ggplot(aes(year, yield_kgha/1000, color = site, linetype = rotation, 
             group = interaction(nrate_kgha, site, rotation))) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method = "lm", se = F, color = "black", aes(group = state)) +
  guides(color = F) +
  facet_grid(rotation ~ state + nrate_kgha, scales = "free")

ggsave("00_exp-explore/fig_yields-over-time.png")
