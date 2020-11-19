# Created:       nov 16 2020
# last edited:   
#
# purpose: compare obs and simulated data
#
# notes: 


rm(list = ls())
#devtools::install_github("wilkelab/ungeviz")
library(tidysawyer2) 
library(tidyverse)
library(ungeviz)


obs_ylds <- 
  ilia_yields %>% 
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rot2 = ifelse(rotation == "cs", "sc", rotation),
         datatype = "obs")

sim_ylds <- 
  saw_tidyapsim %>%
  mutate(state = "IA") %>% 
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rot2 = ifelse(rotation == "cs", "sc", rotation),
         datatype = "sim")

ylds <- 
  obs_ylds %>% 
  bind_rows(sim_ylds) %>% 
  mutate(datatype = factor(datatype, levels = c("sim", "obs")),
         rot2 = factor(rot2, levels = c("sc", "cc"))) %>% 
  group_by(state, rot2, datatype) %>% 
  mutate(yield_mean = mean(yield_kgha, na.rm = T))
  
ylds_mn <- 
  ylds %>% 
  select(rot2, yield_mean) %>% 
  distinct() %>% 
  pivot_wider(names_from = rot2, values_from = yield_mean)

ylds %>%
  ggplot(aes(rot2, yield_kgha)) +
  geom_violin(aes(fill = state), alpha = 0.2) +
  geom_hpline(width = 0.05, size = 0.5, color = "gray20") +
  geom_hpline(aes(rot2, yield_mean), width = 0.2, size = 3, color = "red3") +
  geom_hpline(aes(rot2, yield_mean), width = 0.2, size = 3, color = "red3") +
  geom_segment(data = ylds_mn, x = 1.5, xend = 1.5, 
               aes(y = sc, yend = cc), arrow = arrow(length = unit(0.2, "cm")),
               size = 2) +
  guides(fill = F) +
  scale_fill_manual(values = c("orange2", "lightblue2")) +
  labs(y = "Corn Yields (kg/ha)",
       x = NULL,
       title = "Continuous Corn Penalty\nat High (>250 kg/ha) Nitrogen Rates") +
  facet_grid(state~datatype) + 
  theme_bw()

ggsave("00_exp-explore/fig_obs-vs-sim.png", height = 10, width = 7)

