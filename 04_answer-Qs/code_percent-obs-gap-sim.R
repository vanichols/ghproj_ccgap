# Created:       feb 5 2021
# last edited:   
#
# purpose: answer question about % of obs gap that is n-related?
#
# notes: 


rm(list = ls())
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(scales)

theme_set(theme_bw())


# data --------------------------------------------------------------------

sgap <- 
  saw_tidyapsim %>% 
  select(-rotation) %>% 
  pivot_wider(names_from = rotation2, values_from = yield_kgha) %>% 
  mutate(sgap = sc - cc) %>% 
  select(-cc, -sc)


gaps <- 
  ia_yields %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
    mutate(ogap = sc - cc) %>% 
    mutate(nrate_kgha = round(nrate_kgha, 0),
           nrate_kgha = case_when(
             nrate_kgha == 67 ~ 68,
             nrate_kgha == 202 ~ 203,
             TRUE ~ nrate_kgha)
           ) %>% 
    left_join(sgap) %>% 
    filter(!is.na(ogap)) %>% 
    select(-cc, -sc) %>% 
    mutate(
      sgap = ifelse(sgap < 0, 0, sgap),
      sim_pct_obs = sgap/ogap * 100,
           sim_pct_obs2 = ifelse(sim_pct_obs < 0, 0, sim_pct_obs))


# viz ---------------------------------------------------------------------

gaps %>% 
  select(-contains("sim")) %>% 
  pivot_longer(ogap:sgap) %>% 
  ggplot(aes(nrate_kgha, value, color = name)) + 
  geom_point() + 
  facet_wrap(~site)

gaps %>% 
  group_by(site, nrate_kgha) %>% 
  summarise(sim_pct_obs2 = mean(sim_pct_obs2, na.rm = T)) %>% 
  ggplot(aes(nrate_kgha, sim_pct_obs2)) + 
  geom_col() +
  facet_wrap(~site) +
  coord_cartesian(ylim = c(0, 100)) + 
  labs(title = "Percentage of gap due to N limitation",
       subtitle = "If you believe Apsim captures N limitation")

ggsave("04_answer-Qs/fig_sim-pct-of-obs.png")
