# Created:       feb 5 2021
# last edited:
#
# purpose: answer question about % of obs gap that is n-related?
#
# notes:


rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2")
library(tidysawyer2)
library(tidyverse)
library(saapsim)

library(scales)

theme_set(theme_bw())


# data prep ---------------------------------------------------------------

ia_gap <-
  ia_yields %>%
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>%
  mutate(ogap_kgha = sc - cc,
         nrate_kgha = round(nrate_kgha, 0),
         nrate_kgha = case_when(
           nrate_kgha == 67 ~ 68,
           nrate_kgha == 202 ~ 203,
           TRUE ~ nrate_kgha
         )) %>%
  filter(!is.na(ogap_kgha)) %>%
  select(site, year, nrate_kgha, ogap_kgha)

apsim_gap <-
  saw_tidyapsim %>%
  select(-rotation) %>%
  pivot_wider(names_from = rotation2, values_from = yield_kgha) %>%
  mutate(sgap_kgha = sc - cc) %>%
  select(site, year, nrate_kgha, sgap_kgha)

dat_gap <- 
  ia_gap %>% 
  left_join(apsim_gap) %>% 
  filter(!is.na(sgap_kgha))

# viz ---------------------------------------------------------------------

dat_gap %>% 
  pivot_longer(ogap_kgha:sgap_kgha) %>% 
  group_by(nrate_kgha, site, name) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  ggplot(aes(nrate_kgha, value, color = name)) + 
  geom_point() + 
  facet_wrap(~site)


dat_gap %>% 
  pivot_longer(ogap_kgha:sgap_kgha) %>% 
  group_by(nrate_kgha, site, name) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(sim_pct_of_obs = 100-(ogap_kgha - sgap_kgha)/ogap_kgha * 100,
         sim_pct_of_obs2 = ifelse(sim_pct_of_obs < 0, 0, sim_pct_of_obs)) %>% 
  ggplot(aes(nrate_kgha, sim_pct_of_obs2)) + 
  geom_col() + 
  facet_wrap(~site) + 
  coord_cartesian(ylim = c(0, 100)) + 
  labs(title = "Percentage of gap due to nitrogen deficiency")
  

