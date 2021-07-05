# goal: combine results into something useful
# created: 12/8/2020
# updated: 
#
# notes: 

rm(list = ls())

library(tidyverse)
library(saapsim) #--for conversions
library(tidysawyer2) #--for obs data
library(patchwork)


#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

theme_set(theme_bw())


# data --------------------------------------------------------------------

#--misc data
setdesc <- read_csv("../01_create-parameter-grids/01_set-params.csv")

#--sim data
sims <- read_csv("02_ames.csv")

s.gaps <- 
  sims %>% 
  mutate(set_desc = paste(set_id, set_desc2, sep = "-")) %>% 
  select(site, set_id, set_desc, till, rot2, year, corn_buac) %>% 
  pivot_wider(names_from = rot2, values_from = corn_buac) %>% 
  mutate(sim_gap_buac = sc - cc,
         sim_gap_kgha = saf_buac_to_kgha_corn(sim_gap_buac)) 

#--obs data

obs <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  filter(site == "ames") %>% 
  mutate(rot2 = rotation) %>% 
  select(-state, -crop, -nrate_kgha, -rotation)

o.gaps <- 
  obs %>% 
  pivot_wider(names_from = rot2, values_from = yield_kgha) %>% 
  mutate(obs_gap_kgha = sc - cc,
         obs_gap_buac = saf_kgha_to_buac_corn(obs_gap_kgha)) %>% 
  select(site, year, obs_gap_kgha, obs_gap_buac)


# look at gaps ------------------------------------------------------------

s.gaps %>% 
  left_join(o.gaps) %>% 
  select(site, set_id, set_desc, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = as.factor(set_desc)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(.~till) 

s.gaps %>% 
  filter(set_id %in% c(1, 2, 4, 5)) %>% 
  left_join(o.gaps) %>% 
  select(site, set_id, set_desc, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = as.factor(set_desc)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(.~till) 



# investigate no scripts--------------------------------------------------

ames %>% 
  filter(set_id == 1) %>% 
  ggplot(aes(year, ResidueWTatSowing)) + 
  geom_line(aes(color = till)) + 
  facet_grid(.~rot2) + 
  labs(title = "Reducing tillage inc res amount at corn sowing")

ggsave("fig_res-at-sowing.png")

p1 <- 
  ames %>% 
  filter(set_id == 1) %>% 
  filter(rot2 == "cc") %>% 
  ggplot(aes(year, ResidueWTatSowing)) + 
  geom_line(aes(color = till)) + 
  labs(title = "Reducing tillage inc res amount at corn sowing",
       subtitle = "CC only")

#--gaps in no scripts
simgaps %>% 
  filter(set_id == 1) %>% 
  ggplot(aes(year, sim_gap_buac)) + 
  geom_line(aes(color = till)) + 
  labs(title = "Reducing tillage inc gap w/o scripts")

#--is gap related to residue?
p2 <- 
  simgaps %>% 
  filter(set_id == 1) %>% 
  ggplot(aes(year, sim_gap_buac)) + 
  geom_line(aes(color = till)) +
  labs(title = "Reducing tillage inc gap w/o scripts")


p1/p2 +  plot_annotation(
  title = 'Current gaps are not necessarily related to residue')

ggsave("fig_res-at-sowing-vs-gaps.png")

#--how does gap compare to observed gaps?
ames %>% 
  filter(set_id == 1) %>% 
  select(site, till, rot2, year, corn_buac) %>% 
  left_join(ylds) %>% 
  mutate(obs_buac = saf_kgha_to_buac_corn(yield_kgha)) %>% 
  select(-yield_kgha) %>% 
  rename(sim_buac = corn_buac) %>% 
  pivot_longer(sim_buac:obs_buac) %>% 
  pivot_wider(names_from = rot2, values_from = value) %>% 
  mutate(gap_buac = sc - cc) %>% 
  select(-cc, -sc) %>% 
  pivot_wider(names_from = name, values_from = gap_buac) %>% 
  ggplot() +
  geom_line(aes(year, sim_buac, color = till)) + 
  geom_point(aes(year, obs_buac), size = 2) + 
  labs(title = "Even with red tillage, sims are not capturing magnitude of gap",
       y = "Gap, bu/ac")
  
ggsave("fig_noscript-vs-obs-gaps.png")


# investigate kill  ------------------------------------------------------------

#--is the kill script affecting the gap?
k2 <- 
  simgaps %>% 
  #  filter(set_id == 2) %>% 
  left_join(obsgaps) %>% 
  select(site, set_id, set_desc, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = as.factor(set_id)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(.~till)+
  labs(title = "Kill script generally inc gap size",
       subtitle = "But not enough to match obs gap")

k2

#--how many plants are dying each year?
k1 <- 
  ames %>% 
  filter(set_id == 2) %>% 
  select(set_desc, set_id, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
  mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100) %>% 
  ggplot(aes(year, pct_kill)) + 
  geom_line(aes(color = rot2), size = 2) + 
  facet_grid(.~till) + 
  labs(title = "More plants are killed in NT CC compared to SC")

k1


#--is the difference in gap vals from noscripts related to the pact kill?

simgaps %>%  
  select(site, set_desc, till, year, sim_gap_buac) %>% 
  pivot_wider(names_from = set_desc, values_from = sim_gap_buac) %>% 
  janitor::clean_names() %>% 
  mutate(gap_diff = kill - no_scripts) %>% 
  select(-no_scripts, -kill) %>% 
  left_join(
    ames %>% 
      filter(set_id == 2) %>% 
      select(set_desc, set_id, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
      mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100) %>% 
      select(-PlantDensityS, -PlantDensityH)
  ) %>% 
  ggplot(aes(gap_diff, pct_kill)) + 
  geom_point(size = 3) + 
  facet_grid(.~till) + 
  labs(title = "Pct kill is not related to change in gap from no scripts",
       subtitle = ")
  

k3 <- 
  simgaps %>%  
  select(site, set_desc, till, year, sim_gap_buac) %>% 
  pivot_wider(names_from = set_desc, values_from = sim_gap_buac) %>% 
  janitor::clean_names() %>% 
  mutate(gap_diff = kill - no_scripts) %>% 
  ggplot(aes(year, gap_diff)) + 
  geom_col() + 
  facet_grid(.~till) + 
  labs(y = "kill gap - no script gap, bu/ac")
  
  
k1/k3  

k1
  

# sot style fig -----------------------------------------------------------

obs_stats <- 
  obsgaps %>% 
  group_by(site) %>% 
  summarise(gap_mean = mean(obs_gap_buac, na.rm = T),
            gap_sd = sd(obs_gap_buac, na.rm = T)) %>% 
  mutate(dat_type = "obs") %>% 
  ungroup()


sim_stats <- 
  simgaps %>% 
  group_by(site, till, set_id, set_desc) %>% 
  summarise(gap_mean = mean(sim_gap_buac, na.rm = T),
            gap_sd = sd(sim_gap_buac, na.rm = T)) %>% 
  mutate(dat_type = "sim") %>% 
  ungroup()

#--visualize
bind_rows(sim_stats, 
          obs_stats %>% 
            full_join(sim_stats %>% select(site, till))
          ) %>% 
  mutate(sim_id = ifelse(is.na(set_desc), 
                         paste(site, dat_type, sep = "-"),
                         paste(site, dat_type, set_desc, set_id, sep = "-")),
         sim_id = as.factor(sim_id),
         sim_id = fct_reorder(sim_id, gap_mean)) %>% 
  ggplot(aes(sim_id, gap_mean)) + 
  geom_col(aes(fill = set_desc)) +
  facet_grid(.~till) + 
  coord_flip()


  
  