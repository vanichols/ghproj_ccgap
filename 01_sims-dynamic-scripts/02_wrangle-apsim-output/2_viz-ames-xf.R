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
sims <- read_csv("02_ames.csv") %>% 
  filter(set_id == 1 | set_id > 199) %>% 
  mutate(set_desc = paste(set_id, set_desc2, sep = "-"))

s.gaps <- 
  sims %>% 
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


# look at raw yields ------------------------------------------------------

sims %>% 
  select(set_desc, site, rot2, year, corn_buac, till) %>% 
  ggplot(aes(year, corn_buac)) + 
  geom_line(aes(color = set_desc)) + 
  geom_point(data = obs %>% 
               mutate(corn_buac = saf_kgha_to_buac_corn(yield_kgha)),
             aes(year, corn_buac)) +
  facet_grid(rot2~till) + 
  labs(title = "XF scripts not having any effect")

#--no effect of script using defaults
sims %>% 
  filter(set_id %in% c(1, 200)) %>% 
  select(set_desc, site, rot2, year, corn_buac, till) %>% 
  ggplot(aes(year, corn_buac)) + 
  geom_line(aes(color = set_desc)) + 
  geom_point(data = obs %>% 
               mutate(corn_buac = saf_kgha_to_buac_corn(yield_kgha)),
             aes(year, corn_buac)) +
  facet_grid(rot2~till) + 
  labs(title = "XF scripts not having any effect at default level")

#--cold penalty never gets triggered...
sims %>% 
  filter(set_id %in% c(1, 205, 206)) %>% 
  select(set_desc, site, rot2, year, corn_buac, till) %>% 
  ggplot(aes(year, corn_buac)) + 
  geom_line(aes(color = set_desc)) + 
  geom_point(data = obs %>% 
               mutate(corn_buac = saf_kgha_to_buac_corn(yield_kgha)),
             aes(year, corn_buac)) +
  facet_grid(rot2~till) + 
  labs(title = "Cold penalty never gets triggered at 4 deg")

#--mois triggering? No. 
sims %>% 
  filter(set_id %in% c(1, 207, 208)) %>% 
  select(set_desc, site, rot2, year, corn_buac, till) %>% 
  ggplot(aes(year, corn_buac)) + 
  geom_line(aes(color = set_desc)) + 
  geom_point(data = obs %>% 
               mutate(corn_buac = saf_kgha_to_buac_corn(yield_kgha)),
             aes(year, corn_buac)) +
  facet_grid(rot2~till) + 
  labs(title = "Mois pen not doing much")

#--Is it bc xf doesn't do anything?
# NOTE: the sc might not be matching bc I changed the opt_temp in the soil xml to 32 (from 28)
# need to rerun everything
sims %>% 
  filter(set_id %in% c(1, 209)) %>% 
  select(set_desc, site, rot2, year, corn_buac, till) %>% 
  ggplot(aes(year, corn_buac)) + 
  geom_line(aes(color = set_desc)) + 
  geom_point(data = obs %>% 
               mutate(corn_buac = saf_kgha_to_buac_corn(yield_kgha)),
             aes(year, corn_buac)) +
  facet_grid(rot2~till) + 
  labs(title = "XF has an effect in some years")


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
  filter(set_id %in% c(1, 201, 202, 203, 204, 205, 206)) %>% 
  left_join(o.gaps) %>% 
  select(site, set_id, set_desc, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = as.factor(set_desc)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(.~till) +
  labs(title = "Residue XF penalties",
       subtitle = "Basically nothing is happening unless we raise cold to 10deg")


# investigate xf  ------------------------------------------------------------

#--what is the root depth each year?
sims %>% 
  filter(till == "normal") %>% 
  select(set_desc, till, rot2, year, RootDepth) %>% 
  ggplot(aes(year, RootDepth, group = interaction(set_desc, rot2))) + 
  geom_line(aes(color = rot2), size = 2) + 
  facet_wrap(~set_desc, scales = "free") + 
  labs(title = "dd") + 
  scale_y_reverse()

#--overlay them
sims %>% 
  filter(till == "normal", set_id != 1) %>% 
  select(set_desc, till, rot2, year, RootDepth) %>% 
  ggplot(aes(year, RootDepth, group = interaction(set_desc, rot2))) + 
  geom_line(aes(color = set_desc), size = 2) + 
  facet_wrap(~rot2, scales = "free") + 
  labs(title = "Root depth is in general not affected unless we count more cold days") + 
  scale_y_reverse()


#--dry penalty isn't doing much
sims %>% 
  filter(set_id %in% c(2, 4)) %>% 
  select(set_desc, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
  mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100) %>% 
  ggplot(aes(year, pct_kill, group = interaction(set_desc, rot2))) + 
  geom_line(aes(color = set_desc), size = 2) + 
  geom_hline(yintercept = 5, linetype = "dashed") +
  facet_grid(rot2~till, scales = "free") + 
  labs(title = "Dry kill only affects rotated corn")

ggsave("fig_kill-dry-only-affects-rotC.png")


#--is the temp kill doing anything?
sims %>% 
  filter(set_id %in% c(2, 6)) %>% 
  select(set_desc, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
  mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100) %>% 
  ggplot(aes(year, pct_kill, group = interaction(set_desc, rot2))) + 
  geom_line(aes(color = set_desc), size = 2) + 
  geom_hline(yintercept = 5, linetype = "dashed") +
  facet_grid(rot2~till, scales = "free") + 
  labs(title = "2012 was a temperature kill?")

sims %>% 
  select(set_desc) %>% 
  distinct()

sims %>% 
  #filter(rot2 == "sc") %>% 
  filter(set_id %in% c(2, 7, 8, 10)) %>%
  select(set_desc, till, rot2, year, PlantDensityS, PlantDensityH) %>% 
  mutate(pct_kill = (PlantDensityS - PlantDensityH)/PlantDensityS * 100,
         set_desc = as.factor(set_desc),
         set_desc = fct_reorder(set_desc, pct_kill, mean),
         set_desc = fct_rev(set_desc)) %>% 
  ggplot(aes(year, pct_kill, group = interaction(rot2, set_desc))) + 
  geom_line(aes(color = rot2), size = 2) + 
  geom_hline(yintercept = 5, linetype = "dashed") +
  scale_color_manual(values = c("cc" = "gold2", "sc" = "green4")) +
  facet_grid(set_desc~till) + 
  labs(title = "AMES - Wet kill most influential, strongly responds to residue",
       subtitle = "Cold kill only active in 2004, dry kill only active for rotated corn",
       x = NULL,
       y = "Percentage of plants killed (%)") + 
  theme(strip.text.y = element_text(angle = 0))

ggsave("fig_kill-component-effects.png")


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


  
  