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
ames <- read_csv("02_ames.csv") %>% 
  filter(set_id == 1 | set_id > 99, set_id < 200) %>% 
  mutate(set_desc = paste(set_id, set_desc2, sep = "-"))

nash <- read_csv("02_nash.csv") %>% 
  filter(set_id == 1 | set_id > 99, set_id < 200) %>% 
  mutate(set_desc = paste(set_id, set_desc2, sep = "-"))

sims <- 
  bind_rows(ames, nash)

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


# what is TTsum? ----------------------------------------------------------

sims %>% 
  filter(set_id %in% c(100, 106)) %>% 
  ggplot(aes(TTsum)) + 
  geom_histogram() + 
  labs(title = "TTsum is never hitting 15") +
  facet_wrap(~set_id, scales = "free")

sims %>% 
  filter(set_id %in% c(106)) %>% 
  ggplot(aes(TTsum)) + 
  geom_histogram() + 
  labs(title = "Plants usually get at least 4 GDDs",
       subtitle = "Max GDDs is 26",
       x = "Sum of GDDs < 10, planting-V4ish") 

# what is the distribution of GDDs in a typical year?
ilia_wea %>% 
  mutate(tmax_c2 = ifelse(tmax_c > 30, 30, tmax_c),
         tmax_c2 = ifelse(tmax_c < 10, 10, tmax_c2),
         tmin_c2 = ifelse(tmin_c < 10, 10, tmin_c),
         tav = (tmax_c2 + tmin_c2)/2,
         gdd = tav - 10) %>% 
  ggplot(aes(day, gdd)) + 
  geom_point(alpha = 0.5) + 
  facet_grid(.~state)

ilia_wea %>% 
  mutate(tmax_c2 = ifelse(tmax_c > 30, 30, tmax_c),
         tmax_c2 = ifelse(tmax_c < 10, 10, tmax_c2),
         tmin_c2 = ifelse(tmin_c < 10, 10, tmin_c),
         tav = (tmax_c2 + tmin_c2)/2,
         gdd = tav - 10) %>% 
  filter(gdd == max(gdd))

saf_date_to_doy("2001-06-15")
#--iowa planting
ia_wea %>% 
  left_join(ia_planting) %>% 
  filter( day > plant_doy, day < 166) %>% 
  mutate(tmax_c2 = ifelse(tmax_c > 30, 30, tmax_c),
         tmax_c2 = ifelse(tmax_c < 10, 10, tmax_c2),
         tmin_c2 = ifelse(tmin_c < 10, 10, tmin_c),
         tav = (tmax_c2 + tmin_c2)/2,
         gdd = tav - 10) %>% 
  ggplot(aes(gdd)) +
  geom_density(fill = "skyblue") + 
  geom_vline(xintercept = 4) +
  labs(title = "AMES Daily GDDs from planting - June 15")+
  facet_wrap(~year)
           
# look at raw yields ------------------------------------------------------

sims %>% 
  select(set_desc, site, rot2, year, corn_buac, till) %>% 
  ggplot(aes(year, corn_buac)) + 
  geom_line(aes(color = set_desc)) + 
  geom_point(data = obs %>% 
               mutate(corn_buac = saf_kgha_to_buac_corn(yield_kgha)),
             aes(year, corn_buac)) +
  facet_grid(rot2~till)

sims %>% 
  filter(set_id == 1) %>% 
  select(set_desc, site, rot2, year, corn_buac, till) %>% 
  left_join(obs) %>% 
  mutate(obs_buac = saf_kgha_to_buac_corn(yield_kgha)) %>% 
  select(-yield_kgha) %>% 
  pivot_longer(cols = c(corn_buac, obs_buac)) %>% 
  ggplot(aes(year, value)) + 
  geom_line(aes(color = rot2, group = interaction(rot2, set_desc)), size = 2) +
  facet_grid(name~till) +
  scale_color_manual(values = c("cc" = "gold2", "sc" = "green4")) +
  labs(title = "Apsim w/o scripts",
       subtitle = "Consistently under-predicts continuous corn penalty")

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
  filter(set_id %in% c(1, 101, 102, 103)) %>% 
  left_join(o.gaps) %>% 
  select(site, set_id, set_desc, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = as.factor(set_desc)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(.~till) +
  labs(title = "RUE penalties")

s.gaps %>% 
  filter(set_id %in% c(1, 101, 104, 105)) %>% 
  left_join(o.gaps) %>% 
  select(site, set_id, set_desc, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = as.factor(set_desc)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(.~till) +
  labs(title = "Residue RUE penalties")


# investigate rue  ------------------------------------------------------------

#--is the rue script affecting the gap, compared to no scripts?

s.gaps %>% 
  filter(set_id == 1|set_id  == 100) %>% 
  left_join(o.gaps) %>% 
  select(site, set_id, set_desc, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = as.factor(set_desc)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(.~till) 



#--what is the 'average' soil water 14 days before - sowing
sims %>% 
  filter(set_id == 1) %>% 
  select(set_desc, till, rot2, year, SWSowing) %>% 
  ggplot() + 
  geom_point(aes(year, SWSowing, color = rot2)) + 
  geom_hline(yintercept = 0.31, linetype = "dashed") +
  geom_hline(yintercept = 0.445, linetype = "dashed") +
  facet_grid(. ~ till) + 
  labs(title = "14-day average is always above field capacity")
  
#--is the obs gap related to this soil moisture?

sims %>% 
  filter(set_id == 1, till == "normal", rot2 == "cc") %>% 
  select(set_desc, till, rot2, year, SWSowing, ResidueWTatSowing) %>% 
  left_join(o.gaps) %>% 
  ggplot(aes(SWSowing, obs_gap_kgha, size = ResidueWTatSowing)) + 
  geom_point() + 
  labs("14-day avg is not directly related to obs gap in Ames")
