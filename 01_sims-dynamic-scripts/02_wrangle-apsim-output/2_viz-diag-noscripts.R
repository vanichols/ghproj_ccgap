# goal: combine results into something useful
# created: 12/8/2020
# updated: 12/10/2020 added nashua no scripts
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
set_id_key_full <- read_csv("../01_create-parameter-grids/01_set-params-round2.csv") %>% 
  mutate(set_scrcat = paste(set_script, set_pcat, sep = "-"))

set_id_key <- 
  set_id_key_full %>%
  select(set_id, set_script, set_pdes) %>% 
  distinct()

#--sim data
simsraw <- read_csv("02_sims.csv")

sims <- 
  simsraw %>%
  filter(till == "normal") %>% 
  left_join(set_id_key)

s.gaps <- read_csv("02_sims-gaps.csv") %>% left_join(set_id_key)

#--obs data

obs <- read_csv("02_obs.csv") %>%  
  filter(site %in% c("ames", "nash")) 

o.gaps <- read_csv("02_obs-gaps.csv")


# yields ------------------------------------------------------------------

sims %>% 
  filter(set_id == 1) %>% 
  select(set_pdes, site, rot2, year, corn_buac, till) %>% 
  left_join(obs) %>% 
  mutate(obs_buac = saf_kgha_to_buac_corn(yield_kgha)) %>% 
  select(-yield_kgha) %>% 
  pivot_longer(cols = c(corn_buac, obs_buac)) %>% 
  ggplot(aes(year, value)) + 
  geom_line(aes(color = rot2, group = interaction(rot2, set_pdes),
                linetype = till), size = 2) +
  facet_grid(site ~ name) +
  scale_color_manual(values = c("cc" = "orange2", "sc" = "green4")) +
  labs(title = "Apsim w/o scripts (left) and observed (right)",
       subtitle = "Apsim consistently under-predicts continuous corn penalty",
       y = "Corn yields, bu/ac")

ggsave("fig_diag_noscript-vs-obs-yields.png")

#--how does gap compare to observed gaps?
s.gaps %>% 
  filter(set_id == 1) %>% 
  left_join(o.gaps) %>% 
  select(site, set_id, set_pdes, till, year, sim_gap_buac, obs_gap_buac) %>% 
  pivot_longer(sim_gap_buac:obs_gap_buac) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot() +
  geom_line(aes(year, sim_gap_buac, color = (set_pdes)), size = 2) + 
  geom_line(aes(year, obs_gap_buac), linetype = "dashed") + 
  facet_grid(.~till+site)+
  labs(title = "Even with reduced tillage, sims are not capturing magnitude of gap",
       y = "Gap, bu/ac")

ggsave("fig_diag_noscript-vs-obs-gaps.png")


# stresses ----------------------------------------------------------------


sims %>% 
  filter(set_id == 1) %>% 
  select(site, year, rot2, till, contains("stress")) %>% 
  pivot_longer(5:ncol(.)) %>% 
  mutate(stress = 2 - value) %>% 
  ggplot(aes(year, stress)) + 
  geom_line(aes(color = rot2), size = 1) + 
  facet_grid(till~name+site) +
  scale_color_manual(values = c("cc" = "orange2", "sc" = "green4")) +
  labs(title = "Stresses w/o scripts, NASH CC is water and N stressed compared to SC",
       subtitle = "I did 2 - stress")

ggsave("fig_diag_stresses.png")


# residue -----------------------------------------------------------------

sims %>% 
  filter(set_id == 1) %>% 
  ggplot(aes(year, ResidueWTatSowing)) + 
  geom_line(aes(color = rot2)) + 
  facet_grid(.~till+site) + 
  scale_color_manual(values = c("cc" = "orange2", "sc" = "green4")) +
  labs(title = "Reducing tillage inc res amount at corn sowing",
       subtitle = "NASH had very high residue in 2014")

ggsave("fig_diag_res-at-sowing.png")

#--is sim gap related to residue?
s.gaps %>% 
  filter(set_id == 1) %>%
  select(site, year, till, sim_gap_buac) %>% 
  left_join(sims %>% 
              filter(set_id == 1, rot2 == "cc") %>% 
              select(site, year, rot2, till, ResidueWTatSowing)) %>%
  pivot_longer(cols = c(sim_gap_buac, ResidueWTatSowing)) %>% 
  ggplot(aes(year, value)) + 
  geom_line(aes(color = till)) +
  facet_grid(name~site, scales = "free_y") +
  labs(title = "Simulated gap maybe loosely related to residue amount")


#--is obs gap related to residue?
o.gaps %>% 
  select(site, year, obs_gap_buac) %>% 
  left_join(sims %>% 
              filter(set_id == 1, rot2 == "cc", till == "normal") %>% 
              select(site, year, rot2, till, ResidueWTatSowing)) %>%
  mutate(obs_gap_buac = scale(obs_gap_buac),
         ResidueWTatSowing = scale(ResidueWTatSowing)) %>% 
  pivot_longer(cols = c(obs_gap_buac, ResidueWTatSowing)) %>% 
  ggplot(aes(year, value)) + 
  geom_line(aes(color = name)) +
  facet_wrap(~site, scales = "free_x") +
  labs(title = "Observed gap related to residue amount",
       subtitle = "AMES strong relationship? NASH not so much")

o.gaps %>% 
  select(site, year, obs_gap_buac) %>% 
  left_join(sims %>% 
              filter(set_id == 1, rot2 == "cc", till == "normal") %>% 
              select(site, year, rot2, till, ResidueWTatSowing)) %>%
  filter(!is.na(ResidueWTatSowing)) %>%  
  ggplot(aes(ResidueWTatSowing, obs_gap_buac)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~site, scales = "free_x") +
  labs(title = "Observed gap related to residue amount",
       subtitle = "AMES has relationship? NASH not so much")

ggsave("fig_diag_obsgap-vs-resamt.png")

#--try 1:1 plot
o.gaps %>% 
  select(site, year, obs_gap_buac) %>% 
  left_join(sims %>% 
              filter(set_id == 1, rot2 == "cc", till == "normal") %>% 
              select(site, year, rot2, till, ResidueWTatSowing)) %>%
  mutate(obs_gap_buac = scale(obs_gap_buac),
         ResidueWTatSowing = scale(ResidueWTatSowing)) %>%  
  ggplot(aes(obs_gap_buac, ResidueWTatSowing)) + 
  geom_point() +
  geom_abline() +
  facet_wrap(~site, scales = "free_x") +
  labs(title = "Observed gap related to residue amount",
       subtitle = "AMES strong relationship? NASH not so much")



# soil moisture -----------------------------------------------------------
# DUL = 0.259 for nash, sat = 0.415
# DUL = 0.310 for ames, sat = 0.445


#--how much does the average soil moisture differ each year?
sims %>% 
  filter(set_id == 1, till == "normal") %>% 
  select(site, rot2, year, till, SWSowing) %>% 
  pivot_wider(names_from = rot2, values_from = SWSowing) %>% 
  mutate(dul = ifelse(site == "ames", 0.310, 0.259),
         sat = ifelse(site == "ames", 0.445, 0.415)) %>% 
  ggplot() + 
  geom_linerange(aes(ymin = sc, ymax = cc, x = year)) +
  geom_hline(aes(yintercept = dul), linetype = "dashed") +
  geom_hline(aes(yintercept = sat), linetype = "dashed") +
  geom_point(aes(year, cc), color = "gold2", size = 4) +
  geom_point(aes(year, sc), color = "green4", size = 4) +
  facet_grid(.~site, scales = "free_x") + 
  labs(x = NULL,
       title = "14-day avg soil moisture at planting",
       subtitle = "DUL and SAT indicated with dashed lines",
       y = "Soil moisture (%)")


#--look at where that falls in penalty


# visualize rue dec ---------------------------------------

rue <- read_csv("../../../apsim/data_apsim-outs/01_create-parameter-grids/d01_param-grid-oat-rue.csv")

ruemois <- 
  rue %>% 
  filter(grepl("xMoistureString_yMoistureString|defaults", oat_id)) %>% 
  select(oat_desc, xMoistureString, yMoistureString) %>% 
  separate(xMoistureString, into = c("x1", "x2", "x3", "x4", "x5"), sep = ",") %>% 
  separate(yMoistureString, into = c("y1", "y2", "y3", "y4", "y5"), sep = ",") %>% 
  mutate(oat_desc = factor(oat_desc)) %>% 
  mutate_if(is.character, parse_number) 

viz_ruemois <- 
  ruemois %>% 
  pivot_longer(x1:x5, values_to = "xcoord") %>%
  mutate(name = parse_number(name),
         oat_desc = paste(oat_desc, name)) %>% 
  select(oat_desc, xcoord) %>%
  left_join(
    ruemois %>% 
      pivot_longer(y1:y5, values_to = "ycoord")  %>%
      mutate(name = parse_number(name),
             oat_desc = paste(oat_desc, name)) %>%  
      select(oat_desc, ycoord)
  ) %>% 
  mutate(oat_desc = str_sub(oat_desc, 0, -3))

vals_ruemois <- 
  sims %>% 
  filter(set_id == 1, till == "normal") %>% 
  select(site, rot2, year, till, SWSowing) %>% 
  #pivot_wider(names_from = rot2, values_from = SWSowing) %>% 
  mutate(dul = ifelse(site == "ames", 0.310, 0.259),
         sat = ifelse(site == "ames", 0.445, 0.415),
         mois_ind = (SWSowing - dul) / (sat - dul) + 1) 

viz_ruemois  %>%  
  ggplot(aes(xcoord, ycoord, group = oat_desc)) + 
  geom_line() +
  geom_line(data = viz_ruemois %>% filter(grepl("defaults", oat_desc)), color = "red", size = 2) +
  geom_vline(data = vals_ruemois, aes(xintercept = mois_ind, color = site)) +
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  facet_grid(.~rot2) +
  labs(x = "Average Soil Moisture",
       y = "% RUE",
       title = "RUE reduction due to soil moisture")


ggsave("fig_diag_noscript-mois-at-planting.png")

# windmills ---------------------------------------------------------------

o.gaps %>% 
  left_join(s.gaps %>% filter(till == "normal")) %>% 
  select(set_id, site, year, obs_gap_kgha, sim_gap_kgha) %>%
  filter(!is.na(obs_gap_kgha), !is.na(sim_gap_kgha), set_id == 1) %>% 
  pivot_longer(obs_gap_kgha:sim_gap_kgha) %>% 
  group_by(site, set_id, name) %>% 
  arrange(value) %>% 
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, value)) + 
  geom_point(aes(color = name)) +
  facet_wrap(site~set_id, scales = "free", labeller = label_wrap_gen())

ggsave("fig_dig_noscript-gap-stairstep.png")


