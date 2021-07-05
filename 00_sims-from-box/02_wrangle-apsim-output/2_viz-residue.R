# goal: combine results into something useful
# created: 1/6/2021
# updated: 
#
# notes: is residue related to penalty at certain sites?

rm(list = ls())

library(tidyverse)
library(saapsim) #--for conversions
library(tidysawyer2) #--for obs data
library(patchwork)
library(scales)

#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

theme_set(theme_bw())

# read in sims ------------------------------------------------------------

# #--loop it
# sim.dir <- "../00_apsim-data/all-sites-noscripts-old/"
# xfiles <-list.files(sim.dir)
# 
# for (i in 1:length(xfiles)) {
#   
#   tmp.file <- xfiles[i]
#   
#   if (i == 1) {
#     d1 <- read_csv(paste0(sim.dir, tmp.file))
#   }
#   
#   else {
#     d2 <- read_csv(paste0(sim.dir, tmp.file))
#     d1 <- bind_rows(d1, d2)
#   }
#   
#   i <- i + 1
# }
# 
# d1 %>% 
#   mutate(set_id = 1) %>%  
#   write_csv("02_all-sites-noscripts.csv")



# wrangle --------------------------------------------------------------

d1 <- read_csv("02_all-sites-noscripts.csv")

sims <- 
  d1 %>% 
  filter(soy_buac == 0) %>%
  separate(outfile, into = c("site", "rot_trt")) %>%
  mutate(
    rot = tolower(rot_trt),
    rot2 = ifelse(rot == "cs", "sc", rot)) %>% 
  select(set_id, site, rot2, everything(), -rot_trt)

s.gaps <- 
  sims %>% 
  pivot_wider(names_from = rot2, values_from = corn_buac) %>% 
  mutate(sgap_buac = sc - cc,
         sgap_kgha = saf_buac_to_kgha_corn(sgap_buac)) 

#--obs data

obs <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rot2 = rotation) %>% 
  select(-state, -crop, -nrate_kgha, -rotation)

o.gaps <- 
  obs %>% 
  pivot_wider(names_from = rot2, values_from = yield_kgha) %>% 
  mutate(ogap_kgha = sc - cc,
         ogap_buac = saf_kgha_to_buac_corn(ogap_kgha)) %>% 
  select(site, year, ogap_kgha, ogap_buac)

dat <- 
  sims %>% 
  left_join(o.gaps)

# viz ---------------------------------------------------------------------


dat %>% 
  filter(rot2 == "cc") %>%
  left_join(obs) %>% 
  ggplot(aes(ResidueWTatSowing, ogap_kgha))  + 
  geom_point(aes(size = yield_kgha)) + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_wrap(~site) + 
  labs(title = "CC penalty vs residue at sowing")

ggsave("fig_noscript-res-vs-gap.png")
