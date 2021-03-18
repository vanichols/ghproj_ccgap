# created 3/17/2021
# gina
# purpose: look at and tweak n pct calcs
# updated:

library(saapsim)
library(tidysawyer2)
library(tidyverse)
library(scales)

theme_set(theme_bw())

dat <- read_csv("00_empirical-n-cont/fits-npct.csv")

dat %>% 
  filter(site == "ames")

dat %>% 
  filter(ngap_frac < 0.10)

dat %>% 
  filter(ngap_frac < 0.50,
         ngap_frac > 0.3)


# ngap vs gap -------------------------------------------------------------

dat %>% 
  ggplot(aes(gap_at_rotaonr_kgha, ngap_frac)) + 
  geom_point(size = 3) + 
  labs(x = "CC yield gap (kg/ha)",
       y = "Percentage of gap that can be overcome with N-fert") + 
  scale_y_continuous(labels = label_percent())


dat %>% 
  filter(gap_at_rotaonr_kgha > 4000)


dat %>% 
  filter(gap_at_rotaonr_kgha > 2000,
         ngap_frac > 0.9)

saf_kgha_to_buac_corn(1000)

dat %>% 
  mutate(nover = ifelse(ngap_frac > 0.5, "yes", "no"),
         biggap = ifelse(gap_at_rotaonr_kgha > 1200, "yes", "no")) %>% 
  filter(!is.na(nover)) %>% 
  group_by(nover, biggap) %>% 
  summarise(n = n())


dat %>% 
  filter(!is.na(ngap_frac)) %>% 
  mutate(nover = ifelse(ngap_frac > 0.5, "yes", "no"),
         biggap = ifelse(gap_at_rotaonr_kgha > 1200, "yes", "no"),
         cat = paste(nover, biggap)) %>% 
  ggplot(aes(gap_at_rotaonr_kgha, ngap_frac)) + 
  geom_point(size = 3, aes(color = cat)) + 
  labs(x = "CC yield gap (kg/ha)",
       y = "Percentage of gap that can be overcome with N-fert") + 
  scale_y_continuous(labels = label_percent()) +
  guides(color = F)


# ngap by site ------------------------------------------------------------

dat %>% 
  filter(!is.na(ngap_frac)) %>% 
  ggplot(aes(reorder(site, ngap_frac, mean), ngap_frac)) + 
  geom_jitter(width = 0.2, aes(color = site), size = 2) + 
  stat_summary(geom = "point", size = 5, color = "black") + 
  coord_flip() + 
  guides(color = F)
