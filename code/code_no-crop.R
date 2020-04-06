# created: jan 14 2020
# updated: mar 20 2020 (revisiting)
#
# gina
#
# explore apsim sim results from no-crop (N-mineralization)


# libs --------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(lubridate)
library(janitor)
devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has his data in it :P



# read in .out files ------------------------------------------------------

# there are a lot of these, so I wrote an rds file. Only run this if something changes
# my_dir <- "../../../Box/Gina_APSIM_modeling/sims_no-crop/sim-nocrops-all/"
# list.files(my_dir)
# apraw <-
#   saf_readapout(my_dir)   
# 
# write_rds(apraw, "_data/td_sims_no-crop-res.rds")

ad <- read_rds("_data/td_sims_no-crop-res.rds")

amin <- 
  ad %>% 
  mutate(ext = str_extract(path, "[\\w-]+(?=.out)")) %>% 
  separate(ext, into = c("site", "other"), sep = "-") %>% 
  filter(doy < 200) %>% 
  group_by(site, year) %>% 
  summarise(dlt_n_min = sum(dlt_n_min))


# can just use sad_cgap in the future
#--Nmin vs gap
sad_tidysawyer %>% 
  group_by(crop, site, rotation) %>% 
  mutate(nmax = max(nrate_kgha)) %>% 
  filter(nrate_kgha == nmax) %>% 
  select(crop, site, year, rotation, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(cgap_max = sc-cc) %>% 
  filter(!is.na(cgap_max)) %>% 
  select(-cc, -sc) %>% 
  left_join(amin) %>% 
  ggplot(aes(dlt_n_min, cgap_max)) + 
  geom_point(aes(color = site), size = 3) + 
  geom_smooth(method = "lm", se = F, color = "black") +
  labs(title = "7 sites, 2000-2015",
       x = "Total N mineralized Jan 1 - July 20",
       y = "CC Yield Gap (kg/ha)") + 
  theme_bw()

#--Nmin by site/year

fsite <- amin  %>% 
  ggplot(aes(site, dlt_n_min)) + 
  geom_point() + 
  coord_flip()

fyear <- amin  %>% 
  ggplot(aes(year, dlt_n_min)) + 
  geom_point() + 
  coord_flip()

library(patchwork)
fsite / fyear

library(lme4)
m1 <- lmer(dlt_n_min ~ 1 + (1|site) + (1|year), data = amin)
summary(m1)

