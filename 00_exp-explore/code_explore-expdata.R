# Created:       2/17/2020
# last edited:   3/16/2020
#                3/20/2020
#                6/3/2020
#
# purpose: diagnose john sawyer's data
#
# notes: sotiris wants a bunch of stuff...need to work on that
#  are the gaps related to weather? ie disease? I tried the soil N thing, didn't work


rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2", force = T)
library(tidysawyer2) #--saw_xx data
library(lme4)
library(tidyverse)



# calc sawyer things ------------------------------------------------------

#--lewis has a weird 2013 year
#--normal gap at 0, no gap at mid, neg gap at high. 
saw_tidysawyer %>% 
  filter(site == "lewi") %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  facet_wrap(~year)

#--remove that year for now

saw_filt <- 
  saw_tidysawyer %>% 
    filter(! (year == 2013 & site == "lewi"))

d_max <- 
  saw_cgap %>% 
  filter(!(year == 2013 & site == "lewi"))

# penalty at mid-N rate
d_mid <-  
  saw_filt %>% 
  group_by(crop, site, rotation) %>% 
  filter(nrate_kgha > 100,
         nrate_kgha < 150) %>% 
  select(crop, site, year, rotation, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(cgap_mid = sc-cc) %>% 
  filter(!is.na(cgap_mid)) %>% 
  select(-cc, -sc)

# gap at 0N
d_0 <- 
  saw_filt %>% 
  group_by(crop, site, rotation) %>% 
  mutate(nmin = min(nrate_kgha)) %>% 
  filter(nrate_kgha == nmin) %>% 
  select(crop, site, year, rotation, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(cgap_0 = sc-cc) %>% 
  filter(!is.na(cgap_0)) %>% 
  select(-cc, -sc)


d_gaps <- 
  d_max %>% 
  left_join(d_mid) %>% 
  left_join(d_0)



# how do yield gaps vary by loc? by year? ---------------------------------

# note: there are 7 sites for 2006-2016 only

figloc <- 
  d_gaps %>% 
  filter(year > 2005) %>% 
  ggplot(aes(reorder(site, cgap_max, mean), cgap_max)) + 
  geom_boxplot() + 
  geom_point() +
  coord_flip() + 
  labs(title = '2006-2016',
       y = "yield gap")
  
figyr <- 
  d_gaps %>% 
  group_by(year) %>% 
  mutate(n = n()) %>% 
  filter(n == 7) %>% 
  ggplot(aes(reorder(year, cgap_max, mean), cgap_max)) + 
  geom_boxplot() + 
  geom_point() +
  coord_flip() + 
  labs(title = "7 sites",
       x = NULL,
       y = "yield gap")
  


figyr

library(patchwork)

figloc / figyr

ggsave("00_explore/fig_variation-yr-site.png")


# how to quantify variation explained by year vs site? --------------------


#--assign site as random effect, compare to residual variation (which is basically year?)
library(lme4)
library(broom)

d_stats <- 
  d_gaps %>% 
  unite(site, year, col = "site_yr", remove = F)

m1 <- lmer(cgap_max ~ 1 + (1|site), data = d_stats)
summary(m1)

m2 <- lm(cgap_max ~ 1, data = d_stats)

anova(m1, m2)

#--nothing is even significant. 

# are yield gpas at min and max N rates related? --------------------------

#--no. put that in the 'draft'. It's contrary to Gentry et al. 
d_gaps %>% 
  ggplot(aes(cgap_0, cgap_max)) + 
  geom_point(aes(color = site)) + 
#  facet_grid(.~site) + 
  geom_smooth(method = "lm", aes(color = site), se = F) + 
  labs(x = "Yield gap at 0 Nrate (kg/ha)", 
       y = "Yield gap at max Nrate (kg/ha)") + 
  theme_bw()