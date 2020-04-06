# Created:       2/17/2020
# last edited:   3/16/2020
#                3/20/2020
# 
# purpose: diagnose john sawyer's data
#
# notes: sotiris wants a bunch of stuff...need to work on that
#  are the gaps related to weather? ie disease? I tried the soil N thing, didn't work


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has his data in it :P
library(tidyverse)
library(lme4)


# do i have site-specific soil data already? ------------------------------

soiraw <- read_csv("_data/td_pred-soil-from-root-study.csv") 

soi <- soiraw %>% 
  separate(site_yr, into = c("site", "year")) %>% 
  filter(profile_cm == "0-30") %>% 
  group_by(site) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  mutate(site = str_sub(tolower(site), 1, 4)) 


# calc sawyer things ------------------------------------------------------

#--lewis has a weired 2013 year
#--normal gap at 0, no gap at mid, neg gap at high. 
sad_tidysawyer %>% 
  filter(site == "lewi") %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  facet_wrap(~year)

#--remove that year for now

sad_filt <- 
  sad_tidysawyer %>% 
    filter(! (year == 2013 & site == "lewi"))

d_max <- 
  sad_cgap %>% 
  filter(! (year == 2013 & site == "lewi"))

# penalty at mid-N rate
d_mid <-  
  sad_filt %>% 
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
  sad_filt %>% 
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
ggsave("_figs/diag_cgap-vs-year.png")

library(patchwork)

figloc / figyr



# how to quantify variation explained by year vs site? --------------------


#--assign site and year as random effects?
library(lme4)
library(broom)
d_gaps
m1 <- lmer(cgap_max ~ 1 + (1|site) + (1|year), data = d_gaps)
summary(m1)

m1err <- 
  tidy(m1) %>% 
  filter(term != "(Intercept)") %>% 
  select(group, estimate)

#--can I include their interaction? Does it even matter?

#--look at variance 'components' (?)
m1err %>% 
  ggplot(aes(x = "", y = estimate, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + 
  theme_bw() +
  labs(fill = NULL,
       x = NULL,
       y = NULL,
       title = "Variance Components") + 
  theme(axis.text = element_blank())

#--what if I assume they are fixed effects?
m2 <- lm(cgap_max ~ site*year, data = d_gaps)
anova(m2)

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