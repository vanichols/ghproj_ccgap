# Created:       2/17/2020
# last edited:   3/16/2020
# 
# purpose: diagnose john sawyer's data
#
# notes: sotiris wants a bunch of stuff...need to work on that


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has his data in it :|
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


# penalty at max N rate
d_max <- 
  sad_filt %>% 
  group_by(crop, site, rotation) %>% 
  mutate(nmax = max(nrate_kgha)) %>% 
  filter(nrate_kgha == nmax) %>% 
  select(crop, site, year, rotation, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(cgap_max = sc-cc) %>% 
  filter(!is.na(cgap_max)) %>% 
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
       x = "location",
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
  labs(x = NULL,
       y = "yield gap")
  

library(patchwork)

figloc / figyr


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



###############old#################################

# is yield gap related to average yield? --------------------------------------

# slightly. Gentry found it's related to the yield gap at 0
sad_tidysawyer %>% 
  group_by(site, year) %>% 
  mutate(oa_kgha = mean(yield_kgha, na.rm = T)) %>% 
  left_join(cpen) %>% 
  filter(!is.na(cpen)) %>% 
  ggplot(aes(oa_kgha/1000, cpen/1000)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  labs(x = "Site-year average yield", 
       y = "Corn Penalty (Mg/ha)")

# mono-pen at 0N vs mono-pen at highest N, no.
cpen0 %>% 
  select(crop, site, year, cpen0) %>% 
  left_join(cpen) %>% 
  ggplot(aes(cpen0/1000, cpen/1000)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) + 
  labs(x = "Mono-pen at 0N", 
       y = "Mono-pen at HN (Mg/ha)")
  

sad_tidysawyer %>% select(site, year) %>% table()

# take average yield at highest N rate instead?
sad_tidysawyer %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  group_by(crop, site, year) %>% 
  mutate(myield = mean(yield_kgha, na.rm = T)) %>% 
  ggplot(aes(myield, yield_kgha, color = rotation)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  labs(x = "Mean site-year yield at HN",
       y = "Yield at HN",
       title = "7 sites, 17 years")
         
# does the gap go down w/higher yields? Kind of.    
# notice it is not the same sites that yield high
cpen %>% 
  mutate(yldav = (cc + sc)/2) %>% 
  ggplot(aes(yldav, cpen)) +
  geom_point(aes(color = site), size = 3) +
  geom_smooth(method = "lm", se = F, color = "black") + 
  geom_hline(yintercept = 0, color = "red") + 
  labs(x = "mean site-year yield at HN",
       y = "cont cropping penalty, kg/ha",
       title = "7 sites, 17 years")

#--is it a sig slope?
cpen %>% 
  mutate(yldav = (cc + sc)/2,
         yldavMg = yldav/1000,
         cpenMg = cpen/1000) %>%
  lm(cpenMg ~ yldavMg, data = .) %>% 
  summary()
# yes, it estimates a cpen of 2.7 Mgha at lowest yielding year
# It goes down 142 kg per 1 Mg increase in site-year yield

# yield gaps by year and site ---------------------------------------------

cpen %>%
  ggplot(aes(year, cpen)) + 
  geom_point()

cpen %>%
  ggplot(aes(site, cpen)) + 
  geom_point()

# which has more variation? YEAR!!!!!!!!!!!!!!
cpen %>% 
  lmer(cpen ~ 1 + (1|site) + (1|year), data = .)
  

#--ccpenalty by site
sad_ccpen <- sad_tidysawyer %>% 
  group_by(crop, site, rotation) %>% 
  mutate(nmax = max(nrate_kgha)) %>% 
  filter(nrate_kgha == nmax) %>% 
  select(crop, site, year, rotation, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(ccpen = sc-cc) 


sad_ccpen %>% 
  ggplot(aes(reorder(site, ccpen, FUN = mean, na.rm = T), ccpen)) + 
  geom_point() + 
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "point", color = "red", size = 3) + 
  geom_hline(yintercept = sad_ccpen %>% 
               ungroup() %>% 
               summarise(ccpen = mean(ccpen, na.rm = T)) %>% 
               pull(ccpen),
             color = "red") +
  coord_flip()

ggsave("_figs/raw_ccpen-by-site.png")

sad_tidysawyer %>% 
  group_by(crop, site, rotation) %>% 
  mutate(nmax = max(nrate_kgha)) %>% 
  filter(nrate_kgha == nmax) %>% 
  select(crop, site, year, rotation, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(ccpen = sc-cc) %>% 
  ggplot(aes(reorder(year, ccpen, mean, na.rm = T), ccpen)) + 
  geom_point() + 
  stat_summary(fun.y = mean, geom = "point", color = "red", size = 3) + 
  coord_flip()

