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
#devtools::install_github("vanichols/tidysawyer2")
library(tidysawyer2) #--saw_xx data
library(lme4)
library(tidyverse)



# what is range in gap? -------------------------------------------------


gaps_allN <- 
  ilia_yields %>% 
  mutate(rotation = ifelse(rotation == "cs", "sc", rotation)) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(pct = (sc - cc)/sc * 100,
         gap = (sc - cc),
         nrateF = cut(nrate_kgha, breaks = 3)) %>% 
  ungroup()

gaps_allN %>%
  filter(!is.na(gap)) %>% 
  group_by(nrateF) %>% 
  arrange(gap) %>%
  mutate(n = 1:n(),
         nfrac = n/n()) %>% 
  ggplot(aes(nfrac, gap)) + 
  geom_point(aes(color = nrateF)) +
  geom_hline(yintercept = 0) +
  coord_flip() 

gaps_allN %>%
  filter(!is.na(pct)) %>% 
  group_by(nrateF) %>% 
  arrange(pct) %>%
  mutate(n = 1:n(),
         nfrac = n/n()) %>% 
  ggplot(aes(nfrac, pct)) + 
  geom_point(aes(color = nrateF)) +
  geom_hline(yintercept = 0) +
  coord_flip() 


gaps <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rotation = ifelse(rotation == "cs", "sc", rotation)) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(pct = (sc - cc)/sc * 100,
         gap = (sc - cc)) %>% 
  ungroup()

#--gap by state
gaps %>%
  filter(!is.na(gap)) %>% 
  bind_rows( gaps %>% 
               group_by(state) %>% 
               summarise(pct = mean(pct, na.rm = T),
                         gap = mean(gap, na.rm = T)) %>% 
               mutate(site = "MEAN")) %>% 
  group_by(state) %>% 
  arrange(gap) %>%
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, gap)) + 
  geom_point(color = "gray50") +
  geom_point(data = . %>% filter(site == "MEAN"), color = "red", size = 3) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~state)

#--pct by state
gaps %>%
  filter(!is.na(gap)) %>% 
  bind_rows( gaps %>% 
               group_by(state) %>% 
               summarise(pct = mean(pct, na.rm = T),
                         gap = mean(gap, na.rm = T)) %>% 
               mutate(site = "MEAN")) %>% 
  group_by(state) %>% 
  arrange(pct) %>%
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, pct)) + 
  geom_point(color = "gray50") +
  geom_point(data = . %>% filter(site == "MEAN"), color = "red", size = 3) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_grid(.~state)


#--overall gap
gaps %>%
  filter(!is.na(gap)) %>% 
  bind_rows( gaps %>% 
               summarise(pct = mean(pct, na.rm = T),
                         gap = mean(gap, na.rm = T)) %>% 
               mutate(site = "MEAN")) %>% 
  arrange(gap) %>% 
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, gap)) + 
  geom_jitter(color = "gray50") +
  geom_point(data = . %>% filter(site == "MEAN"), color = "red", size = 3) +
  geom_hline(yintercept = 0) +
  coord_flip()

#--overall pct
gaps %>%
  filter(!is.na(gap)) %>% 
  bind_rows( gaps %>% 
               summarise(pct = mean(pct, na.rm = T),
                         gap = mean(gap, na.rm = T)) %>% 
               mutate(site = "MEAN")) %>% 
  arrange(pct) %>% 
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, pct)) + 
  geom_jitter(color = "gray50") +
  geom_point(data = . %>% filter(site == "MEAN"), color = "red", size = 3) +
  geom_hline(yintercept = 0) +
  coord_flip()


gaps %>% 
  group_by(state, site) %>% 
  summarise(mn = min(pct, na.rm = T),
            mx = max(pct, na.rm = T))


#--gap in one state, by site
gaps %>%
#  filter(!is.na(gap), state == "IA") %>%  
  group_by(site) %>% 
  arrange(gap) %>%
  mutate(n = 1:n()) %>% 
  ggplot(aes(n, gap)) + 
  geom_point(aes(color = as.factor(state))) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~site)




# recreate John's fig -----------------------------------------------------

saw_tidysawyer %>%
  group_by(site) %>% 
  mutate(nmax = max(nrate_kgha)) %>% 
  filter(nrate_kgha == nmax) %>% 
  group_by(site, year) %>% 
  mutate(siteyear_mean = mean(yield_kgha)) %>% 
  ggplot(aes(siteyear_mean, yield_kgha, color = rotation)) + 
  geom_point() + 
  geom_label(aes(label = site)) +
  geom_smooth(method = "lm")

dat <- 
  saw_tidysawyer %>%
  group_by(site) %>% 
  mutate(nmax = max(nrate_kgha)) %>% 
  filter(nrate_kgha == nmax) %>% 
  group_by(site, year) %>% 
  mutate(siteyear_mean = mean(yield_kgha)) 

m1 <- lm(yield_kgha/1000 ~ siteyear_mean*rotation, data = dat)

anova(m1)  
summary(m1)

m2 <- lm(yield_kgha/1000 ~ siteyear_mean + rotation, data = dat)
m3 <- lm(yield_kgha/1000 ~ siteyear_mean, data = dat)
anova(m3, m2)
anova(m2, m1)

# look at data w/error bars -----------------------------------------------

saw_tidysawyer %>% 
  filter(nrate_kgha > 250) %>% 
  filter(site == "ames") %>% 
  mutate(se_kgha = sd_kgha/sqrt(4)) %>% 
  ggplot(aes(year, yield_kgha)) + 
  geom_col(aes(fill = rotation), position = position_dodge2(width = 0.9)) + 
  geom_errorbar(aes(x = year, ymin = yield_kgha - se_kgha, ymax = yield_kgha + se_kgha),
                 color = "black", position = position_dodge2(width = 0.9), size = 1) +
  facet_wrap(.~site, scales = "free")

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