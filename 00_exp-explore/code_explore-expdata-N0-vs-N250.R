# Created:       2/17/2020
# last edited:   3/16/2020
#                3/20/2020
#                6/3/2020
#                11/18/2020
#
# purpose: diagnose john sawyer's data
#


rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2")
library(tidysawyer2) #--saw_xx data
library(lme4)
library(tidyverse)
library(ungeviz)

#--what is the range in max values?
ilia_yields %>%
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  pull(nrate_kgha) %>% 
  range()

neff <- 
  ilia_yields %>%
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)|nrate_kgha == 0) %>% 
  mutate(rot2 = ifelse(rotation == "cs", "sc", rotation),
         nrate = ifelse(nrate_kgha == 0, "N0", "Nmax")) %>% 
  select(-rotation, -nrate_kgha) %>% 
  pivot_wider(names_from = rot2, values_from = yield_kgha) %>% 
  mutate(ccpen = sc - cc) %>% 
  select(-cc, -sc) %>% 
  pivot_wider(names_from = nrate, values_from = ccpen) %>% 
  mutate(Nmax2 = ifelse(Nmax < 0, 0, Nmax),
         ccpenN = (N0-Nmax2)/N0,
         ccpenN2 = ifelse(ccpenN < 0, 0, ccpenN)) %>% 
  filter(ccpenN < 1) #--mcnay 2009 the cc yielded more than cs in 0N

library(ggridges)

neff %>% 
  ggplot(aes(state, ccpenN2)) + 
  geom_jitter(width = 0.2, color = "red3") +
  scale_y_continuous(labels = label_percent()) +
  coord_flip() +
  labs(y = "Contin corn yield penalty\nattributed to nitrogen deficiency (%)",
       x = NULL) + 
  theme_bw()

ggsave("00_exp-explore/fig_ccpen-Ncont.png")

#--is CC0N yield related to gap?

cc0n <- 
  ilia_yields %>%
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rot2 = ifelse(rotation == "cs", "sc", rotation)) %>% 
  select(-rotation, -nrate_kgha) %>% 
  pivot_wider(names_from = rot2, values_from = yield_kgha) %>% 
  mutate(ccpen = sc - cc,
         ccpen2 = ifelse(ccpen < 0, 0, ccpen),
         ccpen_rel = ccpen/sc,
         ccpen_rel2 = ccpen2/sc) %>% 
  left_join(  ilia_yields %>%
                filter(nrate_kgha == 0, rotation == "cc") %>% 
                select(-rotation, -nrate_kgha) %>% 
                rename(yield_cc0n = yield_kgha)
  ) 


#remotes::install_github("cmartin/ggConvexHull")
library(ggConvexHull)

cc0n %>% 
  filter(yield_cc0n < 6000, ccpen_rel > 0) %>% 
  ggplot(aes(x = yield_cc0n, y = ccpen_rel)) + 
  geom_point(color = "red3") + 
  geom_density2d(alpha=.5)
  theme_bw()
  
cc0n %>%
  #  filter(yield_cc0n < 6000, ccpen_rel > 0) %>% 
    ggplot(aes(x = yield_cc0n, y = ccpen_rel2)) + 
    geom_convexhull(alpha=.2, fill = "lightblue") +
  geom_point(color = "red3") +
    theme_bw()
  
  

library(VCA)


# See how it works
remlVCA(rBM_kg.ha~site_yr, Data = rd) -> x

m1 <- lm(ccpen_rel ~ yield_cc0n, data = cc0n)
anova(m1)

library(broom)
tidy(m1)

summary(m1)
