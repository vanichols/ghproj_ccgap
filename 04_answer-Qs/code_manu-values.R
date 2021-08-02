# Created:       july 1 2021
#
# purpose: calc things for manu
#
# notes: 
# last edited:   july 9 2021 - adding cc and sc as preds of penalty
#               july 26 - addressing sotiris comments


rm(list = ls())
#devtools::install_github("femiguez/nlraa")
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(scales)
library(fancycut)
library(lme4)
library(lmerTest)
library(patchwork)


# 1. number of sites ---------------------------------------------------------

ilia_yields %>% 
  select(state, site) %>% 
  distinct() %>% 
  group_by(state) %>% 
  tally()


# 2. yield summary -----------------------------------------------------------

#--what values did I wind up using? I say  8.7 and 9.7. Hmm. 

(9.7-8.7)/9.7

#--why aren't there 157 here?
platylds <- 
  ilia_aonr %>% 
  select(state, site, year, yaonr, rotation) %>% 
  mutate(yearF = as.factor(year))


mxylds <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(yearF = as.factor(year))

#--yields of cc and sc averaged over time, using yields at max N rate
m1a <- lmer(yield_kgha ~ rotation + (1|site) + (1|yearF), data = mxylds)
summary(m1a)

#--yields of cc and sc averaged over time, using yields at aonr plateua
m1b <- lmer(yaonr ~ rotation + (1|site) + (1|yearF), data = platylds)
summary(m1b)


# 2. N values for each site for table-----------------------------------------------------------
 
# ilia_yields %>% 
#   select(site, nrate_kgha) %>% 
#   distinct() %>% 
#   group_by(site) %>% 
#   summarise(across(everything(), str_c, collapse= ",")) %>% 
#   write_csv("04_answer-Qs/sites-nrates.csv")



# 3. avg ngap and nonngap from anor method----------------------------------------------------

gap_comps <- read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  mutate(yearF = as.factor(year)) 


m_nonn1 <- lmer(nonngap ~ (1|site) + (1|yearF), data = gap_comps)
m_nonn2 <- lmer(nonngap ~ (1|site), data = gap_comps)
anova(m_nonn1, m_nonn2)
#--keep year
summary(m_nonn1) #932


m_n1 <- lmer(ngap ~ (1|site) + (1|yearF), data = gap_comps)
m_n2 <- lmer(ngap ~ (1|site), data = gap_comps)
anova(m_n1, m_n2)
#--keep year
summary(m_n1) #427 #932


# 4. Penalty over time? ---------------------------------------

gaps_alln <-
  ilia_yields %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(ogap_kgha = sc - cc,
         ogap_pct = ogap_kgha/sc) %>% 
  mutate(nrate = cut_interval(nrate_kgha, n = 3),
         nrateF = as.numeric(nrate),
         nrateF = case_when(
           nrateF == 1 ~ "Low (0-90 kgN/ha)",
           nrateF == 2 ~ "Med (90-180 kgN/ha)",
           nrateF == 3 ~ "High (180-270 kgN/ha")
  ) 


#--obseved gaps, all sites

gaps_alln %>% 
  arrange(nrate) %>% 
  mutate(nrateF = fct_inorder(nrateF)) %>% 
  ggplot(aes(year, ogap_kgha)) + 
  geom_jitter() + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_grid(.~nrateF, scales = "free_x") + 
  labs(title = "Continuous corn penalty persists over time at all N rates")


# aonr method -------------------------------------------------------------

#--use the anor method. 

preds <- read_csv("00_empirical-n-cont/dat_preds.csv")
aonr <- read_csv("00_empirical-n-cont/dat_aonrs.csv")


dat_modA <- 
  aonr %>% 
  separate(aonr_rot, into = c("aonr", "rotation")) %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  left_join(preds) %>% 
  select(-nrate_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  filter(!is.na(gap_kgha)) %>% 
  mutate(gap_kgha = ifelse(gap_kgha < 0, 0, gap_kgha),
         gap_pct = gap_kgha/sc) %>% 
  pivot_longer(cc:gap_pct) %>% 
  mutate(year0 = year - min(year), 
         yearF = as.factor(year))


#--is there an interaction btwn rot and year?
m9rot <- lmer(value ~ name*year0 + (1+year0|site), 
              data = dat_modA %>% filter(name %in% c("cc", "sc")))

summary(m9rot) #super no, but year is sig

#--ok fit a model without rotation
m9 <- lmer(value ~ year0 + (1 + year0|site), 
           data = dat_modA %>% filter(name %in% c("cc", "sc")))

summary(m9) #212 +-30

#--what are the rotation yields though?
m9b <- lmer(value ~ year0*name + (1 + year0|site), 
           data = dat_modA %>% filter(name %in% c("cc", "sc")))



emmeans::emmeans(m9b, specs = "name")

#--gap size

summary(lmer(value ~ year0 + (1 + year0|site),
             data = dat_modA %>% filter(name == "gap_kgha")))

emmeans::emmeans(m10, specs = "year0")
# year not sig, ignore it? Or leave it in the model? IDK

# just leave it, report the 1252 SE:227

#--gap pct

summary(lmer(value ~ year0 + (1 + year0|site),
             data = dat_modA %>% filter(name == "gap_pct")))

# year not sig, ignore it? Or leave it in the model? IDK

# just leave it, report the 1252 SE:227



# 5. avergage sd in iowa --------------------------------------------------


ia_yields_se %>% 
  summarise(sd_kgha = mean(sd_kgha, na.rm = T))




# 6. cc vs sc as predictor of pen-------------------------------------------------

preds <- read_csv("00_empirical-n-cont/dat_preds.csv")
aonr <- read_csv("00_empirical-n-cont/dat_aonrs.csv")


dat_mod6 <- 
  aonr %>% 
  separate(aonr_rot, into = c("aonr", "rotation")) %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  left_join(preds) %>% 
  select(-nrate_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  filter(!is.na(gap_kgha)) %>% 
  mutate(gap_kgha = ifelse(gap_kgha < 0, 0, gap_kgha),
         gap_pct = gap_kgha/sc) %>% 
  mutate(year0 = year - min(year), 
         yearF = as.factor(year))


m6cc <- lmer(gap_kgha ~ cc + (1+ year0|site), data = dat_mod6)

m6sc <- lmer(gap_kgha ~ sc + (1+ year0|site), data = dat_mod6)

summary(m6cc)
summary(m6sc)


# highest and lowest obs pens ---------------------------------------------

gaps <- 
  read_csv("00_empirical-n-cont/dat_aonrs.csv") %>% 
  filter(aonr_rot == "aonr_sc") %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  left_join(
    read_csv("00_empirical-n-cont/dat_preds.csv") %>% 
  filter(rotation == "sc")
  ) %>% 
  left_join(gaps) %>% 
  mutate(gap_pct = nonngap/pred_yield) %>% 
  filter(!is.na(nonngap)) 

#--by year
gaps %>% 
  group_by(year) %>% 
  summarise(gap = mean(nonngap, na.rm = T),
            n = n()) %>% 
  arrange(-gap)
#--max?
gaps %>% 
  filter(nonngap == max(nonngap, na.rm = T))

gaps %>% 
  filter(gap_pct == max(gap_pct, na.rm = T))

gaps %>% 
  mutate(sz = ifelse(gap_pct > 0.1, ">.1", "less")) %>% 
  group_by(sz) %>% 
  tally() %>% 
  mutate(sum = sum(n),
         pct = n/sum)

gaps %>% 
  ggplot(aes(pred_yield, gap_pct)) + 
  geom_point()
  

gaps %>% 
  group_by(site) %>% 
  summarise(nonngap = mean(nonngap, na.rm = T)) %>% 
  arrange(nonngap)
  