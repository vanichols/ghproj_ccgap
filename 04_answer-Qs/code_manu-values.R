# Created:       july 1 2021
#
# purpose: calc things for manu
#
# notes: 
# last edited:   


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


# number of sites ---------------------------------------------------------

ilia_yields %>% 
  select(state, site) %>% 
  distinct() %>% 
  group_by(state) %>% 
  tally()


# yield summary -----------------------------------------------------------

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

m1a <- lmer(yield_kgha ~ rotation + (1|site) + (1|yearF), data = mxylds)
summary(m1a)

m1b <- lmer(yaonr ~ rotation + (1|site) + (1|yearF), data = platylds)
summary(m1b)

# gap summary -------------------------------------------------------------

#--there is only 157 here. Oh. There are NAs
gap <- read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap) %>% 
  mutate(yearF = as.factor(year)) 

#--36 NAs
gap %>% 
  filter(is.na(nonngap))

#--12 0s
gap %>% 
  filter(nonngap == 0)

157- (36)

gap %>% 
  ggplot(aes(nonngap)) + 
  geom_histogram()

gap %>% 
  summary()

m2 <- lmer(nonngap ~ (1|site) + (1|yearF), data = gap)

summary(m2)

mean(ngap$nonngap)

gap %>% 
  mutate(nover = ifelse(nonngap < 0.01, "Y", "N")) %>% 
  group_by(nover) %>% 
  summarise(n = n())
