# Created:       12/2/2020
# 
# purpose: look at vars id'd by stepwise
#
# notes: 
#
# last edited:   
#

rm(list = ls())
detach("package:MASS", unload=TRUE)
library(tidyverse)

# data --------------------------------------------------------------------

dat <- read_csv("01_create-features/4_dat_preds-all.csv")
sw <- read_csv("02_fit-models/02_stepwise-selections.csv")

pct_vars <- 
  sw %>% 
  filter(respvar == "pen_pct") %>% 
  pull(name)

kgha_vars <- 
  sw %>% 
  filter(respvar == "pen_kgha") %>% 
  pull(name)

dat %>%
  select(-crop, -yearF, -nrate_kgha, -drainage, -pen_kgha) %>% 
  select(state, site, year, pen_pct, everything()) %>% 
  pivot_longer(5:ncol(.)) %>% 
  filter(name %in% pct_vars) %>% 
  ggplot(aes(value, pen_pct)) + 
  geom_point(aes(color = state)) + 
  facet_wrap(~name, scales = "free") + 
  labs(title = "Stepwise selections, pen_pct")


dat %>%
  select(-crop, -yearF, -nrate_kgha, -drainage, -pen_pct) %>% 
  select(state, site, year, pen_kgha, everything()) %>% 
  pivot_longer(5:ncol(.)) %>% 
  filter(name %in% pct_vars) %>% 
  ggplot(aes(value, pen_kgha)) + 
  geom_point() + 
  facet_wrap(~name, scales = "free") + 
  labs(title = "Stepwise selections, pen_kgha")

dat %>%
  select(-crop, -yearF, -nrate_kgha, -drainage, -pen_pct) %>% 
  select(state, site, year, pen_kgha, everything()) %>% 
  pivot_longer(5:ncol(.)) %>% 
  filter(name %in% pct_vars) %>% 
  ggplot(aes(value, pen_kgha)) + 
  geom_point(aes(color = state)) + 
  geom_smooth(aes(color = state), method = "lm", se=F) +
  facet_wrap(~name, scales = "free") + 
  labs(title = "Stepwise selections, pen_kgha")
