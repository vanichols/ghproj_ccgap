# created 2/18/21
# gina
# purpose: figure out how to visualize the fits


rm(list = ls())
library(tidysawyer2)
library(tidyverse)
library(scales)
library(ggmosaic)

fly %>% 
  select(DoYouRecline, RudeToRecline)

ggplot(data = fly) +
  geom_mosaic(aes(x = product(DoYouRecline, RudeToRecline), fill=DoYouRecline), na.rm=TRUE) + 
  labs(x = "Is it rude recline? ", title='f(DoYouRecline | RudeToRecline) f(RudeToRecline)')


npct <- 
  read_csv("00_empirical-n-cont/fits-npct.csv") %>% 
  mutate(ather_frac = 1-ngap_frac) %>% 
  pivot_longer(ngap_frac:ather_frac) %>% 
  mutate(
    mx = max(gap_at_contaonr_kgha, na.rm = T),
    gap_scaled = 1 - (mx - gap_at_rotaonr_kgha)/mx)


tst <- 
  npct %>% 
  filter(site == "ames") %>% 
  mutate(
    mx = max(gap_at_contaonr_kgha, na.rm = T),
    gap_scaled = 1 - (mx - gap_at_rotaonr_kgha)/mx + .01)

#--alpha
ggplot(data = tst, aes(x = year, y = value)) +
  geom_col(aes(year, value, fill = name, alpha = gap_scaled)) 

#--width
ggplot(data = tst, aes(x = year, y = value)) +
  geom_col(aes(year, value, fill = name, width = gap_scaled)) 


npct %>% 
  left_join(ilia_yields %>% 
              select(site, state) %>% 
              distinct()) %>% 
  select(state, site, year, name, value, gap_scaled) %>% 
  ggplot(aes(year, value)) + 
  geom_col(aes(fill = name, width = gap_scaled)) + 
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~site) + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c("gray60", "red")) +
  labs(title = "Percentage of penalty related to nitrogen",
       subtitle = "Empirical approach")

npct %>% 
  left_join(ilia_yields %>% 
              select(site, state) %>% 
              distinct()) %>% 
  select(state, site, year, name, value, gap_scaled) %>% 
  ggplot(aes(year, value)) + 
  geom_col(aes(fill = name)) + 
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_grid(.~site, scales = "free_x", space = "free") + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c("gray60", "red")) +
  labs(title = "Percentage of penalty related to nitrogen",
       subtitle = "Empirical approach")


npct %>% 
  left_join(ilia_yields %>% 
              select(site, state) %>% 
              distinct()) %>%
  filter(name == "ngap_frac") %>% 
  ggplot(aes(x = gap_at_rotaonr_kgha, value)) + 
  geom_point()
