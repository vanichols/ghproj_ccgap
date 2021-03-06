# goal: look at sotiris +mitch sims to see if water is reset
# created: 6/9/2021
# updated: 
#
# notes: 

rm(list = ls())

library(tidyverse)
library(tidysawyer2)
library(saapsim)
library(apsimx)

dat <- read_csv("00_soil-water-reset-Qs/sw_all.csv")

# viz ---------------------------------------------------------------------

saf_date_to_doy("2001-04-15")

dat %>% 
  filter(site == "ames") %>% 
  ggplot(aes(day, value, group = year)) + 
  geom_line(aes(color = as.factor(year))) + 
  geom_hline(aes(yintercept = dul_mm)) + 
  geom_vline(xintercept = 105) + 
  facet_grid(depth_cm ~ rot) + 
  guides(color = F) +
  labs(
    title = "Ames cont corn soil moisture 1999 - 2016",
       subtitle = "horizontal line = DUL",
       x = "day of year",
       y = "soil mois, mm/mm")


#--7 cm depth
dat %>% 
  filter(site == "ames") %>% 
    filter(depth_cm == 7,
         day < 105) %>% 
  ggplot(aes(day, value, group = year)) + 
  geom_line(aes(color = as.factor(year))) + 
  geom_hline(aes(yintercept = dul_mm)) + 
  geom_vline(xintercept = 105) + 
  facet_wrap(~rot) + 
  labs(title = "Ames soil moisture 1999 - 2016, 7 cm depth",
       subtitle = "horizontal line = DUL",
       x = "day of year",
       y = "soil mois, mm/mm")


#--38 cm depth
dat %>% 
  filter(site == "ames") %>% 
  filter(depth_cm == "38 cm depth",
         day < 105) %>% 
  ggplot(aes(day, value, group = year)) + 
  geom_line(aes(color = as.factor(year))) + 
  geom_hline(aes(yintercept = dul_mm)) + 
  geom_vline(xintercept = 105) + 
  facet_wrap(~rot) + 
  labs(title = "Ames soil moisture 1999 - 2016, 38 cm depth",
       subtitle = "horizontal line = DUL",
       x = "day of year",
       y = "soil mois, mm/mm")

dat %>% 
  filter(site == "ames") %>% 
  filter(#depth_cm == "38 cm depth",
         year >= 2011, 
         year <= 2014) %>% 
  ggplot(aes(Date, value)) + 
  geom_line() + 
  geom_hline(aes(yintercept = dul_mm)) + 
  facet_grid(depth_cm~rot) 


# pwalk it ----------------------------------------------------------------



#--use pwalk to make a fig for each site

plot_dat <- dat

plots <-
  plot_dat %>%
  split(.$site) %>%
  map( ~ (
    ggplot(., aes(day, value, group = year)) + 
      geom_line(aes(color = as.factor(year))) + 
      geom_hline(aes(yintercept = dul_mm)) + 
      geom_vline(xintercept = 105) + 
      facet_grid(depth_cm ~ rot) + 
      guides(color = F) +
      labs(title = "Soil moisture 1999 - 2016",
           subtitle = "horizontal line = DUL",
           x = "day of year",
           y = "soil mois, mm/mm")
    
  ))


paths <- stringr::str_c(names(plots), ".png")

pwalk(list(paths, plots), ggsave, path = "00_soil-water-reset-Qs/figs/", width = 9) 

