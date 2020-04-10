# Created:       4/6/2020
# last edited:   
# 
# purpose: Visualize cont corn penalty over years
#
# notes: 


rm(list = ls())
library(tidyverse)
library(readxl)
library(janitor)
library(tidysawyer2)

# data to inform filling in 'over-years' tab ------------------------------

rd <- read_excel("data/lit/lit_summary-penalty-over-years.xlsx", sheet = "ind-studies-years") %>% 
  fill(location)

rd %>% 
  pivot_wider(names_from = corn_year, values_from = yield) %>% 
  clean_names() %>% 
  pivot_longer(x2:x10) %>% 
  mutate(pct = value/x1 * 100) %>%
  rename(years_in_corn = name) %>% 
  select(location, years_in_corn, pct)   %>% 
  write_csv("data/lit/lit_for-over-years.csv")


#data --------------------------------------------------------------------

dat <- read_excel("data/lit/lit_summary-penalty-over-years.xlsx", sheet = "over-years") %>% 
  fill(scope)


# sawyer data -------------------------------------------------------------

saw <- 
  saw_tidysawyer %>%
  select(site, year, rotation, nrate_kgha, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(relative_yield = cc/sc*100) %>% 
  arrange(site, year, nrate_kgha) %>% 
  mutate(years_in_corn = group_indices(., site, year),
         scope = paste0(site, "N", round(nrate_kgha, 0)),
         citation = "sawyer") %>% 
  select(years_in_corn, relative_yield, scope)


saw_tidysawyer %>%
  select(site, year, rotation, nrate_kgha, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(relative_yield = cc/sc*100) %>% 
  arrange(site, year, nrate_kgha) %>% 
  filter(site %in% c("ames", "nash")) %>% 
  filter(year %in% c(2003, 2005)) %>% 
  group_by(site, year) %>% 
  mutate(row = row_number())
  mutate(n = group_indices(., year))


library(dplyr)
dat <- tibble(site = c(rep("ames", 4), rep("nash", 4)),
              year = c(2012, 2012, 2013, 2013, 2013, 2013, 2015, 2015))
  
#--desired data
dat %>% 
  mutate(id = c(1, 1, 2, 2, 1, 1, 2, 2))

#--no 
dat %>% 
  mutate(n = group_indices(., year, site))

#--no 
dat %>% 
  mutate(n = group_indices(., year))


# viz ---------------------------------------------------------------------

dat %>%
  bind_rows(saw) %>% 
  ggplot(aes(years_in_corn, relative_yield)) + 
  geom_point(size = 2, aes(color = scope, group = interaction(scope, citation))) + 
  geom_line(size = 2, aes(color = scope, group = interaction(scope, citation))) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  labs(x = "Years In Corn",
       y = "Yield Relative to Rotated Corn") +
  theme(legend.background = element_rect(color = "black"),
        legend.position = c(0.9, 0.9),
        legend.justification = c(1,1))

ggsave("figs/lit_years-in-corn.png")
ggsave("../../../Box/Gina_APSIM_modeling/figs-from-repo/lit_years-in-corn.png")
