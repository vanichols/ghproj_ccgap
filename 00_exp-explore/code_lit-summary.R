# Created:       4/6/2020
# last edited:   5/26/2020 (moved to new folder)
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

rd <- read_excel("00_exp-explore/lit_summary-penalty-over-years.xlsx", sheet = "ind-studies-years") %>% 
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

dat <- read_excel("00_exp-explore/lit_summary-penalty-over-years.xlsx", sheet = "over-years") %>% 
  fill(scope)


# sawyer data -------------------------------------------------------------

saw <- 
  saw_tidysawyer %>%
  select(site, year, rotation, nrate_kgha, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(relative_yield = cc/sc*100) %>%
  filter(!is.na(relative_yield)) %>% 
  arrange(site, year, nrate_kgha) %>% 
  group_by(site) %>% 
  group_modify(~{
    .x %>% mutate(years_in_corn = group_indices(., year))
  }) %>% 
  mutate(scope = paste0(site),
         citation = "sawyer") %>% 
  ungroup() %>% 
  select(years_in_corn, relative_yield, scope, citation, nrate_kgha)

            
            
            
# viz ---------------------------------------------------------------------

library(scales)

theaverage <- 
  dat %>%
  filter(!grepl("Gentry", citation)) %>% 
  filter(years_in_corn > 1) %>% 
  summarise(mn_rel_yield = mean(relative_yield)) %>% 
  pull()/100


dat %>%
  filter(!grepl("Gentry", citation)) %>% 
  ggplot(aes(years_in_corn, relative_yield/100)) + 
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = .8, ymax = 1.2, fill = "gray70", alpha = 0.5) +
  geom_hline(yintercept = theaverage, linetype = "dotted", size = 2) +
  geom_point(size = 2, aes(color = scope, group = interaction(scope, citation))) + 
  geom_line(size = 2, aes(color = scope, group = interaction(scope, citation))) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  labs(x = "Years In Corn",
       y = "Yield Relative to Rotated Corn") +
  scale_y_continuous(labels = scales::label_percent()) +
#  geom_hline(yintercept = 1) +
  theme(legend.background = element_rect(color = "black"),
         legend.position = c(0.9, 0.9),
         legend.justification = c(1,1))


ggsave("00_exp-explore/fig_lit-penalty-over-time.png")


dat %>%
  bind_rows(saw) %>% 
  ggplot(aes(years_in_corn, relative_yield)) + 
  geom_point(size = 2, aes(color = scope, group = interaction(scope, citation))) + 
  geom_line(size = 2, aes(color = scope, group = interaction(scope, citation))) + 
  theme_bw() + 
  #scale_x_continuous(breaks = seq(1, 10, 1)) +
  labs(x = "Years In Corn",
       y = "Yield Relative to Rotated Corn") +
  geom_hline(yintercept = 100) +
  theme(legend.position = "bottom") +
  facet_grid(.~nrate_kgha)



ggsave("figs/lit_years-in-corn-sawyer.png")
