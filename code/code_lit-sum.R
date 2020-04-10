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

dat %>%
  ggplot(aes(years_in_corn, relative_yield)) + 
  geom_point(size = 2, aes(color = scope, group = interaction(scope, citation))) + 
  geom_line(size = 2, aes(color = scope, group = interaction(scope, citation))) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  labs(x = "Years In Corn",
       y = "Yield Relative to Rotated Corn") +
  geom_hline(yintercept = 100) +
  theme(legend.background = element_rect(color = "black"),
         legend.position = c(0.9, 0.9),
         legend.justification = c(1,1))


ggsave("figs/lit_years-in-corn.png")
ggsave("../../../Box/Gina_APSIM_modeling/figs-from-repo/lit_years-in-corn.png")


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
ggsave("../../../Box/Gina_APSIM_modeling/figs-from-repo/lit_years-in-corn-sawyer.png")
