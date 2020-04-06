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

# data to inform filling in 'over-years' tab ------------------------------

 rd <- read_excel("data/lit/lit-summary-penalty-over-years.xlsx", sheet = "ind-studies-years") %>% 
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

dat <- read_excel("_data/lit/lit_summary-penalty-over-years.xlsx", sheet = "over-years") %>% 
  fill(scope)


# viz ---------------------------------------------------------------------

dat %>% 
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
