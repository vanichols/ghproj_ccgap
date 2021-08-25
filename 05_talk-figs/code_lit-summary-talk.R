# Created:       4/6/2020
# last edited:   5/26/2020 (moved to new folder)
#                11/18/20 make look nicer
#                8/19/2021 make it talk friendly
# 
# purpose: Visualize cont corn penalty over years
#
# notes: 


rm(list = ls())
library(tidyverse)
library(readxl)
library(janitor)
library(tidysawyer2)

source("05_talk-figs/talk-palette3.R")

# data to inform filling in 'over-years' tab ------------------------------

# rd <- read_excel("00_lit-explore/lit_summary-penalty-over-years.xlsx", sheet = "ind-studies-years") %>% 
#   fill(location)
# 
# rd %>% 
#   pivot_wider(names_from = corn_year, values_from = yield) %>% 
#   clean_names() %>% 
#   pivot_longer(x2:x10) %>% 
#   mutate(pct = value/x1 * 100) %>%
#   rename(years_in_corn = name) %>% 
#   select(location, years_in_corn, pct)   %>% 
#   write_csv("data/lit/lit_for-over-years.csv")


# data --------------------------------------------------------------------

dat <- 
  read_excel("00-lit-explore/lit_summary-penalty-over-years.xlsx", sheet = "over-years") %>% 
  fill(scope)



# viz nice ---------------------------------------------------------------------

library(scales)

theaverage <- 
  dat %>%
  filter(!grepl("Gentry|Seifert", citation)) %>% 
  filter(years_in_corn > 1) %>% 
  summarise(mn_rel_yield = mean(relative_yield)) %>% 
  pull()/100


dat %>%
  mutate(penalty = 100 - relative_yield) %>% 
  filter(!grepl("Gentry|Seifert", citation)) %>%  
  ggplot(aes(years_in_corn, penalty/100)) +
  geom_point(size = 2, 
             aes(group = interaction(scope, citation)),
             color = clr_red) + 
  geom_line(aes(group = interaction(scope, citation)),
            color = clr_red) + 
  guides(color = F) +
  labs(x = "Years In Corn",
       y = "Continuous Corn\nPenalty (%)", 
       color = NULL) +
  guides(pch = F) +
    scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, 0.5)) +
  labs(pch = NULL,
       title = "Experimental data from literature",
       caption = "Crookston et al. 1991;\nMeese et al. 1991;\nPorter et al. 1997") +
  theme_bw() + 
  theme(legend.background = element_rect(color = "black"),
        legend.position = c(0.9, 0.1),
        legend.justification = c(1,0),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(size = rel(2))) + 
  bigtheme

ggsave("05_talk-figs/fig_lit-pen-over-time.png", width = 9, height= 5)

