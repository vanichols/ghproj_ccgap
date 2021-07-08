# Created:       4/6/2020
# last edited:   5/26/2020 (moved to new folder)
#                11/18/20 make look nicer
# 
# purpose: Visualize cont corn penalty over years
#
# notes: 


rm(list = ls())
library(tidyverse)
library(readxl)
library(janitor)
library(tidysawyer2)

source("05_manu-figs/palettes.R")


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
  mutate(cit_star = ifelse(grepl("Gentry|Seifert", citation), "*", " "),
         citation = paste0(citation, cit_star)) %>% 
  ggplot(aes(years_in_corn-1, relative_yield/100)) + 
  geom_hline(yintercept = theaverage, linetype = "dotted", size = 1) +
  geom_point(size = 2, aes(color = cit_star, pch = citation, group = interaction(scope, citation))) + 
  geom_line(aes(color = cit_star, group = interaction(scope, citation))) + 
  geom_text(x = -0.1, y = 0.81, label = "*Not replicated field trials, excluded from mean calculation",
            hjust = 0, fontface = "italic", check_overlap = T) +
  guides(color = F) +
  labs(x = "Years of Continuous Maize",
       y = "Continuous Maize Yield Relative to Rotated Maize", 
       color = NULL) +
  scale_color_manual(values = c(dkpnk1, "gray50")) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  
  theme_bw() + 
  theme(legend.background = element_rect(color = "black"),
        legend.position = c(0.9, 0.9),
        legend.justification = c(1,1))

ggsave("05_manu-figs/fig_lit-ccorn-years.png")


