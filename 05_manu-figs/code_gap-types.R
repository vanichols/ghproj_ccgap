# Created:    april 17 2021
#
# purpose: comparing n- and non-n gaps
#
# notes: 
# last edited:   

rm(list = ls())
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(fancycut)
library(patchwork)
library(scales)
library(naniar)


theme_set(theme_bw())

source("05_manu-figs/palettes.R")


# data --------------------------------------------------------------------

dat <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap, ngap)

dat %>%
  arrange(nonngap, ngap) %>% 
  filter(!is.na(nonngap)) %>% 
  mutate(id = 1:n()) %>% 
  filter(nonngap > 0)



# fig ---------------------------------------------------------------------

dat %>%
  arrange(nonngap, ngap) %>% 
  filter(is.na(nonngap)) %>% 
  bind_rows(
    dat %>%
      arrange(nonngap, ngap) %>% 
      filter(!is.na(nonngap))
    ) %>% 
  mutate(id = 1:n(),
         nonngap = ifelse(nonngap == 0 & ngap == 0, 10, nonngap),
         ngap = ifelse(is.na(nonngap), 0, ngap),
         nonngap = ifelse(is.na(nonngap), 10, nonngap)) %>% 
  pivot_longer(nonngap:ngap) %>%
  mutate(name = ifelse(name == "ngap", "Yield gap from N factors", "Yield gap from other factors")) %>% 
  ggplot(aes(id, value)) + 
  geom_col(aes(fill = name), width = 1, color = "black") + 
  geom_vline(xintercept = 0, color = "gray50") +
  geom_vline(xintercept = 36.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 42.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 48.5, linetype = "dashed", color = "gray50") +
  geom_text(x = 18, y = 2600, label = "36 site-years, undetermined components",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 40, y = 2600, label = "6 site-years, no yield gap",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 46, y = 2600, label = "6 site-years, yield gap from N factors",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 90, y = 2600, label = "109 site-years, yield gap from\n  mixture of factors",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  scale_fill_manual(values = c(ylw1, ltbl1)) +
  scale_y_continuous(limits = c(0, 6000)) +
  theme(legend.justification = c(0,0),
        legend.position = c(0.05, 0.05),
        legend.background = element_blank()) +
#  theme(legend.position = "top") +
  labs(fill = NULL,
       y = "Yield gap between\ncontinuous- and rotated-maize (kg ha-1)",
       x = "Site-year") +
  coord_flip()

ggsave("05_manu-figs/fig_gap-components-windmill.png", height = 7)



