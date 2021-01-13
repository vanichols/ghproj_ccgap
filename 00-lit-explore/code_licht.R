# goal: look at mark's data
# created: 12/8/2020
# updated: 1/13/2021
# notes:

rm(list = ls())

library(tidyverse)
library(readxl)


# data --------------------------------------------------------------------

datraw <- read_csv("00-lit-explore/td_licht.csv")

#--viz
datraw %>% 
  ggplot(aes(Tillage, PlDenAve)) + 
  stat_summary(aes(color = PrevCrop, group = PrevCrop, pch = PrevCrop), size = 2) + 
  scale_color_manual(values = c("gold3", "green4")) + 
  labs(y = "Corn stand density (pl/ac)",
       subtitle = "Licht data, 11-site years",
       title = "Rotation effect on stand count depends on tillage")

ggsave("00-lit-explore/fig_licht.png")
  
datraw %>% 
    pull(SiteYear) %>% 
    unique()

  #--ave amount less
datraw %>% 
  filter(Tillage %in% c("NT", "ST")) %>% 
  select(SiteYear, PrevCrop, Tillage, Rep, PlDenAve) %>% 
  pivot_wider(names_from = PrevCrop, values_from = PlDenAve) %>% 
  mutate(pct_diff = (Corn - Soybean)/Soybean * 100) %>% 
  summarise(pct_diff = mean(pct_diff, na.rm = T))

