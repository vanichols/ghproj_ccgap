# Created:       4/1/2020
# last edited:   
# 
# purpose: Create pred summary (soil and wea)
#
# notes: 

rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--has functios
library(tidysawyer2) #--has data
library(tidyverse)
library(lubridate)



# data --------------------------------------------------------------------


wea <- read_csv("_data/td_pred-wea.csv")  
soi <- read_csv("_data/td_pred-soil.csv")


# summarize ---------------------------------------------------------------

saw_cgap %>% 
  #group_by(site) %>% 
  #summarise(cgap_max = median(cgap_max)) %>% 
  filter(cgap_max > -1000) %>% 
  left_join(soi) %>% 
  left_join(wea) %>%
  pivot_longer(wtdepth_cm:heatstress_n) %>% 
  ggplot(aes(value, cgap_max)) + 
  geom_point(aes(color = site)) + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_wrap(~name, scales = "free")

