# goal: look at sotiris sims to see if water is reset
# created: 5/18/2021
# updated: 
#
# notes: 

rm(list = ls())

library(tidyverse)
library(tidysawyer2)
library(saapsim)
library(apsimx)


# get soil profile --------------------------------------------------------
#--fail
extd.dir <- paste0(getwd(), "/00_soil-water-reset-Qs")

inspect_apsim("ames-for-soil-prof.apsim", src.dir = extd.dir, 
              node = "Soil", soil.child = "Water") %>% 
  as_tibble()


depthcats <- 
  dat_cc2 %>% 
  select(depth_cm) %>% 
  distinct() %>% 
  mutate(
    n = 1:n(),
    dcat = paste0("d", n)) %>%
  select(-n) %>% 
  pivot_wider(names_from = dcat, values_from = depth_cm)


soi <- 
  readxl::read_excel("ames-soil-prof.xlsx") %>% 
  as_tibble() %>% 
  select(depth_cm, dul_mm) %>% 
  separate(depth_cm, into = c("di", "df"), sep = "-") %>% 
  mutate(davg = (as.numeric(di) + as.numeric(df))/2) 

dul <- 
  expand_grid(soi, depthcats) %>% 
  mutate(
    dcat = case_when(
      (davg <= d1+1) ~ d1,
      (davg >= d1+1 & davg <= d2) ~ d2,
      (davg >= d2 & davg <= d3) ~ d3,
      (davg >= d3 & davg <= d4) ~ d4,
      (davg >= d4 & davg <= d5) ~ d5,
      #davg >= depthcats[1] & davg < depthcats[2] ~ depthcats[2],
      TRUE ~ 999)
  ) %>% 
  filter(dcat != 999) %>% 
  group_by(dcat) %>% 
  summarise(dul_mm = mean(dul_mm)) %>% 
  rename("depth_cm" = dcat)


