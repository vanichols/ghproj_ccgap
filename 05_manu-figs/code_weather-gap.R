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

ilia_planting <- 
  bind_rows(il_planting, ia_planting) %>% 
  select(site, year, plant_doy)

ilia_aonr <- read_csv("00_empirical-n-cont/dat_gap-components.csv")  %>% 
  select(site, year, nonngap) %>% 
  rename("gap" = nonngap)

#--just the top 2 biggest gap years
ilia_aonr2 <- 
  ilia_aonr %>%
  arrange(site, -gap) %>% 
    group_by(site) %>% 
    mutate(rank = 1:n()) %>% 
    filter(rank <3)
    


# precip before planting --------------------------------------------------

dat <- 
  ilia_wea %>%
  left_join(
ilia_aonr  
  ) %>% 
  filter(!is.na(gap)) %>% 
  left_join(
    ilia_planting
    ) %>% 
  mutate(dap = day - plant_doy) %>% 
  filter(dap > -15, dap < 30)



#--top2 years
dat_top2 <- 
  ilia_wea %>%
  arrange(site) %>% 
  left_join(
    ilia_aonr2  
  ) %>% 
  left_join(
    ilia_planting
  ) %>% 
  mutate(dap = day - plant_doy) 


#--around planting
dat_top2 %>% 
  filter(dap > -15, dap < 1) %>% 
  group_by(site, year) %>% 
  mutate(precip_csum = cumsum(precip_mm),
         gaptop2 = ifelse(is.na(gap), "N", "top2")) %>% 
  ggplot(aes(dap, precip_csum, group = year)) + 
  geom_line(aes(color = gaptop2), size = 1) + 
  geom_vline(xintercept = 0) + 
  facet_wrap(~site, scales = "free")

dat %>% 
  filter(dap > -15, dap < 1) %>% 
  group_by(site, year) %>% 
  mutate(precip_csum = cumsum(precip_mm)) %>% 
  ggplot(aes(dap, precip_csum, group = year)) + 
  geom_line(aes(color = gap/1000), size = 1) + 
  geom_vline(xintercept = 0) + 
  facet_wrap(~site, scales = "free") +
  scale_color_gradient(low = "red", high = "darkblue")



#--all season
dat_top2 %>% 
#  filter(dap > -15, dap < 15) %>% 
  group_by(site, year) %>% 
  mutate(precip_csum = cumsum(precip_mm),
         gaptop2 = ifelse(is.na(gap), "N", "top2")) %>% 
  ggplot(aes(dap, precip_csum, group = year)) + 
  geom_line(aes(color = gaptop2), size = 1) + 
  geom_vline(xintercept = 0) + 
  facet_wrap(~site, scales = "free")




# cold days ---------------------------------------------------------------

dat2 <- 
  ilia_wea %>%
  left_join(
    ilia_aonr  
  ) %>% 
  filter(!is.na(gap)) %>% 
  left_join(
    ilia_planting
  ) %>% 
  mutate(dap = day - plant_doy) %>% 
  filter(dap < 0)


dat2 %>% 
  mutate(cold = ifelse(tmin_c < -15, 1, 0)) %>% 
  group_by(site, year, gap) %>% 
  mutate(cold_csum = cumsum(cold)) %>% 
  ggplot(aes(day, cold_csum, group = year)) + 
  geom_line(aes(color = gap), size = 1) + 
  geom_vline(xintercept = 0) + 
  facet_wrap(~site, scales = "free") + 
  scale_color_viridis_c()



dat2 %>% 
  mutate(cold = ifelse(tmin_c < -15, 1, 0)) %>% 
  group_by(site, year, gap) %>% 
  summarise(cold_sum = sum(cold)) %>% 
  left_join(dat %>% 
              group_by(site, year) %>% 
              mutate(precip_csum = cumsum(precip_mm)) )%>% 
  ggplot(aes(dap, precip_csum, group = year)) + 
  geom_line(aes(color = gap, size = cold_sum)) + 
  geom_vline(xintercept = 0) + 
  facet_wrap(~site, scales = "free") + 
  scale_color_viridis_c()


#--is cold sum related to latitude? yes. 
dat2 %>% 
  mutate(cold = ifelse(tmin_c < -15, 1, 0)) %>% 
  group_by(site, year, gap) %>% 
  summarise(cold_sum = sum(cold))  %>% 
  left_join(ilia_siteinfo) %>% 
  ggplot(aes(lat, cold_sum)) + 
  geom_point()
