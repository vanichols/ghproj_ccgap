# Created:       3/25/2020
# last edited:   3/25/2020
# 
# purpose: Create pred tibble for weather variables
#
# notes: 

rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has his data in it :P
library(tidyverse)
library(lubridate)


# weather data, from package ----------------------------------------------

wea <- sad_wea %>% as_tibble() 


#--site yearly weather
weasy <- wea %>% 
  group_by(site, year) %>%
  summarise(radn = sum(radn, na.rm = T),
            maxt = mean(maxt, na.rm = T),
            mint = mean(mint, na.rm = T),
            rain = sum(rain, na.rm = T)) 
  

#--look at 15-year averages (I know this isn't right, but it's ok for now) across sites
wealt <- 
  weasy %>% 
  group_by(site) %>% 
  summarise(radn_15y = mean(radn),
            maxt_15y = mean(maxt),
            mint_15y = mean(mint),
            rain_15y = mean(rain))


wealt %>% 
  pivot_longer(radn_15y:rain_15y) %>% 
  ggplot(aes(reorder(site, value, mean), value)) +
  geom_point(size = 5) +
  coord_flip() +
  facet_grid(~name, scales = "free")


# weather metrics ---------------------------------------------------------

#--rafa's scirep paper
#--teasdale and cavigelli 2017 paper (always in reference to planting....)
#--others?


#--mean july max temp
wea_july <- 
  wea %>% 
  filter(day < 212, day > 181) %>% 
  group_by(site, year) %>% 
  summarise(jul_mint_mean = mean(mint),
            jul_maxt_mean = mean(maxt),
            jul_rain_tot = sum(rain))

wea_may <- 
  wea %>% 
  filter(day < 151, day > 121) %>% 
  group_by(site, year) %>% 
  summarise(may_mint_mean = mean(mint),
            may_maxt_mean = mean(maxt),
            may_rain_tot = sum(rain))

wea_apr <- 
  wea %>% 
  filter(day < 120, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(apr_mint_mean = mean(mint),
            apr_maxt_mean = mean(maxt),
            apr_rain_tot = sum(rain))

wea_aprmay <- 
  wea %>% 
  filter(day < 151, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(aprmay_mint_mean = mean(mint),
            aprmay_maxt_mean = mean(maxt),
            aprmay_rain_tot = sum(rain))


wea_gs <- 
  wea %>% 
  filter(day < 244, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(gs_rain_tot = sum(rain),
            gs_tavg = mean((maxt + mint)/2))


wea_hs <- 
  wea %>% 
  filter(day < 244, day > 91) %>%
  filter(maxt > 30) %>% 
  group_by(site, year) %>% 
  summarise(heatstress_n = n())



# weather w/respect to planting -------------------------------------------

#NEEDS UPDATED


wea_parms <- 
  weasy %>% 
  left_join(wea_july) %>%
  left_join(wea_may) %>% 
  left_join(wea_apr) %>% 
  left_join(wea_aprmay) %>% 
  left_join(wea_gs) %>% 
  left_join(wea_hs) 
  
