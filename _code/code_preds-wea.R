# Created:       3/25/2020
# last edited:   3/25/2020
#                3/27/2020 (include planting dates)
#                3/30/2020 (added planting-date-referenced weather)
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

pwea <- wea %>% 
  left_join(sad_plant) %>% 
  select(-plant_date, -crop) 

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

library(GGally)
# wealt %>% 
#   pivot_longer(radn_15y:rain_15y) %>% 
#   ggplot(aes(reorder(site, value, mean), value)) +
#   geom_point(size = 5) +
#   coord_flip() +
#   facet_grid(~name, scales = "free")

# lets me look at all weather relations at once
GGally::ggparcoord(data = wealt, columns = 2:5, groupColumn = 1, order = "skewness") +
  geom_line(size = 2, alpha = 0.5)



# weather in reference to planting ----------------------------------------

#--rain tot 2 weeks after planting
wea_2wkp <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 14) %>% 
  summarise(p2wk_rain_tot = sum(rain))

wea_3wkp <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 21) %>% 
  summarise(p3wk_rain_tot = sum(rain))

wea_4wkp <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 28) %>% 
  summarise(p4wk_rain_tot = sum(rain))

#--2 weeks before planting
wea_2wkpp <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 14) %>% 
  summarise(prep2wk_rain_tot = sum(rain))

wea_3wkpp <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 21) %>% 
  summarise(prep3wk_rain_tot = sum(rain))

wea_4wkpp <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 28) %>% 
  summarise(prep4wk_rain_tot = sum(rain))

#--avg high temp in the month 2 months after planting
wea_8wkt <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy + 56, 
         day < plant_doy + 56 + 28) %>% 
  summarise(p2mo_tx_mean = mean(maxt))


#--avg low temp in the month surrounding planting
wea_4wkt <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy - 14, 
         day < plant_doy + 14) %>% 
  summarise(pre2wkp2wk_tl_mean = mean(mint))

#--number of days < 4degF (-15C) before planting
wea_cold <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy) %>% 
  filter(mint < -15) %>% 
  summarise(wintcolddays_n = n())



# weather metrics ---------------------------------------------------------

#--rafa's scirep paper
#--teasdale and cavigelli 2017 paper (always in reference to planting....)
#--others?
#--sa uses rainy days > 1inch, growing season GDD, heat days (Tmax > 34C), cold days (Tmin < 4), rad, avg T



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


wea_parms <- 
  weasy %>% 
  left_join(wea_2wkp) %>% 
  left_join(wea_2wkpp) %>% 
  left_join(wea_3wkp) %>% 
  left_join(wea_3wkpp) %>% 
  left_join(wea_4wkp) %>% 
  left_join(wea_4wkpp) %>% 
  left_join(wea_8wkt) %>%
  left_join(wea_4wkt) %>% 
  left_join(wea_cold) %>% 
  left_join(wea_july) %>%
  left_join(wea_may) %>% 
  left_join(wea_apr) %>% 
  left_join(wea_aprmay) %>% 
  left_join(wea_gs) %>% 
  left_join(wea_hs) 


wea_parms %>% write_csv("_data/td_pred-wea.csv")  
