# Created:       3/25/2020
# last edited:   3/25/2020
#                3/27/2020 (include planting dates)
#                3/30/2020 (added planting-date-referenced weather)
#                4/8/2020 (add Alison suggestions, play w/machine learning)
# 
# purpose: Create pred tibble for weather variables
#
# notes: 


# 
# The set of weather covariates for corn was as follows: aver-
#   age vapor pressure deficit (VPD) from 61 to 90 d after plant- ing, maximum air temperature 91 to 120 d after sowing, maximum air temperature in the 30 d leading up to planting, and precipitation 31 to 60 d after planting. For


rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2", force = T)
#library(saapsim) #--my package, has some soil data in it, not needed here?
library(tidysawyer2)
library(tidyverse)
library(lubridate)


# weather data, from package ----------------------------------------------

wea <- saw_wea %>% as_tibble() 

pwea <- wea %>% 
  left_join(saw_plant) %>% 
  select(-plant_date, -crop) 

#--check, all sites/years?
pwea %>% 
  select(site, year) %>% 
  distinct() 


# #--look at 15-year averages (I know this isn't right, but it's ok for now) across sites
# wealt <- 
#   weasy %>% 
#   group_by(site) %>% 
#   summarise(radn_15y = mean(radn),
#             maxt_15y = mean(maxt),
#             mint_15y = mean(mint),
#             rain_15y = mean(rain))
# 
#library(GGally)
# wealt %>% 
#   pivot_longer(radn_15y:rain_15y) %>% 
#   ggplot(aes(reorder(site, value, mean), value)) +
#   geom_point(size = 5) +
#   coord_flip() +
#   facet_grid(~name, scales = "free")

# lets me look at all weather relations at once
#GGally::ggparcoord(data = wealt, columns = 2:5, groupColumn = 1, order = "skewness") +
#  geom_line(size = 2, alpha = 0.5)



# weather in reference to planting ----------------------------------------

#--days w/max temp < 50 up to 30 days after planting
wea1 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 28) %>%
  filter(maxt <= 10) %>% #--temp is in C
  summarise(p4wk_less50_maxt = n())

wea1 %>% 
  ggplot(aes(p4wk_less50_maxt)) + 
  geom_histogram()


#--days w/avg temp < 50 up to 30 dap
wea2 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy,
         day < plant_doy + 30) %>% 
  mutate(tav = (maxt + mint)/2) %>% 
  filter(tav < 10) %>% 
  summarise(wintcolddays_n = n())

#--rainy days >1" from planting to 60 DAP
wea3 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 30) %>%
  filter(rain >= 25.4) %>% #--rain more than an inch
  summarise(p4wk_1inrain = n())

wea3 %>% 
  ggplot(aes(p4wk_1inrain)) + 
  geom_histogram()

#--days to reach 140
wea4 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy) %>% 
  mutate(gdd = (maxt + mint)/2 - 10,
         gdd_cum = cumsum(gdd)) %>% 
  filter(gdd_cum < 140) %>% 
  summarise(ndays_gdd140 = n())

wea4 %>% 
  ggplot(aes(ndays_gdd140)) + 
  geom_histogram()

#--rain tot 2 weeks after planting
wea5 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 14) %>% 
  summarise(p2wk_rain_tot = sum(rain))

wea6 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 21) %>% 
  summarise(p3wk_rain_tot = sum(rain))

wea7 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 28) %>% 
  summarise(p4wk_rain_tot = sum(rain))

#--2 weeks before planting
wea8 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 14) %>% 
  summarise(prep2wk_rain_tot = sum(rain))

wea9 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 21) %>% 
  summarise(prep3wk_rain_tot = sum(rain))

wea10 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 28) %>% 
  summarise(prep4wk_rain_tot = sum(rain))

#--avg high temp in the month 2 months after planting
wea11 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy + 56, 
         day < plant_doy + 56 + 28) %>% 
  summarise(p2mo_tx_mean = mean(maxt))


#--avg low temp in the month surrounding planting
wea12 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy - 14, 
         day < plant_doy + 14) %>% 
  summarise(pre2wkp2wk_tl_mean = mean(mint))

#--number of days < 4degF (-15C) before planting
wea13 <- 
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
  wea_gs %>% 
  left_join(wea1) %>% 
  left_join(wea2) %>% 
  left_join(wea3) %>% 
  left_join(wea4) %>% 
  left_join(wea5) %>% 
  left_join(wea6) %>% 
  left_join(wea7) %>% 
  left_join(wea8) %>% 
  left_join(wea9) %>%
  left_join(wea10) %>% 
  left_join(wea11) %>% 
  left_join(wea12) %>% 
  left_join(wea13) %>% 
  #left_join(wea_july) %>% #--I kind of don't like the things not referenced to planting
  #left_join(wea_may) %>% 
  #left_join(wea_apr) %>% 
  #left_join(wea_aprmay) %>%
  left_join(wea_hs) %>%
  ungroup() %>% 
  mutate_if(is.numeric, list(~replace_na(., 0)))

# what is correlated? -----------------------------------------------------
library(corrplot)

wea_cor <- 
  wea_parms %>%
  ungroup() %>% 
  select_if(is.numeric) 
corres <- cor(wea_cor, use="complete.obs")
corrplot::corrplot.mixed(corres)
corrplot::corrplot(corres)

# write what i like -------------------------------------------------------

wea_parms %>% write_csv("_data/td_pred-wea.csv")  


# try a decision tree -----------------------------------------------------

library(rpart) # Decision tree package
library(partykit)
library(tree)
library(randomForest)
#library(gbm)
#library(caret)


# need to think about how to evaluate this. What makes it more likely to fall into a WW category?
ydat <-  
  saw_cgap %>% 
  left_join(wea_parms) %>% 
  ungroup() %>% 
  select_if(is.numeric)

ydatsc <- ydat %>% 
  mutate_if(is.numeric, scale)

ydat <- na.omit(ydat)


f_tree <- tree::tree(cgap_max~., ydatsc)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

cv_tree <- cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b') #--this is terrible

prune_tree <- prune.tree(f_tree, best = 2)
plot(prune_tree)
text(prune_tree, pretty = 0)

# pls ---------------------------------------------------------------------
library(pls)
library(caret)

# Fit a PLS model on CN ratios
plsm <- plsr(cgap_max ~., data = ydatsc, validation = "LOO")

# Find the number of dimensions with lowest cross validation error
cv <- RMSEP(plsm)
best.dims = which.min(cv$val[estimate = "adjCV", , ]) - 1

# Use other methods, see what they say...
sebars <- selectNcomp(pls_tmp, method = "onesigma", plot = TRUE)
targets <- selectNcomp(pls_tmp, method = "randomization", plot = TRUE)


# Rerun the model
plsmn <- plsr(cgap_max ~., data = ydatsc, ncomp = best.dims)

# Code copied from Ranae, stupid rownames
varImp(plsmn) %>%
  rownames_to_column() %>%
  rename(var = rowname,
         imp = Overall) %>% 
  ggplot(aes(x = reorder(var, imp), y = imp)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("PLS")
