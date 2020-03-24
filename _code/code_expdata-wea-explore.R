# Created:       3/23/2020
# last edited:   3/24/2020
# 
# purpose: look at yield gaps compard to weather (year explains more variance than site in JS data)
#
# notes: are the gaps related to weather? ie disease? I tried the soil N thing, didn't work


rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has his data in it :P
library(tidyverse)
library(lubridate)
library(corrplot)
library(plotly)


# cheater for doy ---------------------------------------------------------

fun_doy <- function(mydate = "2001-01-01"){
  
  doy_tib0 <- tibble(
    date = seq.Date(from = as.Date("2001-01-01"), to = as.Date("2001-12-31"), by = "day"))
  
  daysinyear <- nrow(doy_tib0)
  
  doy_tib <- doy_tib0 %>% 
    mutate(doy = 1:daysinyear)
  
  res <- doy_tib %>% 
    filter(date == mydate)
 
   return(res)
}

#--scale wants matrices....
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


#--data

wea <- sad_wea %>% as_tibble() %>% 
  mutate(year = paste0("Y", year))


#--yearly weather
weay <- wea %>% 
  group_by(year) %>%
  summarise(radn = sum(radn, na.rm = T),
            maxt = mean(maxt, na.rm = T),
            mint = mean(mint, na.rm = T),
            rain = sum(rain, na.rm = T)) 



#--site yearly weather
weasy <- wea %>% 
  group_by(site, year) %>%
  summarise(radn = sum(radn, na.rm = T),
            maxt = mean(maxt, na.rm = T),
            mint = mean(mint, na.rm = T),
            rain = sum(rain, na.rm = T)) 
  

#--look at averages across sites
wealt <- 
  weasy %>% 
  group_by(site) %>% 
  summarise(radn_lt = mean(radn),
            maxt_lt = mean(maxt),
            mint_lt = mean(mint),
            rain_lt = mean(rain))


wealt %>% 
  pivot_longer(radn_lt:rain_lt) %>% 
  ggplot(aes(reorder(site, value, mean), value)) +
  geom_point(size = 5) +
  coord_flip() +
  facet_grid(~name, scales = "free")




# weather metrics ---------------------------------------------------------

#--rafa's scirep paper
#--teasdale and cavigelli 2017 paper (always in reference to planting)
#--others?


#--when is july? doy 182-212
#--may? 121-150
doy_tib %>% 
  filter(date == "2001-05-30")

#--mean july max temp
wea_july <- 
  wea %>% 
  filter(day < 212, day > 181) %>% 
  group_by(site, year) %>% 
  summarise(julymint_mean = mean(mint),
            julymaxt_mean = mean(maxt),
            julyrain_tot = sum(rain))

wea_may <- 
  wea %>% 
  filter(day < 151, day > 121) %>% 
  group_by(site, year) %>% 
  summarise(maymint_mean = mean(mint),
            maymaxt_mean = mean(maxt),
            mayrain_tot = sum(rain))

wea_apr <- 
  wea %>% 
  filter(day < 120, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(arpmint_mean = mean(mint),
            arpmaxt_mean = mean(maxt),
            arprain_tot = sum(rain))

wea_gs <- 
  wea %>% 
  filter(day < 244, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(gsrain_tot = sum(rain),
            gst = mean((maxt + mint)/2))

fun_doy("2001-05-01")

wea_hs <- 
  wea %>% 
  filter(day < 244, day > 91) %>%
  filter(maxt > 30) %>% 
  group_by(site, year) %>% 
  summarise(heatstress_n = n())


wea_parms <- 
  weasy %>% 
  left_join(wea_july) %>%
  left_join(wea_may) %>% 
  left_join(wea_gs) %>% 
  left_join(wea_hs)


# combine cgap and wea ----------------------------------------------------

gap_wea <- 
  sad_cgap %>% 
  filter(cgap_max > 0) %>% 
  mutate(year = paste("Y", year)) %>% 
  left_join(wea_parms) %>% 
  ungroup()


gap_wea_year <- 
  sad_cgap %>% 
  filter(cgap_max > 0) %>%
  group_by(year) %>% 
  summarise(cgap_max = mean(cgap_max)) %>% 
  left_join(weay) %>% 
  ungroup() %>% 
  mutate(year = paste0("Y", year)) %>%
  mutate_if(is.numeric, scale_this)


# should look at corrs at some point --------------------------------------


gap_cor <- gap_wea_year %>%
  select_if(is.numeric)

corres <- cor(gap_cor, use="complete.obs")
corrplot::corrplot(corres, type = "lower")
corrplot::corrplot.mixed(corres)


p1 <- gap_wea %>% 
  ggplot(aes(radn, cgap_max)) +
  geom_point(aes(color = site)) + 
  facet_wrap(~year, scales = "free")
ggplotly(p1)

gap_wea %>% 
  pivot_longer(radn:rain) %>% 
  ggplot(aes(cgap_max, value)) + 
  geom_point(size = 5) + 
  geom_smooth(method = "lm") +
  facet_wrap(~name, scales = "free")


#--biplot of max t and rain
gap_wea %>% 
  ggplot(aes(rain, maxt)) + 
  geom_point(aes(size = cgap_max))





# try pls -----------------------------------------------------------------


# Preds
prd_tmp <- gap_wea %>%
  ungroup() %>% 
  select(-crop, -site, -year, -cgap_max) %>%
  mutate_all(funs(scale))

myr_tmp <- gap_wea %>%
  select(cgap_max)

sdat_tmp <- bind_cols(myr_tmp, prd_tmp) %>%
  replace(is.na(.), NA)


library(pls)
library(caret)

# Code copied from blog....
#~~~~~~~~~~~~~~~~~~~~~
set.seed(951983)
# Fit a PLS model on d90
# plsr = partial least squares regression
pls_tmp <- plsr(cgap_max ~., data = sdat_tmp, validation = "LOO")

# Find the number of dimensions with lowest cross validation error
# RMSEP = root mean squared error prediction
cvs_tmp <- RMSEP(pls_tmp)
generous <- which.min(cvs_tmp$val[estimate = "adjCV", , ]) - 1 # Subtract 1 bc it's counting the intercept

# Use other methods
sebars <- selectNcomp(pls_tmp, method = "onesigma", plot = TRUE)
targets <- selectNcomp(pls_tmp, method = "randomization", plot = TRUE)

mylow <- 2

# Rerun the model
pls_tmp2 <- plsr(cgap_max ~., data = sdat_tmp, ncomp = mylow) # I really only need 1 says lowsig

# Look at explained variance
explvar(pls_tmp2)

imps_tmp <- varImp(pls_tmp2) %>%
  rownames_to_column() %>%
  rename(var = rowname, 
         imp = Overall) %>%
  mutate(impT = sum(imp),
         imps = imp / impT * 100) 


# PLS says it's minT
library(lme4)
m1 <- lmer(cgap_max ~ radn*maxt*mint*rain + (1|site) + (1|year), data = gap_wea)
anova(m1)

library(pls
        
        
        
        
        