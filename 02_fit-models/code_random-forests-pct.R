# Created:       4/10/2020
# 
# purpose: Look at random forests
#
# notes: 
#
# last edited: 4/20/2020 use new 'master' dataset
#              4/23/2020 try running on pct yield gaps instead, and years > 2005 (when we have balanced data)
#              5/4/2020 make partial dep plots for heat and rain, might want to get rid of year 2000?


rm(list = ls())
library(saapsim) #--has functios
library(tidysawyer2) #--has data
library(tidyverse)
library(lubridate)


# master data -------------------------------------------------------------

dat <- read_csv("01_create-features/cf_preds-all.csv")

# try a decision tree -----------------------------------------------------

library(randomForest)
library(iml)

#--prep data by removing things we don't want in the tree
ccgap <- 
  dat %>% 
  filter(year > 2000) %>% #--this year is an outlier in the exp data, always high gaps in sims
  ungroup() %>% 
  select(-cgap_max) %>% #--only want pct
  select(-crop, -site, -year, -yearF) %>%
  select(-avg_yield) %>% 
  select(-p2mo_gdd) %>% 
  select(-pre2wkp2wk_tl_mean) %>% 
  select(-paw_150cm_mm) %>% 
  filter(!is.na(prev_ccyield))

ccgap <- na.omit(ccgap)

library(corrplot)

dat_cor <- 
  ccgap %>%
  ungroup() %>% 
  select_if(is.numeric) 
corres <- cor(dat_cor, use="complete.obs")
corrplot::corrplot.mixed(corres)
corrplot::corrplot(corres)



gap_mod <- randomForest::randomForest(x = ccgap %>% dplyr::select(-cgap_max_pct),
                                      y = ccgap$cgap_max_pct)



# feature importance ------------------------------------------------------

# Create the predictor 
# (seemingly FeatureImp 
# requires y)
gap_pred = 
  Predictor$new(
    model = gap_mod, 
    data = ccgap, 
    y = ccgap$cgap_max_pct)
# Compute the feature
# importance values
gap_imp = 
  FeatureImp$new(
    predictor = gap_pred, 
    loss = 'mae')
# Plot the feature
# importance values
plot(gap_imp)

# look at it --------------------------------------------------------------

# Create a "predictor" object that 
# holds the model and the data
gap_pred <- 
  Predictor$new(
    model = gap_mod,
    data = ccgap)
# Compute the partial dependence 
# function for temp and windspeed
# takes a looooooooong time
pdp <- 
  FeatureEffect$new(
    predictor = gap_pred, 
    feature = c("heatstress_n", "prep2wk_precip_mm_tot"), 
    method = "pdp") 
# Create the partial dependence plot
pdp$plot() +
  viridis::scale_fill_viridis(
    option = "D") + 
  labs(fill = "Prediction")

ggsave("figs/stats_rf-pdpnd-pct.png")


ice = 
  FeatureEffect$new(
    predictor = gap_pred,
    feature = "p2mo_gdd",
    method = "ice")
plot(ice)

# try just > 2005 years ---------------------------------------------------

#--prep data by removing things we don't want in the tree
ccgap2 <- 
  dat %>% 
  ungroup() %>%
  filter(year > 2005) %>% 
  select(-cgap_max) %>% #--only want pct
  select(-crop, -site, -year, -yearF) %>%
  select(-avg_yield) %>% 
  select(-ndays_gdd140) %>% 
  filter(!is.na(prev_ccyield))

ccgap2 <- na.omit(ccgap2)


gap_mod2 <- randomForest::randomForest(x = ccgap2 %>% dplyr::select(-cgap_max_pct),
                                      y = ccgap2$cgap_max_pct)



gap_pred2 <-  
  Predictor$new(
    model = gap_mod, 
    data = ccgap2, 
    y = ccgap2$cgap_max_pct)
# Compute the feature
# importance values
gap_imp2 <-  
  FeatureImp$new(
    predictor = gap_pred2, 
    loss = 'mae')
# Plot the feature
# importance values
plot(gap_imp2)



# try other packages ------------------------------------------------------
library(tree)

f_tree <- tree::tree(cgap_max_pct ~., ccgap)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

cv_tree <- tree::cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b') #--this is terrible

prune_tree <- tree::prune.tree(f_tree, best = 6)
plot(prune_tree)
text(prune_tree, pretty = 0)

#--shows iacsr, prep2wk_precip_mm are consistently most important
#--similar results