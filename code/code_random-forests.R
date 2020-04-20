# Created:       4/10/2020
# 
# purpose: Look at random forests
#
# notes: 
#
# last edited: 4/20/2020 use new 'master' dataset


rm(list = ls())
library(saapsim) #--has functios
library(tidysawyer2) #--has data
library(tidyverse)
library(lubridate)


# master data -------------------------------------------------------------

dat <- read_csv("data/tidy/td_preds.csv") %>% 
  mutate(year = paste0("Y", year)) #--to ensure it isn't numeric

#--is iacsr and site-index related? yes
dat %>% 
  ggplot(aes(iacsr, avg_yield)) + 
  geom_point()

#--is A horizon depth related to ste-index? not really
dat %>% 
  ggplot(aes(bhzdepth_cm, avg_yield)) + 
  geom_point()


# what is too correlated -------------------------------------------

#--can only do numeric things
dat_cor <- 
  dat %>%
  ungroup() %>% 
  select_if(is.numeric) 
corres <- cor(dat_cor, use="complete.obs")
#corrplot::corrplot.mixed(corres)
corrplot::corrplot(corres)

#hmm. Some correlation problems. 
# paw vs clay
# iacsr vs avg_yield
# p2mo_gdd vs ndays_gdd140
# pre2wkp2wk_tl_mean

# random forests are more robust to things being correlated, I believe. 
#https://stats.stackexchange.com/questions/141619/wont-highly-correlated-variables-in-random-forest-distort-accuracy-and-feature


# try a decision tree -----------------------------------------------------

library(randomForest)
library(iml)

#--prep data by removing things we don't want in the tree
ccgap <- dat %>% 
  ungroup() %>% 
  select(-crop, -site, -year) %>%
  select(-avg_yield) %>% 
  select(-ndays_gdd140) %>% 
  filter(!is.na(prev_ccyield))

ccgap <- na.omit(ccgap)


gap_mod <- randomForest::randomForest(x = ccgap %>% dplyr::select(-cgap_max),
                                      y = ccgap$cgap_max)



# look at it --------------------------------------------------------------

# Create a "predictor" object that 
# holds the model and the data
gap_pred = 
  Predictor$new(
    model = gap_mod,
    data = ccgap)
# Compute the partial dependence 
# function for temp and windspeed
# takes a looooooooong time
pdp = 
  FeatureEffect$new(
    predictor = gap_pred, 
    feature = c("iacsr", "prep2wk_precip_mm_tot"), 
    method = "pdp") 
# Create the partial dependence plot
pdp$plot() +
  viridis::scale_fill_viridis(
    option = "D") + 
  labs(fill = "Prediction")




# feature importance ------------------------------------------------------

# Create the predictor 
# (seemingly FeatureImp 
# requires y)
gap_pred = 
  Predictor$new(
    model = gap_mod, 
    data = ccgap, 
    y = ccgap$cgap_max)
# Compute the feature
# importance values
gap_imp = 
  FeatureImp$new(
    predictor = gap_pred, 
    loss = 'mae')
# Plot the feature
# importance values
plot(gap_imp)


gap_imp_dat <- 
  gap_imp$results %>% 
  as_tibble() %>% 
  arrange(-importance) %>% 
  filter(importance > 1.1) %>% 
  select(feature, importance)

#--I can use these top 10ish features in the LASSOs/PLSs  
gap_imp_dat %>% 
  write_csv("data/smy/sd_rf-feat-imp.csv")




# Compute the ICE function
ice = 
  FeatureEffect$new(
    predictor = gap_pred,
    feature = "p2mo_gdd",
    method = "ice")
# Create the plot
plot(ice)



# Center the ICE function for 
# temperature at the 
# minimum temperature and 
# include the pdp
ice_centered = 
  FeatureEffect$new(
    predictor = gap_pred,
    feature = "avg_ccyield",
    center.at = max(ccgap$avg_ccyield),
    method = "pdp+ice")
# Create the plot
plot(ice_centered)



# try other packages ------------------------------------------------------
library(tree)

f_tree <- tree::tree(cgap_max~., ccgap)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

cv_tree <- tree::cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b') #--this is terrible

prune_tree <- tree::prune.tree(f_tree, best = 6)
plot(prune_tree)
text(prune_tree, pretty = 0)

#--shows iacsr, prep2wk_precip_mm, and pre2skp2sk_tl_mean are most important
#--similar results