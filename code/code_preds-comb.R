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


# days to reach 140 -------------------------------------------------------

saw_cgap %>% 
  filter(cgap_max > -1000) %>% 
  left_join(wea) %>% 
  ggplot(aes(ndays_gdd140, cgap_max)) + 
  geom_point(aes(color = site)) + 
  geom_smooth(method = "lm") + 
  facet_grid(.~site)


saw_cgap %>% 
  filter(cgap_max > -1000) %>% 
  left_join(wea) %>% 
  ggplot(aes(ndays_gdd140, cgap_max)) + 
  geom_point() + 
  geom_smooth(method = "lm")



# ML ----------------------------------------------------------------------


# try a decision tree -----------------------------------------------------

library(rpart) # Decision tree package
library(partykit)
library(tree)
library(randomForest)
#library(gbm)
#library(caret)


# need to think about how to evaluate this. What makes it more likely to fall into a WW category?
dat <-  
  saw_cgap %>%
  filter(cgap_max > -1500) %>% 
  left_join(wea) %>% 
  left_join(soi) 


ydat <- 
  dat %>% 
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

prune_tree <- prune.tree(f_tree, best = 3)
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
best.dims
# Use other methods, see what they say...
#sebars <- selectNcomp(pls_tmp, method = "onesigma", plot = TRUE)
#targets <- selectNcomp(pls_tmp, method = "randomization", plot = TRUE)

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


dat %>% 
  ggplot(aes(prep2wk_rain_tot, cgap_max)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_grid(.~site)

# lasso/RR ----------------------------------------------------------------

library(glmnet)

ydatsc

# Scale and center predictors
pred_tmp <-
  ydat %>%
  select(-cgap_max, -year) %>%
  select_if(is.numeric) %>%
  mutate_all(funs(scale))

myr_tmp <- ydat %>%
  select(cgap_max) %>%
  mutate(cgap_max = cgap_max / 1000)

sdat_tmp <- bind_cols(myr_tmp, pred_tmp)
  
# Ensure no na, this might be trouble for the places w/o WT data....
sdat_tmp <- na.omit(sdat_tmp)
  
# Make predictor matrix
myx_tmp <- model.matrix(cgap_max~., sdat_tmp)[,-1] # trim off the first column
  
# Make response vector
myy_tmp <- 
  sdat_tmp %>%
    select(cgap_max) %>%
    unlist() %>%
    as.numeric()


# ridge regression --------------------------------------------------------

#--Fit a ridge regression model (alpha = 0) on a bunch of lambda values
#--Range of 10^-2 to 10^10
grid <- 10^seq(10, -2, length = 100)
set.seed(951983)
myRR_lam <- glmnet(myx_tmp, myy_tmp, alpha = 0, lambda = grid)
  
#dim(coef(myRR_lam))
#plot(myRR_mod)    # Draw plot of coefficients, not sure how informative this is
  
set.seed(951983)
myRR_cv <- cv.glmnet(myx_tmp, myy_tmp, alpha = 0) # Fit ridge regression model on 'training' data
bestlam <- myRR_cv$lambda.min  # Select lamda that minimizes training MSE
bestlam
myRR_RMSE <- sqrt(myRR_cv$cvm[myRR_cv$lambda == bestlam])

#plot(cv.out) # Draw plot of training MSE as a function of lambda

set.seed(951983)
myRR_mod <-  glmnet(myx_tmp, myy_tmp, alpha = 0) # Fit ridge regression model on full dataset
myRR_coef <-  predict(myRR_mod, type = "coefficients", s = bestlam) # Display coefficients using lambda chosen by CV

# RR results summary
myRR_res <- myRR_coef[-1, 1] %>%
  bind_rows() %>%
  gather() %>%
  mutate(RR_absval = abs(value),
         RR_RMSE = myRR_RMSE) %>%
  rename(RR_value = value,
         pred = key) %>%
  arrange(-RR_absval)


# LASSO -------------------------------------------------------------------

set.seed(951983)
myL_cv <-
  cv.glmnet(myx_tmp, myy_tmp, alpha = 1) # Fit lasso model on training data
plot(myL_cv) # Draw plot of training MSE as a function of lambda
bestlam <-
  myL_cv$lambda.min # Select lamda that minimizes training MSE
myL_RMSE <- sqrt(myL_cv$cvm[myL_cv$lambda == bestlam])

myL_mod <-
  glmnet(myx_tmp, myy_tmp, alpha = 1, lambda = grid) # Fit lasso model on full dataset
myL_coef = predict(myL_mod, type = "coefficients", s = bestlam) # Display coefficients using lambda chosen by CV
myL_coef

# LASSO results summary
myL_res <- myL_coef[-1, 1] %>%
  bind_rows() %>%
  gather() %>%
  mutate(LSO_absval = abs(value),
         LSO_RMSE = myL_RMSE) %>%
  rename(LSO_value = value,
         pred = key) %>%
  arrange(-LSO_absval)


# look at RR and LASSO ----------------------------------------------------

myres <- left_join(myRR_res, myL_res) %>%
    select(pred, RR_value, RR_absval, everything())
  

#--interesting. so hot planting-V12 = bigger gap
#                  extended emergence (cold, wet) = bigger gap
#                  colder winter = smaller gap
#                  warm gs = lower gap (??), this might be indicative of something else....

myres %>% 
  ggplot(aes(reorder(pred, LSO_value, mean), LSO_value)) + 
  geom_col() + 
  coord_flip()

getridof <- myres %>% 
  filter(LSO_value == 0) %>% 
  select(pred)

#--sooo higher p2mo_tx_mean and ndays_gdd140 means bigger gap
#--     lower gs_tavg and wintcolddays_n means lower gap

