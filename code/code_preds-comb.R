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


wea <- read_csv("data/td_pred-wea.csv")  
soi <- read_csv("data/td_pred-soil.csv")


#--what does the corn suitability rating look like?
soi %>% 
  ggplot(aes(iacsr)) +
  geom_histogram()

#--add previous year's continuous corn yield as covariate?
prev_yield <- 
  saw_tidysawyer %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "cc") %>% 
  mutate(prev_ccyield = lag(yield_kgha)) %>% 
  select(site, year, prev_ccyield)

#--use 'site production index' instead of soil things?
avg_yield <- 
  saw_tidysawyer %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  #--make sure it's taken over the same time frame
  filter(year > 2006,
         year < 2017) %>% 
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "cc") %>% 
  summarise(avg_ccyield = mean(yield_kgha, na.rm = T)) %>% 
  select(site, avg_ccyield)

#--include # of years in corn

yrs_corn <- 
  saw_tidysawyer %>% 
  filter(rotation == "cc") %>% 
  group_by(site) %>% 
  group_modify(~{
    .x %>% mutate(years_in_corn = group_indices(., year))
  }) %>% 
  select(site, year, years_in_corn) %>% 
  distinct()
  


dat <-  
  saw_cgap %>%
  filter(cgap_max > -1500) %>% 
  left_join(prev_yield) %>% 
  left_join(avg_yield) %>% 
  left_join(yrs_corn) %>% 
  left_join(wea) %>% 
  left_join(soi) %>% 
  mutate(year = paste0("Y", year)) #--to ensure it isn't numeric


dat %>% 
  ggplot(aes(iacsr, avg_ccyield)) + 
  geom_point()


dat %>% 
  ggplot(aes(bhzdepth_cm, avg_ccyield)) + 
  geom_point()



dat_cor <- 
  dat %>%
  ungroup() %>% 
  select_if(is.numeric) 
corres <- cor(dat_cor, use="complete.obs")
#corrplot::corrplot.mixed(corres)
corrplot::corrplot(corres)

#hmm. Some correlation problems. Get rid of PAW? Or clay and soc?

# # summarize ---------------------------------------------------------------
library(gt)

#https://dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/
library(psych)
dat_sum <- psych::describe(dat)

dat_vars <- rownames(dat_sum)

dat_sum_tib <- 
  dat_sum %>%
  as_tibble() %>% 
  mutate(vars = dat_vars) %>% 
  select(vars, min, max, mean) %>% 
  filter(!is.infinite(min)) %>% 
  mutate_if(is.numeric, round, 0)

gt(dat_sum_tib)

# 
# saw_cgap %>% 
#   #group_by(site) %>% 
#   #summarise(cgap_max = median(cgap_max)) %>% 
#   filter(cgap_max > -1000) %>% 
#   left_join(soi) %>% 
#   left_join(wea) %>%
#   pivot_longer(wtdepth_cm:heatstress_n) %>% 
#   ggplot(aes(value, cgap_max)) + 
#   geom_point(aes(color = site)) + 
#   geom_smooth(method = "lm", se = F, color = "red") +
#   facet_wrap(~name, scales = "free")
# 
# 
# # days to reach 140 -------------------------------------------------------
# 
# saw_cgap %>% 
#   filter(cgap_max > -1000) %>% 
#   left_join(wea) %>% 
#   ggplot(aes(ndays_gdd140, cgap_max)) + 
#   geom_point(aes(color = site)) + 
#   geom_smooth(method = "lm") + 
#   facet_grid(.~site)
# 
# 
# saw_cgap %>% 
#   filter(cgap_max > -1000) %>% 
#   left_join(wea) %>% 
#   ggplot(aes(ndays_gdd140, cgap_max)) + 
#   geom_point() + 
#   geom_smooth(method = "lm")



# ML ----------------------------------------------------------------------


# try a decision tree -----------------------------------------------------

library(rpart) # Decision tree package
library(partykit)
library(tree)
library(randomForest)
#library(gbm)
#library(caret)


ydat <- 
  dat %>% 
  ungroup() %>% 
  select_if(is.numeric)

ydatsc <- ydat %>% 
  mutate_if(is.numeric, scale)

ydat <- na.omit(ydat)


f_tree <- tree::tree(cgap_max~., ydat)
summary(f_tree)
plot(f_tree)
text(f_tree, pretty = 0)

cv_tree <- tree::cv.tree(f_tree)
plot(cv_tree$size, cv_tree$dev, type = 'b') #--this is terrible

prune_tree <- tree::prune.tree(f_tree, best = 3)
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
  ggplot(aes(prep2wk_precip_mm_tot, cgap_max)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_grid(.~site)

# lasso/RR ----------------------------------------------------------------

library(glmnet)

ydatsc

# Scale and center predictors
pred_tmp <-
  ydat %>%
  select(-cgap_max) %>%
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
  filter(LSO_value != 0) %>% 
  mutate(pred = recode(pred,
                       p2mo_tx_mean = "Mean Max T planting-V12",
                       wintcolddays_n = "Number of Days <-15 Before Planting",
                       prep2wk_precip_mm_tot = "Amount of Rain 2 weeks Before Planting",
                       gs_tavg = "April 1 - Sept 1 Average Temperature",
                       bhzdepth_cm = "Depth to B Horizon",
                       wtdepth_cm = "Depth to Water Table",
                       p4wk_1inrain = "Days planting-1 mo with 1 inch of rain",
                       soc_30cm_pct = "Soil Carbon in top 30 cm",
                       ndays_gdd140 = "Number of DAP to reach 140GDD",
                       iascr = "Iowa Corn Suitability Rating",
                       avg_ccyield = "Site Average Corn Yield")
  ) %>% 
  mutate(effect = ifelse(LSO_value < 0, "Drives Penalty Smaller", "Drives Penalty Larger")) %>% 
  ggplot(aes(reorder(pred, LSO_value, mean), LSO_value)) + 
  geom_col(aes(fill = effect)) + 
  coord_flip() + 
  scale_fill_manual(values = c("red", "blue")) +
  labs(y = "Effect on Penalty", 
       fill = NULL,
       x = NULL,
       title = "LASSO Regression, What Drives Continuous Corn Penalty?")

ggsave("figs/stats_lasso.png")
ggsave("../../../Box/Gina_APSIM_modeling/figs-from-repo/stats_lasso.png")

myres %>% 
  ggplot(aes(reorder(pred, RR_value, mean), RR_value)) + 
  geom_col() + 
  coord_flip()



#--sooo higher p2mo_tx_mean and ndays_gdd140 means bigger gap
#--     lower gs_tavg and wintcolddays_n means lower gap

