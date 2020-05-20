# Created:       4/1/2020
# 
# purpose: stats on pct gap (rather than raw gap)
#
# notes: 
#
# last edited:   4/23/2020
#                5/20/2020 moved to 02_fit-models folder

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

datraw <- read_csv("01_create-features/cf_preds-all.csv")


imps <- read_csv("02_fit-models/fm-rfpct-imp.csv")


# do percent gaps -------------------------------------------------------------

imps_lst <- 
  imps %>% 
  select(feature) %>% 
  pull()

dat <- 
  datraw %>% 
  select(-cgap_max) %>% 
  pivot_longer(prev_ccyield:paw_150cm_mm) %>% 
  filter(name %in% imps_lst) %>% 
  pivot_wider(names_from = name,
              values_from = value)

dat_cor <- 
  dat %>%
  ungroup() %>% 
  select_if(is.numeric) 
corres <- cor(dat_cor, use="complete.obs")
corrplot::corrplot(corres)


#hmm. Some correlation problems still in there. SEe what happens

#--keep only numeric things
ydat <- 
  dat %>% 
  ungroup() %>% 
  select_if(is.numeric)

#--scale
ydatsc <- ydat %>% 
  mutate_if(is.numeric, scale)

#--get rid of any na rows (shouldn't be any)
ydat <- na.omit(ydat)

# pls ---------------------------------------------------------------------


# CRAP

library(pls)
library(caret)

plsm <- plsr(cgap_max_pct ~., data = ydatsc, validation = "LOO")

# Find the number of dimensions with lowest cross validation error
cv <- RMSEP(plsm)
best.dims = which.min(cv$val[estimate = "adjCV", , ]) - 1
best.dims
# Use other methods, see what they say...
sebars <- selectNcomp(plsm, method = "onesigma", plot = TRUE)
#targets <- selectNcomp(pls_tmp, method = "randomization", plot = TRUE)

# Rerun the model
plsmn <- plsr(cgap_max_pct ~., data = ydatsc, ncomp = best.dims)

# Code copied from Ranae, stupid rownames
varImp(plsmn) %>%
  rownames_to_column() %>%
  rename(var = rowname,
         imp = Overall) %>% 
  ggplot(aes(x = reorder(var, imp), y = imp)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("PLS")


# lasso/RR ----------------------------------------------------------------

library(glmnet)


# Scale and center predictors
pred_tmp <-
  ydat %>%
  select(-cgap_max_pct) %>%
  select_if(is.numeric) %>%
  mutate_all(funs(scale))

myr_tmp <- ydat %>%
  select(cgap_max_pct) 

sdat_tmp <- bind_cols(myr_tmp, pred_tmp)
  
# Ensure no na, this might be trouble for the places w/o WT data....
sdat_tmp <- na.omit(sdat_tmp)
  
# Make predictor matrix
myx_tmp <- model.matrix(cgap_max_pct~., sdat_tmp)[,-1] # trim off the first column
  
# Make response vector
myy_tmp <- 
  sdat_tmp %>%
    select(cgap_max_pct) %>%
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
  
myres %>% 
  filter(LSO_value != 0) %>% 
  filter(abs(LSO_value) > 1) %>% 
  mutate(pred = recode(pred,
                       p2mo_tx_mean = "Mean Max T planting-V12",
                       wintcolddays_n = "Number of Days less than 4 degF Before Planting",
                       prep2wk_precip_mm_tot = "Amount of Rain 2 weeks Before Planting",
                       gs_tavg = "April 1 - Sept 1 Average Temperature",
                       bhzdepth_cm = "Depth to B Horizon",
                       wtdepth_cm = "Depth to Water Table",
                       p4wk_1inrain = "Days planting-1 mo with 1 inch of rain",
                       soc_30cm_pct = "Soil Carbon in top 30 cm",
                       ndays_gdd140 = "Number of DAP to reach 140GDD",
                       heatstress_n = "Number of days w/Tmax > 30degC 0-120 DAP",
                       iacsr = "Iowa Corn Suitability Rating",
                       years_in_corn = "Years In Experiment",
                       avg_ccyield = "Site Average Corn Yield")
  ) %>% 
  mutate(effect = ifelse(LSO_value < 0, "Decreases Penalty", "Increases Penalty")) %>% 
  ggplot(aes(reorder(pred, LSO_value, mean), LSO_value)) + 
  geom_col(aes(fill = effect)) + 
  coord_flip() + 
  scale_fill_manual(values = c("Decreases Penalty" = "blue",
                               "Increases Penalty" = "red")) +
  labs(y = "Effect on Penalty", 
       fill = NULL,
       x = NULL,
       title = "LASSO Regression,\nWhat Drives Contin Corn Penalty on a Percent Basis?")


ggsave("02_fit-models/fig_lasso-pctgap.png")
ggsave("../../../Box/Gina_APSIM_modeling/figs-from-repo/stats_lasso-pctgap.png")


#--yeah, it's a relationship just w/pct. 
#--it seems like if it is >50
datraw %>%
  select(heatstress_n, cgap_max, cgap_max_pct) %>% 
#  filter(heatstress_n < 50) %>% 
  pivot_longer(-heatstress_n) %>% 
  ggplot(aes(heatstress_n, value)) + 
  geom_point() +
  geom_smooth(method  = "lm") +
  facet_grid(name~., scales = "free")


#--which sites had more than 50 days of heatstress?
datraw %>%
  filter(heatstress_n > 50) %>% 
  select(yearF, everything())

myres %>% 
  ggplot(aes(reorder(pred, RR_value, mean), RR_value)) + 
  geom_col() + 
  coord_flip()



# is the lesser penalty across time superfluous? --------------------------

library(tidysawyer2)

# yes
saw_tidysawyer %>% 
  group_by(site, year) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  ggplot(aes(year, yield_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm")
