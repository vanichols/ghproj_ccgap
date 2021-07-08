# Created:       3/30/2021
# 
# purpose: pls on n- and non-n componenents
#
# notes: 
#
# last edited:   12/2/2020
#                 july 7 2021, looking at
#

rm(list = ls())
library(tidyverse)
library(pls) # NOTE: these packages break the correlation functions
library(caret) # for varImp function



# data --------------------------------------------------------------------

gapc <- read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  mutate(gap_n = ngap,
         gap_nonn = nonngap) %>% 
  dplyr::select(site, year, gap_n, gap_nonn)

wea <- read_csv("01_create-features/1_dat_preds-wea.csv")
soi <- read_csv("01_create-features/2_dat_preds-soil.csv")
oth <- read_csv("01_create-features/3_dat_preds-other.csv") %>% select(site, year, prevyrccyield_kgha, yearsincorn)


mdat <- 
  gapc %>% 
  left_join(wea) %>% 
  left_join(soi) %>% 
  left_join(oth) %>% 
  unite(state, site, year, col = "site_year") 


md_nonn <-
  mdat %>% 
  select(-site_year, -gap_n) %>% 
  ungroup() %>%
  mutate_all(list(scale))



# PLS, loo valid ----------------------------------------------------------

set.seed(951983)

# plsr = partial least squares regression
pls_tmp <- plsr(gap_nonn ~ ., data = md_nonn, validation = "LOO")

# Find the number of dimensions with lowest cross validation error
# RMSEP = root mean squared error prediction
plot(RMSEP(pls_tmp, legendpos = "topright"))
cvs_tmp <- RMSEP(pls_tmp)
generous <-
  as.numeric(which.min(cvs_tmp$val[estimate = "adjCV", ,]) - 1) # Subtract 1 bc it's counting the intercept

# Use other methods
sebars <- selectNcomp(pls_tmp, method = "onesigma", plot = TRUE)
# Wow this is unstable.....
targets <-
  selectNcomp(pls_tmp, method = "randomization", plot = TRUE)

mylow <- generous

mylow <- 2

# Are there weird patterns?
plot(pls_tmp,
     ncomp = mylow,
     asp = 1,
     line = TRUE)

# How much variance is explained by each component?
explvar(pls_tmp)
plot(pls_tmp, "loadings", comps = 1:3, legendpos = "topleft") + abline(h = 0)

# Rerun the model w/chosen # of components
pls_tmp2 <-
  plsr(gap_nonn ~ ., data = md_nonn, ncomp = mylow) # I really only need 1 says lowsig

# Look at explained variance
explvar(pls_tmp2)

#loading.weights(pls_tmp2)

# Extract coefficients
coef_C <- coef(pls_tmp2)
sum.coef <- sum(sapply(coef_C, abs))
coef_C2 <- coef_C * 100 / sum.coef 

# Code copied from Ranae, basically

  varImp(pls_tmp2) %>%
  rownames_to_column() %>%
  rename(var = rowname,
         imp = Overall) %>%
  mutate(imps = imp / sum(imp) * 100,
         coefs = as.numeric(coef_C)) %>% 
  ggplot(aes(reorder(var, imps), abs(coefs))) + 
    geom_col(aes(fill = imps)) + 
    coord_flip()
