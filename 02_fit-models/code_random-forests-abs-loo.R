# Created:       4/10/2020
# 
# purpose: Look at random forests
#
# notes: 
#
# last edited: 4/20/2020 use new 'master' dataset
#              4/23/2020 try running on pct yield gaps instead, and years > 2005 (when we have balanced data)
#              5/4/2020 make partial dep plots for heat and rain, might want to get rid of year 2000?
#              5/22/2020 look at results when leaving out one site and year
#              5/26/2020 write resulst to use in lasso
#             5/29/2020 (don't use %)



rm(list = ls())
library(saapsim) #--has functios
#library(tidysawyer2) #--has data
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
  dplyr::select(-cgap_max_pct) %>% #--dont want pct
  dplyr::select(-crop, -year) %>%
  dplyr::select(-avg_yield) %>% 
  dplyr::select(-p2mo_gdd) %>% 
  dplyr::select(-pre2wkp2wk_tl_mean) %>% 
  dplyr::select(-paw_150cm_mm) %>% 
  dplyr::select(-p2wk_precip_mm_tot) %>% 
  filter(!is.na(prev_ccyield))

ccgap <- na.omit(ccgap)



# loo site ----------------------------------------------------

#--full model

dat.tmp <- 
  ccgap %>% 
  dplyr::select(-site, -yearF)

mod.tmp <- randomForest::randomForest(x = dat.tmp %>% 
                                        dplyr::select(-cgap_max),
                                      y = dat.tmp$cgap_max)

pred.tmp <-
  Predictor$new(
    model = mod.tmp, 
    data = dat.tmp, 
    y = dat.tmp$cgap_max)

imp.tmp <-
  FeatureImp$new(
    predictor = pred.tmp, 
    loss = 'mae')

imp_dat <- 
  imp.tmp$results %>% 
  as_tibble() %>% 
  arrange(-importance) %>% 
  filter(importance > 1.1) %>% 
  dplyr::select(feature, importance) %>% 
  mutate(loo = "full")


#--leave-one-site-out

mysites <-
  ccgap %>% dplyr::select(site) %>% distinct() %>% pull()


for (i in 1:length(mysites)) {
  
  loo <- mysites[i]
  
  dat.tmp <- 
    ccgap %>% 
    filter(site != loo) %>% 
    dplyr::select(-site, -yearF)

  mod.tmp <- randomForest::randomForest(x = dat.tmp %>% 
                                          dplyr::select(-cgap_max),
                                      y = dat.tmp$cgap_max)

pred.tmp <-
  Predictor$new(
    model = mod.tmp, 
    data = dat.tmp, 
    y = dat.tmp$cgap_max)

imp.tmp <-
  FeatureImp$new(
    predictor = pred.tmp, 
    loss = 'mae')

imp_dat.tmp <- 
  imp.tmp$results %>% 
  as_tibble() %>% 
  dplyr::arrange(-importance) %>% 
  dplyr::filter(importance > 1.1) %>% 
  dplyr::select(feature, importance) %>% 
  dplyr::mutate(loo = loo)

imp_dat <- bind_rows(imp_dat, imp_dat.tmp)

i <- i + 1

}

#--site individually
imp_dat %>%
  filter(!grepl("Y", loo)) %>% 
  group_by(loo, feature) %>% 
  dplyr::rename(imp = importance) %>% 
  ggplot(aes(reorder(feature, imp), imp)) +
  geom_point(size = 4) + 
  geom_segment(y = 0, aes(xend = feature, yend = imp)) + 
  coord_flip() + 
  facet_wrap(~loo) + 
  labs(title = "RF on raw gap")

ggsave("02_fit-models/fig_rf-loo-raw-site.png")

imp_dat %>%
  dplyr::group_by(feature) %>% 
  dplyr::summarise(imp = sum(importance)) %>% 
  ggplot(aes(reorder(feature, imp), imp)) +
  geom_point(size = 4) + 
  geom_segment(y = 0, aes(xend = feature, yend = imp)) + 
  coord_flip() + 
  labs(title = "Leave-one-out sum of imps, raw gap")


# do it for years ---------------------------------------------------------

#--leave-one-site-out

myyears <-
  ccgap %>% select(yearF) %>% distinct() %>% pull()


for (i in 1:length(mysites)) {
  
  loo <- myyears[i]
  
  dat.tmp <- 
    ccgap %>% 
    filter(yearF != loo) %>% 
    select(-site, -yearF)
  
  mod.tmp <- randomForest::randomForest(x = dat.tmp %>% 
                                          dplyr::select(-cgap_max),
                                        y = dat.tmp$cgap_max)
  
  pred.tmp <-
    Predictor$new(
      model = mod.tmp, 
      data = dat.tmp, 
      y = dat.tmp$cgap_max)
  
  imp.tmp <-
    FeatureImp$new(
      predictor = pred.tmp, 
      loss = 'mae')
  
  imp_dat.tmp <- 
    imp.tmp$results %>% 
    as_tibble() %>% 
    arrange(-importance) %>% 
    filter(importance > 1.1) %>% 
    select(feature, importance) %>% 
    mutate(loo = loo)
  
  imp_dat <- bind_rows(imp_dat, imp_dat.tmp)
  
  i <- i + 1
  
}


#--years individually
imp_dat %>%
  filter(grepl("Y", loo)) %>% 
  group_by(loo, feature) %>% 
  rename(imp = importance) %>% 
  ggplot(aes(reorder(feature, imp), imp)) +
  geom_point(size = 4) + 
  geom_segment(y = 0, aes(xend = feature, yend = imp)) + 
  coord_flip() + 
  facet_wrap(~loo) + 
  labs(title = "RF, raw gap")
  
ggsave("02_fit-models/fig_rf-loo-raw-year.png")


imp_dat %>%
  mutate(loo_cat = ifelse(grepl("Y", loo), "year", "site")) %>% 
  group_by(loo_cat, feature) %>% 
  summarise(imp = sum(importance)) %>% 
  ggplot(aes(reorder(feature, imp), imp)) +
  geom_point(size = 4, aes(color = imp < 8)) + 
  geom_segment(y = 0, aes(xend = feature, yend = imp,
                          color = imp < 8)) + 
  coord_flip() + 
  labs(title = "Leave-one-out sum of imps, raw gap") + 
  facet_wrap(.~loo_cat)

ggsave("02_fit-models/fig_rf-loo-raw-summary.png")

imp_dat %>%
  mutate(loo_cat = ifelse(grepl("Y", loo), "year", "site")) %>% 
  group_by(feature) %>% 
  summarise(imp = sum(importance)) %>% 
  ggplot(aes(reorder(feature, imp), imp)) +
  geom_point(size = 4, aes(color = imp < 15)) + 
  geom_segment(y = 0, aes(xend = feature, yend = imp,
                          color = imp < 15)) + 
  coord_flip() + 
  labs(title = "Leave-one-out sum of imps, raw gap") 

ggsave("02_fit-models/fig_rf-loo-raw-summary.png")




# write list of summed imps -----------------------------------------------


imp_dat %>%
  mutate(loo_cat = ifelse(grepl("Y", loo), "year", "site")) %>% 
  group_by(loo_cat, feature) %>% 
  summarise(imp = sum(importance)) %>%
  arrange(-imp) %>% 
  write_csv("02_fit-models/fm-rfpct-imp-raw-loo.csv")
  
