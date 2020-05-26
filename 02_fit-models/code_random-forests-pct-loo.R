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
  select(-crop, -year) %>%
  select(-avg_yield) %>% 
  select(-p2mo_gdd) %>% 
  select(-pre2wkp2wk_tl_mean) %>% 
  select(-paw_150cm_mm) %>% 
  select(-p2wk_precip_mm_tot) %>% 
  filter(!is.na(prev_ccyield))

ccgap <- na.omit(ccgap)



# loo site ----------------------------------------------------

#--full model

dat.tmp <- 
  ccgap %>% 
  select(-site, -yearF)

mod.tmp <- randomForest::randomForest(x = dat.tmp %>% 
                                        dplyr::select(-cgap_max_pct),
                                      y = dat.tmp$cgap_max_pct)

pred.tmp <-
  Predictor$new(
    model = mod.tmp, 
    data = dat.tmp, 
    y = dat.tmp$cgap_max_pct)

imp.tmp <-
  FeatureImp$new(
    predictor = pred.tmp, 
    loss = 'mae')

imp_dat <- 
  imp.tmp$results %>% 
  as_tibble() %>% 
  arrange(-importance) %>% 
  filter(importance > 1.1) %>% 
  select(feature, importance) %>% 
  mutate(loo = "full")


#--leave-one-site-out

mysites <-
  ccgap %>% select(site) %>% distinct() %>% pull()


for (i in 1:length(mysites)) {
  
  loo <- mysites[i]
  
  dat.tmp <- 
    ccgap %>% 
    filter(site != loo) %>% 
    select(-site, -yearF)

  mod.tmp <- randomForest::randomForest(x = dat.tmp %>% 
                                          dplyr::select(-cgap_max_pct),
                                      y = dat.tmp$cgap_max_pct)

pred.tmp <-
  Predictor$new(
    model = mod.tmp, 
    data = dat.tmp, 
    y = dat.tmp$cgap_max_pct)

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

#--site individually
imp_dat %>%
  filter(!grepl("Y", loo)) %>% 
  group_by(loo, feature) %>% 
  rename(imp = importance) %>% 
  ggplot(aes(reorder(feature, imp), imp)) +
  geom_point(size = 4) + 
  geom_segment(y = 0, aes(xend = feature, yend = imp)) + 
  coord_flip() + 
  facet_wrap(~loo)

ggsave("02_fit-models/fig_rf-loo-site.png")

imp_dat %>%
  group_by(feature) %>% 
  summarise(imp = sum(importance)) %>% 
  ggplot(aes(reorder(feature, imp), imp)) +
  geom_point(size = 4) + 
  geom_segment(y = 0, aes(xend = feature, yend = imp)) + 
  coord_flip() + 
  labs(title = "Leave-one-out sum of imps")


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
                                          dplyr::select(-cgap_max_pct),
                                        y = dat.tmp$cgap_max_pct)
  
  pred.tmp <-
    Predictor$new(
      model = mod.tmp, 
      data = dat.tmp, 
      y = dat.tmp$cgap_max_pct)
  
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
  facet_wrap(~loo)
  
ggsave("02_fit-models/fig_rf-loo-year.png")


imp_dat %>%
  mutate(loo_cat = ifelse(grepl("Y", loo), "year", "site")) %>% 
  group_by(loo_cat, feature) %>% 
  summarise(imp = sum(importance)) %>% 
  ggplot(aes(reorder(feature, imp), imp)) +
  geom_point(size = 4, aes(color = imp < 6)) + 
  geom_segment(y = 0, aes(xend = feature, yend = imp,
                          color = imp < 6)) + 
  coord_flip() + 
  labs(title = "Leave-one-out sum of imps") + 
  facet_wrap(.~loo_cat)

ggsave("02_fit-models/fig_rf-loo-summary.png")


imp_dat %>%
  mutate(loo_cat = ifelse(grepl("Y", loo), "year", "site")) %>% 
  group_by(loo_cat, feature) %>% 
  summarise(imp = sum(importance)) %>%
  arrange(-imp) %>% 
  write_csv("02_fit-models/fm-rfpct-imp-loo.csv")
  
