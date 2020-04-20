# Created:       4/1/2020
# 
# purpose: Create pred summary (soil and wea)
#
# notes: 
# last edited:  4/20/2020 cleaning up code, separating data things from stats things

rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--has functios
library(tidysawyer2) #--has data
library(tidyverse)
library(lubridate)



# data --------------------------------------------------------------------


wea <- read_csv("data/td_pred-wea.csv")  
soi <- read_csv("data/td_pred-soil.csv")



# what else should we add? ------------------------------------------------

#--what does the corn suitability rating look like?
soi %>% 
  ggplot(aes(iacsr)) +
  geom_histogram()

#--add previous year's continuous corn yield at max nrate (indicative of residue amt)
prev_yield <- 
  saw_tidysawyer %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "cc") %>% 
  mutate(prev_ccyield = lag(yield_kgha)) %>% 
  select(site, year, prev_ccyield)

#--use 'site production index' of cs at max n rate
avg_yieldsc <- 
  saw_tidysawyer %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  #--make sure it's taken over the same time frame
  filter(year > 2006,
         year < 2017) %>% 
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "sc") %>% 
  summarise(avg_scyield = mean(yield_kgha, na.rm = T)) %>% 
  select(site, avg_scyield)

avg_yieldcc <- 
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


avg_yield <- 
  saw_tidysawyer %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  #--make sure it's taken over the same time frame
  filter(year > 2006,
         year < 2017) %>% 
  select(site, year, rotation, yield_kgha) %>%
  summarise(avg_yield = mean(yield_kgha, na.rm = T)) %>% 
  select(site, avg_yield)

#--is one better than the other? are they different?
avg_yieldsc %>% 
  left_join(avg_yieldcc) %>% 
  ggplot(aes(avg_scyield, avg_ccyield)) + 
  geom_point(aes(color = site)) + 
  geom_label(aes(label = site))

#--they aren't exactly the same order
library(corrr) 
avg_yieldsc %>% 
  left_join(avg_yieldcc) %>% 
  select(-site) %>% 
  corrr::correlate(method = "spearman")

#--I think the overall average is the best


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
  filter(cgap_max > -1500) %>% #--that one lewis point, just seems weird
  left_join(prev_yield) %>% 
  left_join(avg_yield) %>% 
  left_join(yrs_corn) %>% 
  left_join(wea) %>% 
  left_join(soi) %>% 
  mutate(year = paste0("Y", year)) #--to ensure it isn't numeric


# summarize ---------------------------------------------------------------

# create a summary table to explain what I threw at the models
#https://dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/

library(gt)
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

#--make something explaining what each var is
dat_tab <- 
  dat_sum_tib %>% 
  mutate(vars_nice = recode(vars,
                            "cgap_max" = "CC/SC gap at max N rate",
                            "prev_ccyield" = "Prev year CC yield at max N rate\n (indicative of residue amount)",
                            "avg_yield" = "Avg yield at max N at that site",
                            "years_in_corn" = "Number of Years in Cont Corn",
                            "heatstress_n" = "# Days w/Tmax > 30oC from planting to 120 DAP",
                            "ndays_gdd140" = "# DAP to acheive 140 GDDs",
                            "p2wk_precip_mm_tot" = "Total Precip 0-14 DAP",
                            "prep2wk_precip_mm_tot" = "Total Precip 2 wks before pl",
                            "pre2wkp2wk_tl_mean" = "Mean low temp 4 weeks around planting",
                            "wintcolddays_n" = "# of days < 4degF before Jan 1 - planting",
                            "p2mo_gdd" = "GDDs 0-2mo after planting")
                            ) %>% 
  select(vars_nice, min, max, mean) %>% 
  gt() %>% 
  tab_header(
    title = "Variables Included In Models")

gtsave(dat_tab, filename = "tble_all-dat.png", path = "tables/")

