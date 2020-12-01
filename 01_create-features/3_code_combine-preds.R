# Created:       4/1/2020
# 
# purpose: Create pred summary (soil and wea)
#
# notes: 
# last edited:  4/20/2020 cleaning up code, separating data things from stats things
#               4/30/2020 trying new folder structure
#               6/4/2020 add drainage
#               12/1/2020 add IL


#--make sure data is up-to-date, if you want
#source("01_create-features/1_code_preds-wea.R")
#source("01_create-features/2_code_preds-soil.R")

rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2", force = T)
library(tidysawyer2) #--has wea data
library(lubridate)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
library(tidyr)


# data --------------------------------------------------------------------

wea <- read_csv("01_create-features/1_dat_preds-wea.csv")
soi <- read_csv("01_create-features/2_dat_preds-soil.csv")


# what else should we add? ------------------------------------------------

#--add previous year's continuous corn yield at max nrate (indicative of residue amt)
prev_yield <- 
  ilia_yields %>%
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "cc") %>% 
  mutate(prevyrccyield_kgha = lag(yield_kgha)) %>% 
  select(site, year, prevyrccyield_kgha)

#--use 'site production index' of cs at max n rate
avg_yieldsc <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "sc") %>% 
  summarise(avescyield_kgha = mean(yield_kgha, na.rm = T)) %>% 
  select(site, avescyield_kgha)

avg_yieldcc <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  #--make sure it's taken over the same time frame?
  select(site, year, rotation, yield_kgha) %>%
  filter(rotation == "cc") %>% 
  summarise(aveccyield_kgha = mean(yield_kgha, na.rm = T)) %>% 
  select(site, aveccyield_kgha)


avg_yield <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  #--make sure it's taken over the same time frame
  select(site, year, rotation, yield_kgha) %>%
  summarise(aveyield_kgha = mean(yield_kgha, na.rm = T)) %>% 
  select(site, aveyield_kgha)

#--is one better than the other? are they different?
avg_yield %>% 
  left_join(aveccyield_kgha) %>% 
  left_join(avescyield_kgha) %>%
  pivot_longer(3:4) %>% 
  ggplot(aes(aveyield_kgha, value)) + 
  geom_point(aes(color = name))

#--I think the overall average is the best


#--include # of years in corn

yrs_corn <- 
  ilia_yields %>% 
  select(-state) %>% 
  filter(rotation == "cc") %>% 
  group_by(site) %>% 
  group_modify(~{
    .x %>% mutate(yearsincorn = group_indices(., year))
  }) %>% 
  select(site, year, yearsincorn) %>% 
  distinct()
  
#--tiled or not?
#--don't have info on IL right now
drainage <- 
  ia_siteinfo %>% 
  select(site_name, site, drainage)

il_siteinfo


# put it all together -----------------------------------------------------

dat <-  
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(pen_kgha = sc - cc,
         pen_pct = pen_kgha/sc * 100) %>% 
  filter(pen_kgha > -1500) %>% #--that one lewis point, just seems weird
  rename(cc_kgha = cc,
         sc_kgha = sc) %>% 
  left_join(prev_yield) %>% 
  left_join(avg_yield) %>% 
  left_join(yrs_corn) %>% 
  left_join(wea) %>% 
  left_join(soi) %>% 
  #left_join(drainage) %>% 
  mutate(yearF = paste0("Y", year)) %>% #--to ensure it isn't numeric
  select(crop, state, site, yearsincorn, yearF, year, everything()) %>% 
  filter(!is.na(prevyrccyield_kgha))


#

#--does pct gap dec over years? yes.
dat %>% 
  ggplot(aes(yearF, pen_pct)) + 
  geom_point() + 
  facet_grid(.~state, scales = "free_x")

dat %>% 
  ggplot(aes(years_in_corn, pen_pct)) + 
  geom_point() +
  geom_label(aes(label = site)) +
#  geom_smooth(method = "lm") + 
  facet_grid(.~state, scales = "free_x")


# save data ---------------------------------------------------------------

dat %>% 
  write_csv("01_create-features/3_dat_preds-all.csv")


# summarize ---------------------------------------------------------------

# create a summary table to explain what I threw at the models
#https://dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/

library(gt)
library(psych)

dat_sum <- psych::describe(dat %>% select(-year, -yearF))

dat_vars <- rownames(dat_sum)

dat_sum_tib <- 
  dat_sum %>%
  as_tibble() %>% 
  mutate(vars = dat_vars) %>%
  separate(vars, into = c("var", "unit"), sep = "_", remove = F) %>% 
  select(vars, var, unit, min, max, mean) %>% 
  filter(!is.infinite(min)) %>% 
  mutate_if(is.numeric, round, 0)

#--make something explaining what each var is
dat_nice <- 
  dat_sum_tib %>% 
  mutate(vars_nice = recode(vars,
                            "year" = "Year, all years included",
                            "yearsincorn" = "Years in corn",
                            "wyprecip_mm" = "Water year (Oct-Oct) precip total",
                            "gs_precip_mm_tot" = "Growing season precip total",
                            "heatstress_cum" = "Cumulative heatstress, base 30degC",
                            "wtdepth_cm" = "Average depth to water table",
                            "paw30_mm" = "Plant-available-water in top 30 cm",
                            "prevyrccyield_kgha" = "Preceeding year CC yield",
                            "aveyield_kgha" = "Average site-year yield",
                            "cc_kgha" = "CC yield",
                            "sc_kgha" = "SC yield",
                            "pen_kgha" = "CC/SC gap at max N rate",
                            "pen_pct" = "CC/SC gap at max N rate as % of SC yield",
                            "prev_ccyield" = "Prev year CC yield at max N rate\n (indicative of residue amount)",
                            "avg_yield" = "Avg yield at max N at that site",
                            "years_in_corn" = "Number of Years in Cont Corn",
                            "heatstress_n" = "# Days w/Tmax > 30oC from planting to 120 DAP",
                            "ndays_gdd140" = "# DAP to acheive 140 GDDs",
                            "p2wk_precip_mm_tot" = "Total precip 0-14 DAP",
                            "prep2wk_precip_mm_tot" = "Total precip 2 wks before pl",
                            "pre2wkp2wk_tl_mean" = "Mean low temp 4 weeks around planting",
                            "wintcolddays_n" = "Days < 4degF before Jan 1 - planting",
                            "p2mo_gdd" = "GDDs 0-2mo after planting",
                            "iacsr" = "Iowa Corn Suitability Rating",
                            "bhzdepth_cm" = "Depth to B horizon"),
         unit = recode(unit,
                       "precip" = "mm",
                       "tl" = "degC",
                       "n" = "days",
                       "cum" = "degC-days",
                       "pct" = "%",
                       "NA" = "years"
                            ),
         unit = ifelse(is.na(unit), "years", unit)
  )


dat_nice %>% 
  select(vars_nice, unit, min, max, mean) %>% 
  filter(!vars_nice %in% c("crop*", "state*", "site*", "nrate_kgha")) %>% 
  gt() %>% 
  tab_header(
    title = "Predictors Included In Models")


gtsave(dat_tab, filename = "3_tbl_preds.png", path = "01_create-features/")

