# created 2/12/21
# gina
# purpose: mike thinks I can get some kind of info from aonr stuff
# updated:3/19/21 separated aonr and npct calcs
#         3/24/2021 added calcs on simulated data

library(tidysawyer2)
library(tidyverse)
library(nlraa)



# sim data ----------------------------------------------------------------


sims_cal <- 
  ilia_simsyields %>% 
  select(state, site, year, rotation2, nrate_kgha, cal_nosc) %>% 
  rename("rotation" = rotation2,
         "yield_kgha" = cal_nosc)

sims_nocal <- 
  ilia_simsyields %>% 
  select(state, site, year, rotation2, nrate_kgha, uncal) %>% 
  rename("rotation" = rotation2,
         "yield_kgha" = uncal)




# functions ---------------------------------------------------------------


tst <- 
  ilia_yields %>%
  filter(site == "ames",
         year %in% c(2000),
         rotation == "cc") 


tst.m <- nls(yield_kgha ~ (a + b * nrate_kgha + c * I(nrate_kgha^2)) *
               (nrate_kgha <= -0.5 * b/c) +
               (a + I(-b^2/(4 * c))) * (nrate_kgha > -0.5 * b/c),
             start = list(a = 1.37,
                          b = 0.0215,
                          c = -0.0000568),
             control = list(maxiter = 1000),
             data = tst)

coef(tst.m)



qpfit_fun <- function(tst = data){
  
  tst.m <- nls(yield_kgha ~ (a + b * nrate_kgha + c * I(nrate_kgha^2)) *
                 (nrate_kgha <= -0.5 * b/c) +
                 (a + I(-b^2/(4 * c))) * (nrate_kgha > -0.5 * b/c),
               start = list(a = 1.37,
                            b = 0.0215,
                            c = -0.0000568),
               control = list(maxiter = 1000),
               data = tst)
  return(tst.m)
  
}

#--specifically to get coefficients and aonr
qpcoefs_fun <- function(tst = data){
  
  tst.m <- nls(yield_kgha ~ (a + b * nrate_kgha + c * I(nrate_kgha^2)) *
                 (nrate_kgha <= -0.5 * b/c) +
                 (a + I(-b^2/(4 * c))) * (nrate_kgha > -0.5 * b/c),
               start = list(a = 1.37,
                            b = 0.0215,
                            c = -0.0000568),
               control = list(maxiter = 1000),
               data = tst)
  
  tst.coef <- 
    coef(tst.m) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble() %>% 
    rename("coef" = 2) %>% 
    pivot_wider(names_from = rowname, values_from = coef)
  
  return(tst.coef)
  
}

qpfit_fun(tst = tst)
qpcoefs_fun(tst = tst)


# get aonrs ---------------------------------------------------------------

#--fit separately for each site-year
cal_aonrs <- 
  sims_cal %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpcoefs_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  unnest(cols = c(model)) %>% 
  mutate(aonr_kgha = -0.5 * (b/c)) %>% 
  select(site, year, rotation, aonr_kgha) %>% 
  mutate(rotation = paste0("aonr_", rotation),
         aonr_kgha = round(aonr_kgha, 0)) %>% 
  rename("aonr_rot" = rotation)  %>% 
  mutate(desc = "cal no scripts")

cal_aonrs %>% write_csv("00_empirical-n-cont/dat_aonrs-sims.csv")


#--fit all site-years together
cal_aonrs_allsites <- 
  sims_cal %>% 
  group_by(rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpcoefs_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  unnest(cols = c(model)) %>% 
  mutate(aonr_kgha = -0.5 * (b/c)) %>% 
  select(rotation, aonr_kgha) %>% 
  mutate(rotation = paste0("aonr_", rotation),
         aonr_kgha = round(aonr_kgha, 0)) %>% 
  rename("aonr_rot" = rotation)   %>% 
  mutate(desc  = "cal no scripts")

cal_aonrs_allsites %>% write_csv("00_empirical-n-cont/dat_aonrs-sims-by-rot-only.csv")



# get preds ---------------------------------------------------------------

#--get preds at many values
cal_mods <- 
  sims_cal%>% 
  group_by(state, site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()


# all sites, simulated data -----------------------------------------------

#--get preds at many values
cal_mods <- 
  sims_cal %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()

#--342 didn't converge?!  
cal_mods %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(is.null(model)) %>%
  ungroup()

#--ignore it for now, but this seems like a problem

#--get preds at many values
cal_mods_allsites <- 
  sims_cal %>% 
  group_by(rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()


#--get yield preds at 0-300 N kga/ha
cal_prds <- 
  cal_mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,300)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,300)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 

cal_prds %>% write_csv("00_empirical-n-cont/dat_preds-sims.csv")

cal_prds_allsites <- 
  cal_mods_allsites %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,300)))) %>%
  unnest(pred_yield) %>%
  group_by(rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,300)))) %>% 
  select(rotation, nrate_kgha, pred_yield) 

cal_prds_allsites %>% write_csv("00_empirical-n-cont/dat_preds-sims-by-rot-only.csv")
