# created 2/12/21
# gina
# purpose: mike thinks I can get some kind of info from aonr stuff
# updated:3/19/21 separated aonr and npct calcs

library(tidysawyer2)
library(tidyverse)
library(scales)
library(ggmosaic)



# data --------------------------------------------------------------------

aonrs <- read_csv("00_empirical-n-cont/dat_aonrs.csv")

# functions ---------------------------------------------------------------------

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


  
#--map it, will those starting values work for everything?

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


# try on all sites --------------------------------------------
#--note: what happens in years where no plateau is reached?

tst.tib <- 
  ilia_yields


#--get preds at many values
tst.mods <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()

#--50 didn't converge. Mixed model might help with this? 
tst.noconv <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(is.null(model)) %>%
  ungroup()

#--do some sites have omre probs than others?
tst.noconv %>% 
  ggplot(aes(site)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~rotation)
#--really it's just the sc I'm concerned about. 
#--if I can ignore the CC instances and just do gaps at highest N (or something)

#--get yield preds at 0-300 N kga/ha
tst.prds <- 
  tst.mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,300)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,300)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 


#--want gap at rot aonr, then gap at cont aonr
#--maybe not
#--actually maybe. The fits are much more stable
tst.rotaonrgap <-
  tst.prds %>%
  ungroup() %>%
  left_join(aonrs) %>%
  filter(aonr_rot == "aonr_sc") %>%
  filter(nrate_kgha == aonr_kgha) %>%
  select(site, year, rotation, pred_yield, aonr_rot)  %>%
  pivot_wider(names_from = rotation, values_from = pred_yield) %>%
  mutate(gap_at_rotaonr_kgha = sc - cc) %>%
  select(-cc,-sc, -aonr_rot) %>%
  mutate(gap_at_rotaonr_kgha = ifelse(gap_at_rotaonr_kgha < 0, 0, gap_at_rotaonr_kgha))

tst.contaonrgap <-
  tst.prds %>%
  ungroup() %>%
  left_join(aonrs) %>%
  filter(aonr_rot == "aonr_cc") %>%
  filter(nrate_kgha == aonr_kgha) %>%
  select(site, year, rotation, pred_yield, aonr_rot)  %>%
  pivot_wider(names_from = rotation, values_from = pred_yield) %>%
  mutate(gap_at_contaonr_kgha = sc - cc) %>%
  select(-cc,-sc, -aonr_rot) %>%
  mutate(gap_at_contaonr_kgha = ifelse(gap_at_contaonr_kgha < 0, 0, gap_at_contaonr_kgha))

tst.npct <-
  tst.rotaonrgap %>%
  left_join(tst.contaonrgap) %>%
  mutate(ngap_frac = (gap_at_rotaonr_kgha - gap_at_contaonr_kgha)/gap_at_rotaonr_kgha,
         ngap_frac = case_when(
           ngap_frac < 0 ~ 0,
           (gap_at_contaonr_kgha == 0)&(gap_at_rotaonr_kgha==0) ~ 1,
           TRUE ~ ngap_frac
         ))

tst.npct %>% write_csv("00_empirical-n-cont/dat_npct.csv")

