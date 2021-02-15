# created 2/12/21
# gina
# purpose: mike thinks I can get some kind of info from aonr stuff
# updated

library(tidysawyer2)
library(tidyverse)



# dat ---------------------------------------------------------------------

ilia_aonr %>% 
  ggplot(aes(year, aonr, color = rotation)) + 
  geom_jitter() + 
  facet_wrap(~site)

#--filter out years where aonr is max value


ilia_aonr %>%
  anti_join( ilia_aonr %>% 
              group_by(site) %>%
              filter(aonr > max(aonr) - 5)) %>% 
  ggplot(aes(year, aonr, color = rotation)) + 
  geom_point() + 
  facet_wrap(~site)

library(tidyverse)

atmax <- 
  ilia_aonr %>% 
  group_by(site) %>%
  filter(aonr > max(aonr) - 5)


ilia_yields %>%
  filter(site == "ames", 
         year %in% c(2001, 2002)) %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  facet_wrap(~site+year)


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


# try functions -----------------------------------------------------------

tst.tib <- 
  ilia_yields %>%
  filter(site == "ames",
         year %in% c(2002))


tst.aonrs <- 
  tst.tib %>% 
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
  pivot_wider(names_from = rotation, values_from = aonr_kgha)

tst.aonrs

tst.mods <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  filter(!is.null(model)) 

#--visualize
tst.tib %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  geom_vline(data = tst.aonrs %>% 
               pivot_longer(aonr_cc:aonr_sc) %>% 
               separate(name, into = c("x", "rotation")), 
             aes(xintercept = value, color = rotation))
  
tst.prds <- 
  tst.mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,270)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,270)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 

tst.diffs <- 
  tst.prds %>% 
  ungroup() %>% 
  left_join(tst.aonrs) %>% 
  filter(nrate_kgha == aonr_cc | nrate_kgha == aonr_sc) %>% 
  pivot_longer(aonr_cc:aonr_sc) %>% 
  filter(rotation == "cc") %>%
  filter(nrate_kgha == value) %>% 
  select(-nrate_kgha, -value) %>% 
  pivot_wider(names_from = name, values_from = pred_yield) %>% 
  mutate(yaonr_diff_kgha = (aonr_cc - aonr_sc)) %>% 
  select(site, year, yaonr_diff_kgha)

#--compare to yield gap at max N

tst.npct <- 
  ilia_gaps %>% 
  select(site, year, nrate_kgha, gap_kgha) %>% 
  left_join(tst.diffs) %>% 
  filter(!is.na(yaonr_diff_kgha)) %>% 
  mutate(gap_npct = yaonr_diff_kgha/gap_kgha * 100) 

tst.npct



# try on bigger subset of data --------------------------------------------

tst.tib <- 
  ilia_yields %>%
  filter(site == "ames")


tst.aonrs <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  #mutate(model = map(data, qp_fun))
  mutate(model = map(data, possibly(qpcoefs_fun, NULL))) %>% 
  rowwise() %>%
  filter(!is.null(model)) %>% 
  unnest(cols = c(model)) %>% 
  mutate(aonr_kgha = -0.5 * (b/c)) %>% 
  select(site, year, rotation, aonr_kgha) %>% 
  mutate(rotation = paste0("aonr_", rotation),
         aonr_kgha = round(aonr_kgha, 0)) %>% 
  pivot_wider(names_from = rotation, values_from = aonr_kgha) %>% 
  filter(!is.na(aonr_sc))

tst.aonrs

tst.mods <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>%
  filter(!is.null(model))
  
#--visualize
tst.tib %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  geom_vline(data = tst.aonrs %>% 
               pivot_longer(aonr_cc:aonr_sc) %>% 
               separate(name, into = c("x", "rotation")), 
             aes(xintercept = value, color = rotation)) + 
  facet_wrap(~site+year)

tst.prds <- 
  tst.mods %>%
  ungroup() %>% 
  mutate(pred_yield = map(model, .f = possibly(predict, NULL), newdata = data.frame(nrate_kgha = seq(0,270)))) %>%
  rowwise() %>% 
  filter(!is.null(pred_yield)) %>% 
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,270)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 

tst.diffs <- 
  tst.prds %>% 
  ungroup() %>% 
  left_join(tst.aonrs) %>% 
  filter(nrate_kgha == aonr_cc | nrate_kgha == aonr_sc) %>% 
  pivot_longer(aonr_cc:aonr_sc) %>% 
  filter(rotation == "cc") %>%
  filter(nrate_kgha == value) %>% 
  select(-nrate_kgha, -value) %>% 
  pivot_wider(names_from = name, values_from = pred_yield) %>% 
  mutate(yaonr_diff_kgha = (aonr_cc - aonr_sc)) %>% 
  select(site, year, yaonr_diff_kgha)

#--compare to yield gap at max N

tst.npct <- 
  ilia_gaps %>% 
  select(site, year, nrate_kgha, gap_kgha) %>% 
  left_join(tst.diffs) %>% 
  filter(!is.na(yaonr_diff_kgha)) %>% 
  mutate(gap_npct = yaonr_diff_kgha/gap_kgha * 100) 


#--look at problems
tst.npct %>% 
  ggplot(aes(year, gap_npct)) + 
  geom_point()

#--visualize
tst.tib %>% 
  filter(year == 2004) %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  geom_vline(data = tst.aonrs %>% 
               filter(year == 2004) %>% 
               pivot_longer(aonr_cc:aonr_sc) %>% 
               separate(name, into = c("x", "rotation")), 
             aes(xintercept = value, color = rotation)) + 
  facet_wrap(~site+year)

tst.aonrs %>% filter(year == 2004)

tst.npct %>% 
  filter(year == 2004)

tst.prds %>% 
  filter(year == 2004, 
         nrate_kgha %in% c(237, 134)
         )


#--I have to think about this

# old ---------------------------------------------------------------------


qpcoefs_fun(tst = tst)

tst2 <- 
  ilia_yields %>% 
  filter(site == "ames",
         year == 2001) %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  #mutate(model = map(data, qp_fun))
  mutate(model = map(data, possibly(qpcoefs_fun, NULL))) %>% 
  filter(!is.null(model)) %>%
  unnest(cols = c(model)) %>% 
  mutate(aonr_kgha = -0.5 * (b/c)) %>% 
  select(site, year, rotation, aonr_kgha) %>% 
  mutate(rotation = paste0("aonr_", rotation),
         aonr_kgha = round(aonr_kgha, 0)) %>% 
  pivot_wider(names_from = rotation, values_from = aonr_kgha)

tst2

tst3 <- 
  ilia_yields %>% 
  filter(site == "ames",
         year == 2001) %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  filter(!is.null(model)) 


ilia_yields %>% 
  filter(site == "ames",
         year == 2001) %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  geom_vline(data = tst2, aes(xintercept = aonr, color = rotation))

tst4 <- 
  tst3 %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,270)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,270)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 

tst5 <- 
  tst4 %>% 
  ungroup() %>% 
  left_join(tst2) %>% 
  filter(nrate_kgha == aonr_cc | nrate_kgha == aonr_sc) %>% 
  pivot_longer(aonr_cc:aonr_sc) %>% 
  filter(rotation == "cc") %>%
  filter(nrate_kgha == value) %>% 
  select(-nrate_kgha, -value) %>% 
  pivot_wider(names_from = name, values_from = pred_yield) %>% 
  mutate(yaonr_diff_kgha = (aonr_cc - aonr_sc)) %>% 
  select(site, year, yaonr_diff_kgha)




#--compare to yield gap at max N
ilia_gaps %>% 
  filter(site == "ames",
         year == 2001)  %>% 
  select(site, year, gap_kgha) %>% 
  left_join(tst5) %>% 
  mutate(gap_npct = yaonr_diff_kgha/gap_kgha * 100) 


  






#--mitch

dat <- read_csv("00_empirical-n-cont/from-mitch/aonr a b c coefs.csv") 

dat %>% 
  select(-X1) %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  filter(site == "Lewis", year == 2002, rotation == "CC") %>% 
  unnest()

dat %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(qp = purrr::map(data, ~try(nls(yield ~ (a + b * rate + c * I(rate)) *
                                          (rate <= -0.5 * b/c) +
                                          (a + I(-b^2/(4 * c))) * (rate > -0.5 * b/c),
                                        start = list(a = as.data.frame(.)[1,2],
                                                     b = as.data.frame(.)[1,3],
                                                     c = as.data.frame(.)[1,4]),
                                        control = list(maxiter = 1000),
                                        data = .)))) %>%
  mutate(newfit = as.character(qp),
         newfit = str_sub(newfit, 1, 3)) %>%
  filter(newfit != "Err") %>%
  mutate(a_coef = sapply(qp, FUN = function(qp){a = coef(qp)[1]}),
         b_coef = sapply(qp, FUN = function(qp){b = coef(qp)[2]}),
         c_coef = sapply(qp, FUN = function(qp){c = coef(qp)[3]}),
         aonr = -0.5 * (b_coef/c_coef),
         yaonr = (a_coef + I(-b_coef^2/(4 * c_coef)))) qp_regress



## maybe a way to solve the SC aonr at CC yield? 


qp_regress %>%
  mutate(pred_yield = map(qp, .f = predict, newdata = data.frame(rate = seq(0,270)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(rate = as.numeric(as.character(seq(0,270))))


read_csv("00_empirical-n-cont/from-mitch/aonr a b c coefs.csv") %>% 
  select(-X1) %>% 
  
