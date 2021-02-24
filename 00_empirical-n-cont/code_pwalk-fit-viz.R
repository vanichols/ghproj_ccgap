# created 2/18/21
# gina
# purpose: figure out how to visualize the fits


rm(list = ls())
library(tidysawyer2)
library(tidyverse)
library(scales)



# functions ---------------------------------------------------------------

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



# fit to all sites --------------------------------------------------------

tst.tib <- 
  ilia_yields

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
  rename("aonr_rot" = rotation)  

tst.aonrs %>% 
  write_csv("00_empirical-n-cont/fits-aonrs.csv")

#--get preds at many values
tst.mods <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()

#--50 didn't converge. Mixed model might help with this. 
tst.noconv <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(is.null(model)) %>%
  ungroup()

tst.noconv %>% 
  select(site, year, rotation) %>% 
  write_csv("00_empirical-n-cont/fits-no-conv.csv")

#--get prediction values 
tst.prds <- 
  tst.mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,350)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,350)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 

tst.prds %>% 
  write_csv("00_empirical-n-cont/fits-preds.csv")



# viz ---------------------------------------------------------------------

qsite <- c("brow")

siteopts <- 
  ilia_yields %>% 
  ungroup() %>% 
  select(site) %>% 
  distinct() %>% 
  pull(site)

plot.list <- list()

for (i in 1:length(siteopts)){

    qsite <- siteopts[i]
  
    viz.aonr <- 
      tst.aonrs %>% 
      separate(aonr_rot, into = c("x", "rotation")) %>% 
      rename("nrate_kgha" = aonr_kgha) %>% 
      select(-x) %>% 
      left_join(tst.prds) %>% 
      filter(site %in% qsite)
    
    viz.prds <- 
      tst.prds %>% 
      filter(site %in% qsite)
    
    viz.obs <- 
      ilia_yields %>% 
      mutate(nrate_kgha = round(nrate_kgha, 0)) %>% 
      filter(site %in% qsite)
    
plot.list[[i]] <- 
  ggplot() + 
      geom_point(data = viz.obs, aes(x = nrate_kgha, y = yield_kgha, color = rotation)) + 
      geom_line(data = viz.obs, aes(x = nrate_kgha, y = yield_kgha, color = rotation), linetype = "dashed") + 
      geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield, color = rotation), size = 2) + 
      geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield, fill = rotation), pch = 23, size = 2, stroke = 2) + 
      facet_wrap(~year) + 
      theme_bw()
    
    
}


#--use pwalk to make a fig for each site

plot_dat <- raw %>% 
  arrange(site, year, n_rate) %>% 
  filter(crop == 'corn')

plots <-
  plot_dat %>%
  split(.$site) %>%
  map( ~ (
    ggplot(., aes(n_rate, yield_bua)) + #--put the column name of what you want here
      geom_point(size = 3) +
      geom_line(linetype = "dashed") +
      facet_grid(cropsys ~ year) +
      labs(title = "corn")
  ))

paths <- stringr::str_c(siteopts, ".png")

pwalk(list(paths, plot.list), ggsave, path = "00_empirical-n-cont/quadplat-fits/", width = 9) 
