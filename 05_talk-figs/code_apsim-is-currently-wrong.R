# created aug 13 2021
# gina
# purpose: show that apsim is wrong
# updated:

rm(list = ls())
library(tidysawyer2)
library(tidyverse)
library(nlraa)
library(patchwork)

theme_set(theme_bw())
source("05_talk-figs/talk-palette.R")

# data ----------------------------------------------------------------

obs <- 
  ilia_yields

sims_cal <- 
  ilia_simsyields %>% 
  select(state, site, year, rotation2, nrate_kgha, cal_nosc) %>% 
  rename("rotation" = rotation2,
         "yield_kgha" = cal_nosc)

sims_uncal <- 
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


# fit models --------------------------------------------------------------

obs_mods <- 
  obs %>% 
  group_by(rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()


cal_mods <- 
  sims_cal %>% 
  group_by(rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()

uncal_mods <- 
  sims_uncal %>% 
  group_by(rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()


# get aonrs ---------------------------------------------------------------

#--fit all site-years together

obs_aonrs_allsites <- 
  obs %>% 
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
  mutate(desc  = "observed")


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


uncal_aonrs_allsites <- 
  sims_uncal %>% 
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
  mutate(desc  = "uncal no scripts")

aonrs <- 
obs_aonrs_allsites %>% 
  bind_rows(uncal_aonrs_allsites) %>% 
  bind_rows(cal_aonrs_allsites)
  

# get preds ---------------------------------------------------------------

cal_prds_allsites <- 
  cal_mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,300)))) %>%
  unnest(pred_yield) %>%
  group_by(rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,300)))) %>% 
  select(rotation, nrate_kgha, pred_yield)   %>% 
  mutate(desc  = "cal no scripts")

uncal_prds_allsites <- 
  uncal_mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,300)))) %>%
  unnest(pred_yield) %>%
  group_by(rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,300)))) %>% 
  select(rotation, nrate_kgha, pred_yield)   %>% 
  mutate(desc  = "uncal no scripts")

obs_prds_allsites <- 
  obs_mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,300)))) %>%
  unnest(pred_yield) %>%
  group_by(rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,300)))) %>% 
  select(rotation, nrate_kgha, pred_yield)   %>% 
  mutate(desc  = "observed")

prds <- 
  cal_prds_allsites %>% 
  bind_rows(uncal_prds_allsites) %>% 
  bind_rows(obs_prds_allsites)

# figure ------------------------------------------------------------------

viz.aonr <- 
  aonrs %>% 
  separate(aonr_rot, into = c("x", "rotation")) %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  select(-x) %>% 
  left_join(prds) %>% 
  mutate(rot = ifelse(rotation == "cc", "Continuous maize AONR", "Rotated maize AONR")) %>% 
  filter(desc != "cal no scripts") %>% #--this doesn't look good
  mutate(desc = ifelse(desc == "observed", "Experimental Data", "Models"))

viz.prds <- 
  prds %>% 
  filter(nrate_kgha < 300) %>% 
  mutate(rot = ifelse(rotation == "cc", "Continuous maize AONR", "Rotated maize AONR")) %>% 
  filter(desc != "cal no scripts") %>% #--this doesn't look good
  mutate(desc = ifelse(desc == "observed", "Experimental Data", "Models"))



# all the goodies ---------------------------------------------------------
yld_lab <- (expression(atop("Corn Yield", paste("(Mg "~ha^-1*")"))))

n_lab <- (expression("Nitrogen fertilization rate (kg N "~ha^-1*")"))

ggplot() + 
  geom_line(data = viz.prds, aes(x = nrate_kgha, y = pred_yield/1000, color = rot, linetype = desc), size = 2) + 
  geom_point(data = viz.aonr, aes(x = nrate_kgha, y = pred_yield/1000, fill = rot), pch = 23, size = 4, stroke = 2) + 
  scale_color_manual(values = c("Continuous maize AONR" = clr_cc, 
                                "Rotated maize AONR" = clr_rot)) +
  scale_fill_manual(values = c("Continuous maize AONR" = clr_cc, 
                               "Rotated maize AONR" = clr_rot)) +
  labs(x = n_lab,
       y = yld_lab,
       
       #expression(flux*phantom(x)*(g~CO[2]~m^{-2}~h^{-1})))
       
       color = NULL,
       fill = NULL) +
  guides(fill = F, #guide_legend(nrow = 2),
         color = F, #guide_legend(nrow = 2),
         linetype = F) +
  theme_bw() + 
  facet_grid(.~desc) + 
  theme(legend.background = element_rect(color= "black"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size =rel(1.5)),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.justification = "center",
        legend.text = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)))

ggsave("05_talk-figs/fig_current-model-problem.png", 
       width = 11, 
       height = 6)



