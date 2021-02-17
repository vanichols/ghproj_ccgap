# created 2/12/21
# gina
# purpose: same as aonr, but try fitting mixed models?
# updated

rm(list = ls())

#remotes::install_github("vanichols/tidysawyer2")
library(tidysawyer2)
library(scales)
library(purrr)
library(dplyr) #--overwrites collapse from nlme
library(ggplot2)
library(readr)

library(nlme)
#remotes::install_github("femiguez/nlraa")
library(nlraa)
library(emmeans)
library(car) #--overwrites recode in dplyr
library(minpack.lm)
library(janitor)


# dat ---------------------------------------------------------------------

ilia_yields %>% 
  ggplot(aes(nrate_kgha, yield_kgha, color = rotation)) + 
  geom_jitter() + 
  facet_wrap(~site)


#--let's start with just Iowa
dat <- 
  ilia_yields %>% 
  mutate(eu = paste(site, year, rotation, sep = "_")) %>% 
  filter(state == "IA") %>% 
  select(-state, -crop) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(year = as.factor(year))


datG <- groupedData(yield_kgha ~ nrate_kgha | eu, data = dat)


#--fit model to each group
# a = intercept
# b = slope
# c = quadratic term
# xs = breakpoint

lmodG <- nlsList(yield_kgha ~ SSquadp(nrate_kgha, a, b, c, xs), data = datG) 

plot(intervals(lmodG))

#--everything is all over the place
#--
#--a, b, c, could have random components added?
fmm1 <- nlme(lmodG, random = pdDiag(a + b + c ~ 1))
plot(fmm1)
plot(fmm1, id = 0.01) #--fernando likes things to be under 2

head(coef(fmm1)) #--notice xs is the same for everything
intervals(fmm1) #--can't do it. uh oh. 

#--add grouped random effects
fmm2 <- update(fmm1, random = list(site = pdDiag(a + b + c ~ 1),
                                   eu = pdDiag(a + b + c  ~ 1)),
               groups = ~ site/eu)


plot(fmm2, id = 0.01) #--fernando likes things to be under 2

#--add fixed effect of rotation
#--won't run if it includes c? complains, but still runs? No. 
fxf1 <- fixef(fmm2) 
fmm3a <- update(fmm2, 
                fixed = list(a + b + c + xs ~ rotation),
                start = c(fxf1[1], 0, #--a
                          fxf1[2], 0, #--b
                          fxf1[3], 0, #--c
                          fxf1[4], 0)) #--xs


head(coef(fmm2))
intervals(fmm2)
plot(intervals(lmodG))

#--eliminate c? How do I decide? I guess c is small anyways
fxf1 <- fixef(fmm2)
fmm3b <- update(fmm2,
                fixed = list(a + b + xs ~ rotation),
                start = c(fxf1[1], 0, #--a
                          fxf1[2], 0, #--b
                          #fxf1[3], 0, #--c
                          fxf1[4], 0)) #--xs


plot(fmm3b)
intervals(fmm3b) 
#--I can calc my aonr from this
coef(fmm3b) %>% as_tibble() %>% janitor::clean_names()


# get aonrs for each eu ---------------------------------------------------

# aonr =  -0.5 * (b/c)
fmm3b_coefs <- 
  coef(fmm3b) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% janitor::clean_names()

bvals <- 
  fmm3b_coefs %>% 
  select(rowname, starts_with("b_")) %>% 
  separate(rowname, into = c("site", "eu"), sep = "/") %>% 
  separate(eu, into = c("site2", "year", "rot"), sep = "_") %>% 
  mutate(bval = ifelse(rot == "cc", b_intercept, b_intercept + b_rotationsc)) %>% 
  select(site, year, rot, bval)

cvals <- 
  fmm3b_coefs %>% 
  rename("cval" = c) %>% 
  select(rowname, cval) %>% 
  separate(rowname, into = c("site", "eu"), sep = "/") %>% 
  separate(eu, into = c("site2", "year", "rot"), sep = "_") %>% 
  select(site, year, rot, cval)


aonrs <- 
  bvals %>% 
  left_join(cvals) %>% 
  mutate(aonr = -0.5 * (bval/cval)) %>% 
  select(site, year, rot, aonr)

# get predicted yields ----------------------------------------------------

prd <- predict_nlme(fmm3b, interval = "conf", plevel = 1) #-I think this is site level? 

#-What if I want year?
prd2 <- predict_nlme(fmm3b, interval = "none", plevel = 2) #-will this do site-year?
newdat <- cbind(dat, prd2) %>% as_tibble()


ggplot(
  data = newdat,
  aes(
    x = nrate_kgha,
    y = prd2,
    color = rotation,
    group = interaction(site, year, rotation)
  ),
  size = 2
) +
  geom_point() +
  geom_line() +
  facet_grid(. ~ rotation)

#--worked! now do it with more nrates
#--won't do it above 250. So this defeats the purpose, doesn't it?
bigdat <- 
  dat %>% 
  select(-nrate_kgha, -yield_kgha) %>% 
  expand_grid(., nrate_kgha = seq(0:350))

bigprd2 <- predict_nlme(fmm3b, interval = "none", plevel = 2, newdata = bigdat)
newbigdat <- cbind(bigdat, bigprd2) %>% as_tibble()





ggplot(
  data = newbigdat,
  aes(
    x = nrate_kgha,
    y = bigprd2,
    color = rotation,
    group = interaction(site, year, rotation)
  ),
  size = 2
) +
  geom_line() +
  facet_grid(. ~ rotation)


## Parameter values and contrast among groups
emmeans(fmm3, ~ rotation, param = "a")
emmeans(fmm3, ~ rotation, param = "b")
emmeans(fmm3, ~ rotation, param = "xs")
## Contrasts
contrast(emmeans(fmm3, ~ rotation, param = "a"), "pairwise")
contrast(emmeans(fmm3, ~ rotation, param = "b"), "pairwise")
contrast(emmeans(fmm3, ~ rotation, param = "xs"), "pairwise")





sim_leach <- simulate_nlme(lmod3a, nsim = 100, psim = 1, level = 0)
leachG$mn.s <- apply(sim_leach, 1, mean)
leachG$mxn.s <- apply(sim_leach, 1, max)
leachG$mnn.s <- apply(sim_leach, 1, min)


ggplot() + 
  geom_ribbon(data = leachG,
              mapping = aes(x = nrate_kgha,
                            ymin = mxn.s,
                            ymax = mnn.s, fill = rotation),
              alpha = 0.5) +
  geom_line(data = leachG, aes(x = nrate_kgha, 
                               y = prds, 
                               color = rotation), size = 2) +
  labs(y = "leaching_kgha")



#--I need to figure out how to 'smooth' the predictions

newdat <- dat %>% 
  select(year, site_id, rotation) %>%
  distinct() %>% 
  expand_grid(nrate_kgha = seq(0, 250, by = 10))



# dat ---------------------------------------------------------------------

ilia_aonr %>% 
  ggplot(aes(year, aonr, color = rotation)) + 
  geom_jitter() + 
  facet_wrap(~site)

ilia_yields %>%
  filter(site == "ames", 
         year %in% c(2001, 2002)) %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  facet_wrap(~site+year)



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
         year %in% c(2001))


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
#pivot_wider(names_from = rotation, values_from = aonr_kgha)

tst.aonrs

tst.mods <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>% 
  ungroup()

#--visualize
tst.tib %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  geom_vline(data = tst.aonrs %>% 
               separate(aonr_rot, into = c("x", "rotation")), 
             aes(xintercept = aonr_kgha, color = rotation))

tst.prds <- 
  tst.mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,350)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,350)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 


#--want gap at rot aonr, then gap at cont aonr
#--wrong
# tst.diffs <- 
#   tst.prds %>% 
#   ungroup() %>% 
#   left_join(tst.aonrs) %>% 
#   filter(nrate_kgha == aonr_kgha) %>% 
#   select(site, year, rotation, pred_yield, aonr_rot)  %>% 
#     pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
#     mutate(gap_kgha = sc - cc) %>%
#     select(-cc, -sc) %>% 
#     pivot_wider(names_from = aonr_rot, values_from = gap_kgha) %>% 
#     mutate(
#       tot_gap = aonr_sc + aonr_cc,
#       n_pct = aonr_sc/tot_gap * 100)

tst.diffs <- 
  tst.prds %>% 
  ungroup() %>% 
  left_join(tst.aonrs) %>% 
  filter(nrate_kgha == aonr_kgha) %>% 
  select(site, year, rotation, pred_yield, aonr_rot)  %>% 
  pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  select(-cc, -sc) %>% 
  pivot_wider(names_from = aonr_rot, values_from = gap_kgha) %>% 
  rename("gap_at_rotaonr" = aonr_sc,
         "gap_at_contaonr" = aonr_cc) %>% 
  mutate(ngap_kgha = (gap_at_rotaonr - gap_at_contaonr)/gap_at_rotaonr,
         nonngap_kgah = 1-ngap_kgha)

#--compare to yield gap at max N
#--might be underestimating n-cont in some instances. 
#--if gap at highest n rate is smaller than gap at contaonr, we underestimated n cont.
tst.npct <- 
  ilia_gaps %>% 
  select(site, year, nrate_kgha, gap_kgha) %>% 
  left_join(tst.diffs) %>% 
  filter(!is.na(ngap_kgha))

tst.npct



# try on bigger subset of data --------------------------------------------

tst.tib <- 
  ilia_yields %>%
  filter(site == "ames")


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

tst.aonrs

#--visualize
tst.tib %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  geom_vline(data = tst.aonrs %>% 
               separate(aonr_rot, into = c("x", "rotation")), 
             aes(xintercept = aonr_kgha, color = rotation)) + 
  facet_wrap(~site+year)

#--get preds at many values
tst.mods <- 
  tst.tib %>% 
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(model = map(data, possibly(qpfit_fun, NULL))) %>% 
  rowwise() %>% 
  filter(!is.null(model)) %>%
  ungroup()


tst.prds <- 
  tst.mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,350)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,350)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 


#--want gap at rot aonr, then gap at cont aonr

tst.diffs <- 
  tst.prds %>% 
  ungroup() %>% 
  left_join(tst.aonrs) %>% 
  filter(nrate_kgha == aonr_kgha) %>% 
  select(site, year, rotation, pred_yield, aonr_rot)  %>% 
  pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  select(-cc, -sc) %>% 
  pivot_wider(names_from = aonr_rot, values_from = gap_kgha) %>% 
  rename("gap_at_rotaonr" = aonr_sc,
         "gap_at_contaonr" = aonr_cc) %>% 
  mutate(ngap_kgha = (gap_at_rotaonr - gap_at_contaonr)/gap_at_rotaonr,
         nonngap_kgah = 1-ngap_kgha)

#--compare to yield gap at max N
#--might be underestimating n-cont in some instances. 
#--if gap at highest n rate is smaller than gap at contaonr, we underestimated n cont.
tst.npct <- 
  ilia_gaps %>% 
  select(site, year, nrate_kgha, gap_kgha) %>% 
  left_join(tst.diffs) %>% 
  filter(!is.na(ngap_kgha))

tst.npct

tst.npct %>% 
  ggplot(aes(year, ngap_kgha)) + 
  geom_col() + 
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~site)

#--visualize checkin
yearq <- 2007
tst.tib %>% 
  filter(year == yearq) %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  geom_vline(data = tst.aonrs %>%
               filter(year == yearq) %>% 
               separate(aonr_rot, into = c("x", "rotation")), 
             aes(xintercept = aonr_kgha, color = rotation),
             size = 2) + 
  facet_grid(.~site+year)

tst.npct %>% 
  filter(year == 2007)

# try on all sites --------------------------------------------
#--note: what happens in years where no plateau is reached?

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

tst.aonrs

#--visualize (takes a long time...)
# tst.tib %>% 
#   filter(state == "IA") %>% 
#   ggplot(aes(nrate_kgha, yield_kgha)) + 
#   geom_point(aes(color = rotation)) + 
#   geom_vline(data = tst.aonrs %>% 
#                separate(aonr_rot, into = c("x", "rotation")), 
#              aes(xintercept = aonr_kgha, color = rotation)) + 
#   facet_wrap(~site+year)

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


tst.prds <- 
  tst.mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,350)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,350)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 


#--want gap at rot aonr, then gap at cont aonr

tst.diffs <- 
  tst.prds %>% 
  ungroup() %>% 
  left_join(tst.aonrs) %>% 
  filter(nrate_kgha == aonr_kgha) %>% 
  select(site, year, rotation, pred_yield, aonr_rot)  %>% 
  pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  select(-cc, -sc) %>% 
  pivot_wider(names_from = aonr_rot, values_from = gap_kgha) %>% 
  mutate(aonr_sc = ifelse(aonr_sc < 0, 0, aonr_sc),
         aonr_cc = ifelse(aonr_cc < 0, 0, aonr_cc)) %>% 
  rename("gap_at_rotaonr" = aonr_sc,
         "gap_at_contaonr" = aonr_cc) %>% 
  mutate(ngap_frac = (gap_at_rotaonr - gap_at_contaonr)/gap_at_rotaonr)

#--compare to yield gap at max N
#--might be underestimating n-cont in some instances. 
#--if gap at highest n rate is smaller than gap at contaonr, we underestimated n cont.
tst.npct <- 
  ilia_gaps %>% 
  select(site, year, nrate_kgha, gap_kgha) %>% 
  left_join(tst.diffs) %>% 
  mutate(ngap_frac = ifelse(is.nan(ngap_frac), 1, ngap_frac),  #--if there is no gap, nitrogen did all ofit
         ngap_frac = ifelse(gap_kgha < 0, 1, ngap_frac)) #--if cc yielded more than sc, it's all nitrogen

tst.npct

#--look at it by year
tst.npct %>% 
  ggplot(aes(year, ngap_frac)) + 
  geom_col() + 
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~site, scales = "free_x")

#--visualize checkin
yearq <- 2010
siteq <- "nash"

tst.tib %>% 
  filter(year == yearq) %>% 
  filter(site == siteq) %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation)) + 
  geom_vline(data = tst.aonrs %>%
               filter(year == yearq) %>%
               filter(site == siteq) %>% 
               separate(aonr_rot, into = c("x", "rotation")), 
             aes(xintercept = aonr_kgha, color = rotation),
             size = 2) + 
  facet_grid(.~site+year)

tst.npct %>% 
  filter(year == yearq) %>% 
  filter(site == siteq) 

#--the spread in gaps from low to high N should correspond to n-pcts
ilia_yields %>%
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>%
  mutate(gap_kgha = sc - cc) %>%
  filter(!is.na(gap_kgha)) %>%
  ggplot(aes(year, gap_kgha)) + 
  geom_jitter(aes(color = nrate_kgha)) + 
  facet_wrap(~site, scales = "free_x") + 
  scale_color_viridis_c()

#--avg the npct
tst.npct %>% 
  group_by(state, site) %>% 
  summarise(ngap_frac = mean(ngap_frac, na.rm = T)) %>% 
  ggplot(aes(reorder(site, -ngap_frac), ngap_frac)) + 
  geom_col(aes(fill = state)) + 
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) + 
  labs(y = "Percent of cont corn penalty due to nitrogen",
       title = "Nitrogen contribution to penalty, averaged over years",
       x = NULL)

#--put on map to see if there is a north-south gradient
map_ilia <- 
  as_tibble(map_data('state')) %>%
  filter(region %in% c("iowa", "illinois"))


#--overlaid circles (one each year)
ggplot() +
  geom_polygon(
    data = map_ilia,
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = "white"
  ) +
  geom_point(
    data = 
      tst.npct %>% 
      left_join(ilia_siteinfo),
    aes(x = long, 
        y = lat, 
        size = ngap_frac),
    pch = 21
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  guides(fill = F,
         color = F) +
  coord_quickmap()



#--one circle, average of years
ggplot() +
  geom_polygon(
    data = map_ilia,
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = "white"
  ) +
  geom_point(
    data = 
      tst.npct %>% 
      group_by(state, site) %>% 
      summarise(ngap_frac = mean(ngap_frac, na.rm = T)) %>% 
      left_join(ilia_siteinfo),
    aes(x = long, 
        y = lat, 
        size = ngap_frac),
    pch = 21,
    fill = "orange"
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  guides(fill = F,
         color = F) +
  coord_quickmap()






