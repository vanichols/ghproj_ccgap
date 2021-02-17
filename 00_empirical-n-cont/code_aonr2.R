# created 2/12/21
# gina
# purpose: mike thinks I can get some kind of info from aonr stuff
# updated

library(tidysawyer2)
library(tidyverse)
library(scales)


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




  


# mixed model? ------------------------------------------------------------
