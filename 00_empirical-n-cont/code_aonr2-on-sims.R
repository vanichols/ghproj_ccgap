# created 2/12/21
# gina
# purpose: find n-cont from sim data

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

#--do some sites have omre probs than others?
tst.noconv %>% 
  ggplot(aes(site)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~rotation)
#--really it's just the sc I'm concerned about. 
#--if I can ignore the CC instances and just do gaps at highest N (or something)

tst.prds <- 
  tst.mods %>%
  mutate(pred_yield = map(model, .f = predict, newdata = data.frame(nrate_kgha = seq(0,300)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(nrate_kgha = as.numeric(as.character(seq(0,300)))) %>% 
  select(site, year, rotation, nrate_kgha, pred_yield) 


#--want gap at rot aonr, then gap at cont aonr
#--maybe not
# tst.diffs <- 
#   tst.prds %>% 
#   ungroup() %>% 
#   left_join(tst.aonrs) %>% 
#   filter(nrate_kgha == aonr_kgha) %>% 
#   select(site, year, rotation, pred_yield, aonr_rot)  %>% 
#   pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
#   mutate(gap_kgha = sc - cc) %>% 
#   select(-cc, -sc) %>% 
#   pivot_wider(names_from = aonr_rot, values_from = gap_kgha) %>% 
#   mutate(aonr_sc = ifelse(aonr_sc < 0, 0, aonr_sc),
#          aonr_cc = ifelse(aonr_cc < 0, 0, aonr_cc)) %>% 
#   rename("gap_at_rotaonr" = aonr_sc,
#          "gap_at_contaonr" = aonr_cc) %>% 
#   mutate(ngap_frac = (gap_at_rotaonr - gap_at_contaonr)/gap_at_rotaonr)

#--rotaonr in brow 2005 was 60?! Yup
tst.tib %>% 
  filter(site == "brow", 
         year == 2006) %>% 
  ggplot(aes(nrate_kgha, yield_kgha, color = rotation)) + 
  geom_point(size = 3) 

#--try: gap at rot aonr, then smallest gap above rot aonr
tst.rotaonrgap <- 
  tst.prds %>%
  ungroup() %>%
  left_join(tst.aonrs) %>%
    filter(aonr_rot == "aonr_sc") %>% 
  filter(nrate_kgha == aonr_kgha) %>%
  select(site, year, rotation, pred_yield, aonr_rot)  %>%
  pivot_wider(names_from = rotation, values_from = pred_yield) %>%
  mutate(gap_at_rotaonr_kgha = sc - cc) %>%
  select(-cc, -sc) %>%
  mutate(gap_at_rotaonr_kgha = ifelse(gap_at_rotaonr_kgha < 0, 0, gap_at_rotaonr_kgha))

#--I'm not convinced this is best. What if sc peaks at it's aonr then goes down?
tst.biggestgap <- 
  ilia_yields %>% 
  left_join(tst.aonrs) %>%
  filter(aonr_rot == "aonr_sc") %>% 
  filter(nrate_kgha > aonr_kgha) %>%
  select(state, site, year, rotation, nrate_kgha, yield_kgha)  %>%
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>%
  mutate(gap_at_highn_kgha = sc - cc) %>%
  group_by(state, site, year) %>% 
  mutate(mingap = min(gap_at_highn_kgha)) %>% #--I think?
  filter(gap_at_highn_kgha == mingap) %>% 
  mutate(gap_at_highn_kgha = ifelse(gap_at_highn_kgha < 0, 0, gap_at_highn_kgha)) %>% 
  select(-cc, -sc, -mingap)

tst.npct <- 
  tst.biggestgap %>% 
  left_join(tst.rotaonrgap) %>% 
  mutate(ngap_frac = (gap_at_rotaonr_kgha - gap_at_highn_kgha)/gap_at_rotaonr_kgha,
         ngap_frac = ifelse(ngap_frac < 0, 0, ngap_frac))


#--compare to yield gap at max N
#--might be underestimating n-cont in some instances. 
#--if gap at highest n rate is smaller than gap at contaonr, we underestimated n cont.
# tst.npct <- 
#   ilia_gaps %>% 
#   select(site, year, nrate_kgha, gap_kgha) %>% 
#   left_join(tst.diffs) %>% 
#   mutate(ngap_frac = ifelse(is.nan(ngap_frac), 1, ngap_frac),  #--if there is no gap, nitrogen did all ofit
#          ngap_frac = ifelse(gap_kgha < 0, 1, ngap_frac)) #--if cc yielded more than sc, it's all nitrogen

tst.npct

#--look at it by year
tst.npct %>% 
  filter(!is.na(ngap_frac)) %>% 
  ggplot(aes(as.factor(year), ngap_frac)) + 
  geom_col() + 
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~site, scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 45))

#--does year or site have a bigger impact?
tst.anova <-
  tst.npct %>% 
  ungroup() %>% 
  mutate(year = paste(site, year, sep = "_")) %>% 
  select(site, ngap_frac, year) %>% 
  filter(!is.na(ngap_frac))


# is site or year contributing more? --------------------------------------
library(lme4)
library(lmerTest)
library(emmeans)

tst.anova

m1 <- lmer(ngap_frac ~ 1 + (1|site), data = tst.anova)
m2 <- lm(ngap_frac ~ site, data = tst.anova) #--but not same # of points...

AIC(m1, m2) #--lm is better?
anova(m2)
emmeans(m2, specs = "site") %>% 
  broom::tidy() %>% 
  arrange(estimate) %>%
#  mutate(site = fct_inorder(site)) %>% 
  ggplot(aes(reorder(site, -estimate), estimate)) + 
  geom_col() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  scale_y_continuous(label = label_percent()) + 
  labs(x = NULL,
       y = "Percent of cont corn penalty due to nitrogen",
       title = "Nitrogen contribution to penalty, averaged over years")

#--try blubs of mixed model
ranef(m1) %>% 
  as.data.frame() %>%
  as_tibble() %>%  
  arrange(condval) %>%
  #  mutate(site = fct_inorder(site)) %>% 
  ggplot(aes(reorder(grp, -condval), condval)) + 
  geom_col() + 
  geom_linerange(aes(ymin = condval - condsd,
                     ymax = condval + condsd)) + 
  scale_y_continuous(label = label_percent()) + 
  labs(x = NULL,
       y = "Percent of cont corn penalty due to nitrogen",
       title = "Deviation from mean")

tst.anova %>% 
  mutate(preds = predict(m1)) %>% 
  select(site, preds) %>% 
  distinct() %>% 
  #  mutate(site = fct_inorder(site)) %>% 
  ggplot(aes(reorder(site, -preds), preds)) + 
  geom_col() +  
  scale_y_continuous(label = label_percent()) + 
  labs(x = NULL,
       y = "Percent of cont corn penalty due to nitrogen",
       title = "Deviation from mean")



#--why are dsup and dslo different?



#--visualize checkin
yearq <- 2015
siteq <- c("nash")

tst.tib %>% 
  filter(year == yearq) %>% 
  filter(site %in% siteq) %>% 
  ggplot(aes(nrate_kgha, yield_kgha, color = rotation)) + 
  geom_point() +
  geom_line() +
  geom_vline(data = tst.aonrs %>%
               filter(year == yearq) %>%
               filter(site %in% siteq) %>%
               separate(aonr_rot, into = c("x", "rotation")),
             aes(xintercept = aonr_kgha, color = rotation),
             size = 2) +
   facet_grid(site~year)

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


#--is the npct related to yields?
ilia_yields %>% 
  group_by(site) %>% 
  summarise(avgyield_kgha = mean(yield_kgha)) %>% 
  left_join(tst.npct) %>% 
  ggplot(aes(avgyield_kgha, ngap_frac)) + 
  geom_point()

  



# nice figs ---------------------------------------------------------------

#--make a nice npct one

tst.npct %>% 
  filter(!is.na(ngap_frac)) %>% 
  #filter(state == "IA") %>%
  select(state, site, year, ngap_frac) %>% 
  mutate(anonngap_frac = 1 - ngap_frac) %>% 
  pivot_longer(ngap_frac:anonngap_frac) %>% 
  ggplot(aes(as.factor(year), value)) + 
  geom_col(aes(fill = name)) + 
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~site, scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c("gray60", "red")) +
  labs(title = "Percentage of penalty related to nitrogen",
       subtitle = "Empirical approach")

#--idea = should do a mosaic plot, with widths related to size of gap
