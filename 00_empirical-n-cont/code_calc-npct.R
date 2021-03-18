# created 2/12/21
# gina
# purpose: mike thinks I can get some kind of info from aonr stuff
# updated

library(tidysawyer2)
library(tidyverse)
library(scales)
library(ggmosaic)

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
  left_join(tst.aonrs) %>%
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
  left_join(tst.aonrs) %>%
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

tst.npct %>% write_csv("00_empirical-n-cont/fits-npct.csv")


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





# nice fig, each year nfrac ---------------------------------------------------------------

tst.npct %>% 
  filter(!is.na(ngap_frac)) %>%
  select(site, year, ngap_frac) %>% 
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

ggsave("00_empirical-n-cont/fig_gap-nfrac.png")



# ggmosaic ----------------------------------------------------------------


#--idea = should do a mosaic plot, with widths related to size of gap
#--done w/miranda's help

#--take averages of sites
tst.npct %>% 
  filter(!is.na(ngap_frac)) %>%
  select(site, year, ngap_frac) %>%
  group_by(site) %>% 
  summarise(ngap_frac = mean(ngap_frac)) %>%
  arrange(-ngap_frac) %>% 
  mutate(site = fct_inorder(site)) %>% 
  mutate(anonngap_frac = 1 - ngap_frac) %>% 
  pivot_longer(ngap_frac:anonngap_frac) %>% 
  ggplot(aes(site, value)) + 
  geom_col(aes(fill = name)) + 
  scale_y_continuous(labels = label_percent()) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c("gray60", "red")) +
  labs(title = "Percentage of penalty related to nitrogen",
       subtitle = "Empirical approach, averaged over site-years")

ggsave("00_empirical-n-cont/fig_gap-nfrac-site-avg.png")


#--take averages of sites, just nfrac w/errors
tst.npct %>% 
  filter(!is.na(ngap_frac)) %>%
  select(site, year, ngap_frac) %>%
  group_by(site) %>% 
  mutate(mn = mean(ngap_frac)) %>% 
  arrange(-mn) %>% 
  mutate(site = fct_inorder(site)) %>% 
  ggplot(aes(reorder(site,-mn), ngap_frac)) + 
  stat_summary(geom = "bar", fill = "red") +
  stat_summary(geom = "errorbar", width = 0.2) +
  scale_y_continuous(labels = label_percent()) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c("gray60", "red")) +
  labs(title = "Percentage of penalty related to nitrogen",
       subtitle = "Empirical approach, averaged over site-years",
       x = "Site",
       y = "Percentage of penalty overcome by nitrogen fertilization")

ggsave("00_empirical-n-cont/fig_gap-nfrac-site-avg-errors.png")

