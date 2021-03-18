# created 2/12/21
# gina
# purpose: mike thinks I can get some kind of info from aonr stuff
# updated

rm(list = ls())
library(tidysawyer2)
library(tidyverse)
library(scales)
library(ggmosaic)
library(ggExtra)

theme_set(theme_bw())

# data --------------------------------------------------------------------


tst.tib <- 
  ilia_yields


tst.npct <- 
  read_csv("00_empirical-n-cont/fits-npct.csv") %>% 
  left_join(tst.tib %>% select(state, site) %>% distinct())

npct_mean <- 
  tst.npct %>% 
  summarise(ngap_frac = mean(ngap_frac, na.rm = T)) %>% 
  pull(ngap_frac)

tst.npct %>% 
  ggplot(aes(ngap_frac)) + 
  geom_histogram(bins = 40)

p1 <- 
  tst.npct %>% 
  filter(!is.na(ngap_frac)) %>% 
  arrange(-ngap_frac) %>% 
  mutate(rown = 1:n(),
         rown2 = rown/max(rown)) %>% 
  ggplot(aes(rown2, ngap_frac, color = state)) + 
  geom_point() + 
  geom_hline(yintercept = npct_mean, color = "red") +
  geom_segment(aes(x = rown2, xend = rown2, y = 0, yend = ngap_frac)) +
  coord_flip() +
  theme(axis.text.y = element_blank()) + 
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Observation",
       y = "Percent of gap closed by Nfert")

ggExtra::ggMarginal(p1, margins = "x", type = "histogram", bins = 40)

ggsave("00_empirical-n-cont/fig_gap-nfrac-lollypop.png")


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

#--seems like site?
m0 <- lm(ngap_frac ~ site * year, data = tst.anova)
anova(m0)

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

# tst.tib %>% 
#   filter(year == yearq) %>% 
#   filter(site %in% siteq) %>% 
#   ggplot(aes(nrate_kgha, yield_kgha, color = rotation)) + 
#   geom_point() +
#   geom_line() +
#   geom_vline(data = tst.aonrs %>%
#                filter(year == yearq) %>%
#                filter(site %in% siteq) %>%
#                separate(aonr_rot, into = c("x", "rotation")),
#              aes(xintercept = aonr_kgha, color = rotation),
#              size = 2) +
#    facet_grid(site~year)
# 
# tst.npct %>% 
#   filter(year == yearq) %>% 
#   filter(site == siteq) 
  
#--the spread in gaps from low to high N should correspond to n-pcts?
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
