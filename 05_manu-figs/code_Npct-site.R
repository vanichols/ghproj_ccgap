# Created:    july 7 2021
#
# purpose: make fig of pct N
#
# notes: 
# last edited:   

rm(list = ls())
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(fancycut)
library(patchwork)
library(scales)
library(naniar)
library(lme4)
library(lmerTest)
library(emmeans)

theme_set(theme_bw())

source("05_manu-figs/palettes.R")

# data --------------------------------------------------------------------


#--gap components
npct <- read_csv("00_empirical-n-cont/dat_gap-components.csv") %>%
  mutate(gap = nonngap + ngap)

npct_mean <- 
  npct %>% 
  summarise(ngap_frac = mean(ngap_frac, na.rm = T)) %>% 
  pull(ngap_frac)


#--shoudl I get the mean using a modeL?probably
mod_dat <- 
  npct %>% 
  filter(!is.na(gap)) %>% 
  mutate(yearF = as.factor(year)) 

summary(lmer(ngap_frac ~ 1 + (1|site) + (1|yearF), data = mod_dat))
# mean of 0.394

#--take averages of sites
npct %>% 
  filter(!is.na(ngap_frac)) %>%
  select(site, year, ngap_frac) %>%
  left_join(ilia_siteinfo) %>% 
  group_by(manu_id) %>% 
  summarise(ngap_frac = mean(ngap_frac)) %>%
  arrange(-ngap_frac) %>% 
  mutate(manu_id = fct_inorder(manu_id)) %>% 
  mutate(anonngap_frac = 1 - ngap_frac) %>% 
  pivot_longer(ngap_frac:anonngap_frac) %>% 
  ggplot(aes(manu_id, value)) + 
  geom_col(aes(fill = name)) + 
  scale_y_continuous(labels = label_percent()) +
  geom_hline(yintercept = 0.394, linetype = "dashed") +
  coord_cartesian(ylim = c(0, 1)) +
#  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c("gray60", ylw1)) +
  labs(title = "Percentage of penalty related to nitrogen",
       subtitle = "Empirical approach, averaged over site-years")

ggsave("00_empirical-n-cont/fig_gap-nfrac-site-avg.png")


#--take averages of sites, just nfrac w/errors
npct %>% 
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


# use models to get bars --------------------------------------------------


m1 <- lmer(ngap_frac ~ 1 + (1|site) + (1|yearF), data = mod_dat)
m2 <- lmer(ngap_frac ~ site + (1|yearF), data = mod_dat) #--but not same # of points...

AIC(m1, m2) #--lm is better? You want low AIC, right?
# m1 is much better

m3 <-lmer(ngap_frac ~ 1 + (1|site), data = mod_dat)

AIC(m1, m3)
#--m3 is much simpler

a <- summary(m3)
est <- (a[["coefficients"]] %>% as.vector())[1]


cis <- 
  confint(m3) %>% 
  as.data.frame() %>% 
  rownames_to_column()


mod_res <- 
  ranef(m3)$site %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  mutate(est = est) %>% 
  janitor::clean_names() %>% 
  mutate(
    est_cl = cis[3, 2],
    est_cu = cis[3,3],
    cond_est = est + intercept,
    cond_cl = est_cl + intercept,
    cond_cu = est_cu + intercept,
    site = rowname) %>% 
  select(site, cond_est, cond_cl, cond_cu) %>% 
  left_join(ilia_siteinfo) 


mod_res %>% 
  ggplot(aes(reorder(manu_id, -cond_est), cond_est)) + 
  geom_col(aes(fill = state)) + 
  geom_hline(yintercept = est, linetype = "dashed") +
  geom_linerange(aes(ymin = cond_cl,
                     ymax = cond_cu)) + 
  scale_y_continuous(label = label_percent(), limits = c(0, 1)) + 
  scale_fill_manual(values = c(rd2, dkpr2)) +
  labs(x = NULL,
       y = "Percentage of full penalty (%)",
       fill = "State")

ggsave("05_manu-figs/fig_Npct-site.png")

#--versus latitude?

mod_res %>% 
  ggplot(aes(cond_est, lat)) + 
  geom_point(aes(color = state)) 


#--latitude has a clear temperature gradient

tav_lt <- 
  ilia_wea %>% 
  mutate(tav_c = (tmax_c + tmin_c)/2) %>% 
  group_by(state, site) %>% 
  summarise(tav_c = mean(tav_c, na.rm = T)) %>% 
  ungroup()

pcp_lt <- 
  pcp_ann %>% 
  group_by(site) %>% 
  summarise(pcp_lt = mean(pcp_mm))

#--what are the range in lat, pcp, and temp?
mod_res %>% 
  summarise(mn = min(lat),
            mx = max(lat))

mod_res %>% 
  filter(lat == min(lat))

tav_lt %>% 
  summarise(mn = min(tav_c),
            mx = max(tav_c))

pcp_lt %>% 
  summarise(mn = min(pcp_lt),
            mx = max(pcp_lt))


mod_res %>% 
  left_join(tav_ann) %>% 
  ggplot(aes(cond_est, tav_c)) + 
  geom_point(aes(color = state)) 


mod_res %>% 
  left_join(tav_ann %>% 
              group_by(site) %>% 
              summarise(tav_c = mean(tav_c))) %>% 
  ggplot(aes(reorder(manu_id, -cond_est), cond_est)) + 
  geom_col(aes(fill = tav_c)) + 
  geom_hline(yintercept = est, linetype = "dashed") +
  geom_linerange(aes(ymin = cond_cl,
                     ymax = cond_cu)) + 
  scale_y_continuous(label = label_percent(), limits = c(0, 1)) + 
  #scale_fill_manual(values = c(rd2, dkpr2)) +
  labs(x = NULL,
       y = "Percentage of full penalty (%)",
       fill = "Mean Air Temperature (deg C)") + 
  theme(legend.direction = "horizontal",
        legend.position = "bottom") + 
  guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5))
