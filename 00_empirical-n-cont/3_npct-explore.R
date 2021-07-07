# created 2/12/21
# gina
# purpose: mike thinks I can get some kind of info from aonr stuff
# updated 3/19/2021 - separated exploration from calculations

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

aonrs <- 
  read_csv("00_empirical-n-cont/dat_aonrs.csv")
  
# npct <- 
#   read_csv("00_empirical-n-cont/dat_gap-components.csv")
#   left_join(tst.tib %>% select(state, site) %>% distinct())

nsims <- 
  read_csv("00_empirical-n-cont/dat_npct-sims.csv") %>% 
  left_join(tst.tib %>% select(state, site) %>% distinct())

#--gap components
npct <- read_csv("00_empirical-n-cont/dat_gap-components.csv") %>%
  mutate(gap = nonngap + ngap) %>% 
  left_join(ilia_siteinfo %>% select(site, state))

npct_mean <- 
  npct %>% 
  summarise(ngap_frac = mean(ngap_frac, na.rm = T)) %>% 
  pull(ngap_frac)


# quad plat gap vs max N gap ----------------------------------------------

#--how does gap estimated by quad plat differ from gap at max N?
ilia_gaps %>%
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(state, site, year, gap_kgha) %>% 
  left_join(npct %>% select(site, year, gap_at_contaonr_kgha)) %>% 
  ggplot(aes(gap_kgha, gap_at_contaonr_kgha)) + 
  geom_point(aes(color = site)) +
  geom_abline()


#obs sim components vs obs ---------------------------------------------------

#--which sims are these? myabe this isn't wht i think it is
nsims %>%
  rename("sim_gap" = 3,
         "sim_nonn" = 4) %>%
  mutate(sim_n = sim_gap - sim_nonn) %>%
  select(site, year, sim_gap, sim_nonn, sim_n) %>%
  left_join(
    npct %>%
      rename("obs_gap" = 3,
             "obs_nonn" = 4) %>%
      mutate(obs_n = obs_gap - obs_nonn) %>%
      select(site, year, obs_gap, obs_nonn, obs_n)
  ) %>% 
  ggplot(aes(sim_n, obs_n)) + 
  geom_point() + 
  geom_abline()


ilia_gaps %>%
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  select(state, site, year, gap_kgha) %>% 
  left_join(npct %>% select(site, year, gap_at_contaonr_kgha)) %>% 
  filter(gap_kgha < 1000,
         gap_at_contaonr_kgha > 2000)



# n vs non-n comp ---------------------------------------------------------


#--not related at all
npct %>% 
  ggplot(aes(ngap, nonngap)) + 
  geom_point(aes(color = site, size = gap))



# higher sc aonr = higher n pct? ------------------------------------------

aonrs %>% 
  filter(aonr_rot == "aonr_sc") %>% 
  left_join(npct %>% 
              mutate(gap_npct = gap_n/gap)) %>% 
  ggplot(aes(aonr_kgha, gap_npct)) + 
  geom_point()

# lollypop fig npct -------------------------------------------------------

p1 <- 
  npct %>% 
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

#ggsave("00_empirical-n-cont/fig_gap-nfrac-lollypop.png")


#--look at it by year
npct %>% 
  filter(!is.na(ngap_frac)) %>% 
  ggplot(aes(as.factor(year), ngap_frac)) + 
  geom_col() + 
  scale_y_continuous(labels = label_percent()) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~site, scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 45))


# is site or year cont more? ----------------------------------------------
library(lme4)
library(lmerTest)
library(emmeans)

#--does year or site have a bigger impact?

#--make it a site-year
dat_anova <-
  npct %>% 
  ungroup() %>% 
  mutate(year = paste(site, year, sep = "_")) %>% 
  select(site, ngap_frac, year) %>% 
  filter(!is.na(ngap_frac))

dat_anova

#--site is 32% of total, using this method
m0 <- lm(ngap_frac ~ site * year, data = dat_anova)
anova(m0)

4.6595/(4.6596+9.853)

#--include site as random, whatever is left over is site-year
m1 <- lmer(ngap_frac ~ 1 + (1|site), data = dat_anova)
m2 <- lm(ngap_frac ~ site, data = dat_anova) #--but not same # of points...

AIC(m1, m2) #--lm is better? You want low AIC, right?
anova(m2)
summary(m1)

#--site is 27% using this method
(0.03419) / (0.03419+0.09243)


# include residue in models? ----------------------------------------------

#--want highest yield achieved in cc in the year beofre

prev_yield <- 
  ilia_yields %>% 
  filter(rotation == "cc") %>% 
  group_by(state, site, year, rotation) %>% 
  summarise(max_yield = max(yield_kgha, na.rm = T)) %>%
  group_by(state, site) %>% 
  mutate(prev_yr_ccyield = lag(max_yield)/1000) %>% 
  ungroup() %>% 
  select(site, year, prev_yr_ccyield) %>% 
  unite(site, year, col = "year") 

dat_anova2 <- 
  dat_anova %>% 
  left_join(prev_yield)

m3 <- lmer(ngap_frac ~ prev_yr_ccyield + (1|site), data = dat_anova2)
summary(m3)


# pie chart of variation --------------------------------------------------

dpie <- tibble(name = c("Site", "Year"),
               rxsum = c(0.3, 0.7))

# You have to do weird things to make the labels get on the right place
dpie %>%
  mutate(name = factor(name, levels = c("Year", "Site")),
         name2 = fct_rev(name)) %>% 
  arrange(name2) %>% 
  mutate(half = rxsum/2,
         prev = lag(rxsum),
         prev = ifelse(is.na(prev), 0, prev),
         cumprev = cumsum(prev),
         pos = half + cumprev)  %>%
  ggplot(aes(x = "", y = rxsum, fill = name)) +
  geom_bar(stat = "identity") +
  guides(fill = F) +
  coord_polar("y", start = 0) + 
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(y = pos,
                label = name), size = 5) + 
  scale_fill_manual(values = c("deepskyblue", "red"))



# get fixed effect estimates for each site ---------------------------------------------

#--fixed effect estimates for site
emmeans(m2, specs = "site") %>% 
  broom::tidy() %>% 
  arrange(estimate) %>%
  left_join(ilia_yields %>% select(state, site) %>% distinct()) %>% 
#  mutate(site = fct_inorder(site)) %>% 
  ggplot(aes(reorder(site, -estimate), estimate)) + 
  geom_col(aes(fill = state)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  scale_y_continuous(label = label_percent()) + 
  labs(x = NULL,
       y = "Percent of cont corn penalty due to nitrogen",
       title = "Nitrogen contribution to penalty, averaged over years")


# get averrage prod of each site
site_avgs <- 
  ilia_yields %>% 
  filter(nrate_kgha > 150) %>% 
  group_by(state, site) %>% 
  summarise(site_yield_avg = mean(yield_kgha))


#--fixed effect estimates for site
emmeans(m2, specs = "site") %>% 
  broom::tidy() %>% 
  arrange(estimate) %>%
  left_join(ilia_yields %>% select(state, site) %>% distinct()) %>% 
  left_join(site_avgs) %>% 
  ggplot(aes(reorder(site, -estimate), estimate)) + 
  geom_col(aes(fill = site_yield_avg, color = state), size = 2) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  scale_y_continuous(label = label_percent()) + 
  scale_color_manual(values = c("white", "black")) +
  scale_fill_viridis_c() +
  labs(x = NULL,
       y = "Percent of cont corn penalty due to nitrogen",
       title = "Nitrogen contribution to penalty, averaged over years")

#ggsave("00_empirical-n-cont/fig_gap-nfrac-site-avg.png")

# use random effect apporach ----------------------------------------------


#--try blubs of mixed model
coef(m1)$site %>% 
#ranef(m1) %>% 
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>%
  rename(site = 1,
         blup = 2) %>% 
  left_join(npct %>% select(site, state) %>% distinct()) %>% 
  arrange(blup) %>% 
  mutate(site = fct_inorder(site),
         site = fct_rev(site)) %>% 
  ggplot(aes(site, blup)) + 
  geom_col(aes(fill = state)) + 
  # geom_linerange(aes(ymin = condval - condsd,
  #                    ymax = condval + condsd)) + 
  scale_y_continuous(label = label_percent()) + 
  labs(x = NULL,
       y = "Percent of cont corn penalty due to nitrogen",
       title = "Deviation from mean")

dat_anova %>% 
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
# npct %>% 
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
npct %>% 
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
      npct %>% 
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
      npct %>% 
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
  left_join(npct) %>% 
  ggplot(aes(avgyield_kgha, ngap_frac)) + 
  geom_point()

  



# nice fig, each year nfrac ---------------------------------------------------------------

npct %>% 
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

#ggsave("00_empirical-n-cont/fig_gap-nfrac.png")



# ggmosaic ----------------------------------------------------------------


#--idea = should do a mosaic plot, with widths related to size of gap
#--done w/miranda's help

#--take averages of sites
npct %>% 
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

#ggsave("00_empirical-n-cont/fig_gap-nfrac-site-avg.png")


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

ggsave("00_empirical-n-cont/fig_gap-nfrac-site-avg-errors.png")



# fert rates --------------------------------------------------------------


