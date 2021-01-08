# Created:       jan 8 2021
# last edited:   
#
# purpose: are gaps larger in hot/dry years w/in a site?
#
# notes: 


rm(list = ls())
#devtools::install_github("wilkelab/ungeviz")
library(tidysawyer2) 
library(tidyverse)
library(saapsim)

theme_set(theme_bw())

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


scale_this2 <- function(x){
  (x) / sd(x, na.rm=TRUE)
}


# data --------------------------------------------------------------------

pcp_ann <- 
  ilia_wea %>% 
  group_by(state, year, site) %>% 
  summarise(pcp_mm = sum(precip_mm, na.rm = T))  %>% 
  ungroup()

tav_ann <- 
  ilia_wea %>% 
  mutate(tav_c = (tmax_c + tmin_c)/2) %>% 
  group_by(state, site, year) %>% 
  summarise(tav_c = mean(tav_c, na.rm = T)) %>% 
  ungroup()


# within site, rel to obs wea ---------------------------------------------

###precip----
pcp_ann1 <- 
  ia_planting %>% 
  bind_rows(il_planting) %>% 
  select(site, year) %>% 
  left_join(pcp_ann) %>% 
  filter(!is.na(pcp_mm)) 

#--test
tst <- 
  pcp_ann1 %>% 
  filter(site %in% c('ames', 'deka'),
                          year %in% c(2000, 2001, 2002))

tst %>% 
  group_by(site) %>% 
  group_modify(~{
    .x %>% mutate(pcp_sc = scale_this(pcp_mm))
  })

tst %>% 
  filter(site == "deka") %>% 
  group_by(site) %>% 
  group_modify(~{
    .x %>% mutate(pcp_sc = scale_this(pcp_mm))
  })

#--yes
pcp_sc1 <- 
  pcp_ann1 %>% 
  group_by(state, site) %>% 
  group_modify(~{
    .x %>% mutate(pcp_sc = scale_this(pcp_mm))
  })


###temp----
tav_ann1 <- 
  ia_planting %>% 
  bind_rows(il_planting) %>% 
  select(site, year) %>% 
  left_join(tav_ann) %>% 
  filter(!is.na(tav_c))

tav_sc1 <- 
  tav_ann1 %>% 
  group_by(state, site) %>% 
  group_modify(~{
    .x %>% mutate(tav_sc = scale_this(tav_c))
  })



###look at wea----
tav_sc1 %>% 
  left_join(pcp_sc1) %>% 
  ggplot(aes(pcp_sc, tav_sc)) + 
  geom_point(aes(color = site)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)


###add gap info----

ilia_gaps <- 
  ilia_yields %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(gap_kgha = sc - cc,
         gap_pct = gap_kgha/sc)

#--size by gap
ilia_gaps %>% 
  left_join(tav_sc1) %>% 
  left_join(pcp_sc1) %>% 
  ggplot(aes(pcp_sc, tav_sc)) + 
  geom_point(aes(size = gap_kgha)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

#--color by gap
ilia_gaps %>% 
  left_join(tav_sc1) %>% 
  left_join(pcp_sc1) %>%
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_sc, tav_sc)) + 
  geom_point(aes(fill = gap_kgha), pch= 21, size = 4) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  scale_fill_gradient2(low = "red", high = "darkblue", midpoint = 0) +
  labs(title = "Scaled within site-years",
       x = "Annual Precipitation Total",
       y = "Annual Mean Air Temperature")


# within site, rel to LTwea --------------------------------------------


pcp_ann %>% 
  group_by(site) %>%
  summarise(n = n())

#--need more weather data for Iowa sites

# pretend for now

pcp_lt <- 
  pcp_ann %>% 
  group_by(site) %>% 
  summarise(pcp_lt = mean(pcp_mm))

tav_lt <- 
  tav_ann %>% 
  group_by(site) %>% 
  summarise(tav_lt = mean(tav_c))

ilia_gaps %>% 
  left_join(tav_ann) %>% 
  left_join(pcp_ann) %>% 
  left_join(pcp_lt) %>% 
  left_join(tav_lt) %>% 
  mutate(pcp_cen = pcp_mm - pcp_lt,
         tav_cen = tav_c - tav_lt) %>%
  group_by(state, site) %>% 
  group_modify(~{
    .x %>% mutate(tav_sc = scale_this2(tav_cen),
                  pcp_sc = scale_this2(pcp_cen))
  }) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_sc, tav_sc)) + 
  geom_point(aes(fill = gap_kgha), pch = 21, size = 4) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  scale_fill_gradient2(low = "red", high = "darkblue", midpoint = 0) +
  labs(title = "Scaled to 'long-term'* mean within site",
       subtitle = "Iowa sites only have 17 years, Illinois has 40",
       x = "Annual Precipitation Total",
       y = "Annual Mean Air Temperature")


#--what is missing? starts of exp, no problem

# across sites -------------------------------------------------


pcp_ann2 <- 
  pcp_ann %>% 
  mutate(pcp_sc = scale_this(pcp_mm))

tav_ann2 <- 
  tav_ann %>% 
  mutate(tav_sc = scale_this(tav_c))

ilia_gaps %>% 
  left_join(tav_ann2) %>% 
  left_join(pcp_ann2) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_sc, tav_sc)) + 
  geom_point(aes(fill = gap_kgha), pch = 21, size = 4) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  scale_fill_gradient2(low = "red", high = "darkblue", midpoint = 0) +
  labs(title = "Scaled across sites",
       x = "Annual Precipitation Total",
       y = "Annual Mean Air Temperature")



# with yield amt ----------------------------------------------------------

ilia_gaps %>% 
  left_join(tav_ann) %>% 
  left_join(pcp_ann) %>% 
  left_join(pcp_lt) %>% 
  left_join(tav_lt) %>% 
  mutate(pcp_cen = pcp_mm - pcp_lt,
         tav_cen = tav_c - tav_lt) %>%
  group_by(state, site) %>% 
  group_modify(~{
    .x %>% mutate(tav_sc = scale_this2(tav_cen),
                  pcp_sc = scale_this2(pcp_cen))
  }) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_sc, tav_sc)) + 
  geom_point(aes(fill = gap_kgha, size = sc), 
             pch = 21) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  scale_fill_gradient2(low = "red", high = "darkblue", midpoint = 0) +
  labs(title = "Scaled to 'long-term'* mean within site",
       subtitle = "Iowa sites only have 17 years, Illinois has 40",
       x = "Annual Precipitation Total",
       y = "Annual Mean Air Temperature")


#--dslo and dsup have the same weather...
#--one doesn't necessarily have larger penalties than the other
ilia_gaps %>% 
  filter(site %in% c("dsup", "dslo")) %>%
  select(site, year, gap_kgha) %>% 
  pivot_wider(names_from = site, values_from = gap_kgha) %>% 
  ggplot(aes(dslo, dsup)) + 
  geom_point() + 
  geom_abline()


# dry sites ---------------------------------------------------------------

pcp_lt %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(reorder(site, pcp_lt), gap_kgha)) + 
  geom_point(aes(size = pcp_lt), color = "red") + 
  stat_summary(fun = "mean", geom = "point", size = 5, color = "black", pch  = 17) + 
  labs(title = "Penalty by long-term site avg precip")


pcp_lt %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_lt, gap_kgha)) + 
  geom_point(aes(size = pcp_lt), color = "red") + 
  stat_summary(fun = "mean", geom = "point", size = 5, color = "black", pch  = 17) + 
  labs(title = "Penalty by long-term site avg precip",
       x = "Avg annual precip (mm)",
       y = "Penalty (kg/ha)")

ggsave("00_exp-explore/fig_gap-vs-ltprecip.png")

# dry years ---------------------------------------------------------------

pcp_ann1 %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_mm, gap_kgha)) + 
  geom_point(color = "red", size = 4) + 
  geom_smooth(method = "lm", se = F) +
  labs(title = "Penalty by site-year annual precip",
       x = "Annual precip (mm)",
       y = "Penalty (kg/ha)")

anova(lm(gap_kgha ~ pcp_mm, data = pcp_ann1 %>% 
     left_join(ilia_gaps) %>% 
     filter(!is.na(gap_kgha))))

pcp_lt %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_lt, gap_kgha)) + 
  geom_point(aes(size = pcp_lt), color = "red") + 
  stat_summary(fun = "mean", geom = "point", size = 5, color = "black", pch  = 17) + 
  labs(title = "Penalty by long-term site avg precip",
       x = "Avg annual precip (mm)",
       y = "Penalty (kg/ha)")

ggsave("00_exp-explore/fig_gap-vs-ltprecip.png")


# 2 wk wet years ---------------------------------------------------------------

wea <- read_csv("01_create-features/1_dat_preds-wea.csv")

wea %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(prep2wk_precip_mm_tot, gap_kgha)) + 
  geom_point(color = "red", size = 4) + 
  geom_smooth(method = "lm", se = F) +
  labs(title = "Penalty by pre-plant 2 wk precip",
       x = "Cumulative precip the two weeks before planting (mm)",
       y = "Penalty (kg/ha)")


anova(lm(gap_kgha ~ prep2wk_precip_mm_tot, 
         data = 
           wea %>% 
           left_join(ilia_gaps) %>% 
           filter(!is.na(gap_kgha))))

