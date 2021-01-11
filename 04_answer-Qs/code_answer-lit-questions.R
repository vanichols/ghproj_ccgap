# Created:       jan 8 2021
# last edited:   
#
# purpose: answer literature questions
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


# weather data --------------------------------------------------------------------

pcp_ann <- 
  ilia_wea %>% 
  group_by(state, year, site) %>% 
  summarise(pcp_mm = sum(precip_mm, na.rm = T))  %>% 
  ungroup()

pcp_lt <- 
  pcp_ann %>% 
  group_by(site) %>% 
  summarise(pcp_lt = mean(pcp_mm))

tav_ann <- 
  ilia_wea %>% 
  mutate(tav_c = (tmax_c + tmin_c)/2) %>% 
  group_by(state, site, year) %>% 
  summarise(tav_c = mean(tav_c, na.rm = T)) %>% 
  ungroup()

tav_lt <- 
  tav_ann %>% 
  group_by(site) %>% 
  summarise(tav_lt = mean(tav_c))



# 1. high yields mean lower gaps? --------------------------------------------


ilia_gaps %>%
  left_join(ilia_yields %>%
              group_by(site) %>%
              summarise(env_yield = mean(yield_kgha, na.rm = T))) %>% 
  ggplot(aes(env_yield, gap_kgha)) + 
  geom_point(aes(color = site)) + 
  stat_summary(fun = "mean", geom = "point", size = 5, color = "black", pch  = 17) + 
  geom_smooth(method = "lm", se = F, linetype = "dashed", color = "black") +
  labs(title = "Penalty by site's average yields",
       x = "Avg corn yield (kg/ha)",
       y = "Penalty (kg/ha)")

  ggsave("04_answer-Qs/fig_1-gap-vs-envyld.png")


mod_envyld <- 
  lm(gap_kgha ~ env_yield, data = ilia_gaps %>%
     left_join(ilia_yields %>%
                 group_by(site) %>%
                 summarise(env_yield = mean(yield_kgha, na.rm = T))))

anova(mod_envyld)


# 2. drier areas have higher gaps -----------------------------------------

pcp_lt %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_lt, gap_kgha)) + 
  geom_point(aes(size = pcp_lt), color = "red") + 
  stat_summary(fun = "mean", geom = "point", size = 5, color = "black", pch  = 17) + 
  labs(title = "Penalty by long-term site avg precip",
       x = "Avg annual precip (mm)",
       y = "Penalty (kg/ha)")

ggsave("04_answer-Qs/fig_2-gap-vs-ltprecip.png")



# 3. drought years have higher penalties -------------------------------------

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

#--test
tav_sc1 %>% 
  left_join(pcp_sc1) %>% 
  ggplot(aes(pcp_sc, tav_sc)) + 
  geom_point(aes(color = site)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)


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
  geom_point(aes(color = gap_kgha, size = gap_kgha), pch= 19) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  scale_fill_gradient2(low = "red", high = "darkblue", midpoint = 0) +
  scale_color_gradient2(low = "red", high = "darkblue", midpoint = 0) +
  geom_text(x = -2, y = 2.5, label = "Hot and dry") +
  geom_text(x = 2, y = -2, label = "Cold and wet") +
  geom_text(x = 2, y = 2.5, label = "Hot and wet") +
  geom_text(x = -2, y = -2, label = "Cold and dry") +
  labs(title = "Scaled within site-years",
       x = "Annual Precipitation Total",
       y = "Annual Mean Air Temperature")



### scale to LT average rather than data average----
pcp_ann %>% 
  group_by(site) %>%
  summarise(n = n())

#--great, about even for LT calcs 

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
  geom_point(aes(color = gap_kgha, size = gap_kgha), pch= 19) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  scale_fill_gradient2(low = "red", high = "darkblue", midpoint = 0) +
  scale_color_gradient2(low = "red", high = "darkblue", midpoint = 0) +
  geom_text(x = -2, y = 2.5, label = "Hot and dry") +
  geom_text(x = 2, y = -2, label = "Cold and wet") +
  geom_text(x = 2, y = 2.5, label = "Hot and wet") +
  geom_text(x = -2, y = -2, label = "Cold and dry") +
  labs(title = "Scaled to 'long-term'* mean within site",
       subtitle = "40 years",
       x = "Annual Precipitation Total",
       y = "Annual Mean Air Temperature")

ggsave("04_answer-Qs/fig_3-gap-vs-drought.png")



# 4. Nmin vs penalty ------------------------------------------------------

ames_nmin <- 
  read_csv("01_sims-oat-by-hand/sims-ames-CCbase/dat-ames-CCbase-raw.csv") %>% 
  select(rot, apsim_oat, year, gross_miner, net_miner) %>% 
  mutate(site = "ames") %>% 
  filter(apsim_oat == "base") %>% 
  filter(rot == "CC")

ilia_gaps %>% 
  left_join(ames_nmin) %>% 
  filter(!is.na(net_miner)) %>% 
  ggplot(aes(gross_miner, gap_kgha)) + 
  geom_point() + 
  labs(title = "Ames only")

all_nmin <- 
  read_csv("01_sims-oat-by-hand/sims-no-crop/dat_no-crops.csv")

ilia_gaps %>% 
  left_join(all_nmin) %>% 
  filter(!is.na(n_min)) %>% #--only Iowa right now
  ggplot(aes(n_min, gap_kgha)) + 
  geom_point()


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


# site-LTprecip vs gap ---------------------------------------------------------------

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

mod_pcplt <- 
  lm(gap_kgha ~ pcp_lt, 
   data = pcp_lt %>% 
     left_join(ilia_gaps) %>% 
     filter(!is.na(gap_kgha)))

anova(mod_pcplt)     

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


#--are annual and 2wk correlated?
pcp_ann1 %>% 
  left_join(ilia_gaps) %>%
  left_join(wea) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(pcp_mm, prep2wk_precip_mm_tot)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)



# planting date by gap ----------------------------------------------------

ia_planting %>% 
  bind_rows(il_planting) %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(plant_doy, gap_kgha)) + 
  geom_point(aes(color = site)) + 
  geom_smooth(aes(color = site), method = "lm", se = F) + 
  facet_grid(.~state)
