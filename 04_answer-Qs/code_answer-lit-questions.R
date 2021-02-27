# Created:       jan 8 2021
#
# purpose: answer literature questions
#
# notes: 
# last edited:   2/5/2021, added gap versus res at diff nrates


rm(list = ls())
#devtools::install_github("femiguez/nlraa")
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(scales)
library(fancycut)
library(lme4)
library(lmerTest)

theme_set(theme_bw())

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


scale_this2 <- function(x){
  (x) / sd(x, na.rm=TRUE)
}


# data --------------------------------------------------------------------

gaps_alln <-
  ilia_yields %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(ogap_kgha = sc - cc,
         ogap_pct = ogap_kgha/sc)

gaps_maxn <- 
  ilia_gaps %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha))

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

#--look at gaps at high N category
gaps_alln %>% 
  mutate(nrate = cut_interval(nrate_kgha, n = 3),
         nrateF = as.numeric(nrate),
         nrateF = case_when(
           nrateF == 1 ~ "Low (0-90 kgN/ha)",
           nrateF == 2 ~ "Med (90-180 kgN/ha)",
           nrateF == 3 ~ "High (180-270 kgN/ha")
  ) %>% 
  filter(grepl("High", nrateF)) %>% 
  mutate(avg = (cc + sc)/2) %>% 
  pivot_longer(cols = c("cc", "sc", "avg")) %>% 
  ggplot(aes(value, ogap_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  facet_grid(.~name)

#--look at gaps at high N category
gaps_alln %>% 
  mutate(nrate = cut_interval(nrate_kgha, n = 3),
         nrateF = as.numeric(nrate),
         nrateF = case_when(
           nrateF == 1 ~ "Low (0-90 kgN/ha)",
           nrateF == 2 ~ "Med (90-180 kgN/ha)",
           nrateF == 3 ~ "High (180-270 kgN/ha")
  ) %>% 
  filter(grepl("High", nrateF)) %>% 
  mutate(avg = (cc + sc)/2) %>% 
  pivot_longer(cols = c("cc", "sc", "avg")) %>% 
  filter(name == "sc") %>% 
  ggplot(aes(value, ogap_pct)) + 
  geom_point() + 
  geom_vline(xintercept = 9000, color = "red") +
  facet_grid(.~name)


gaps_maxn %>%
  left_join(ilia_yields %>%
              group_by(site) %>%
              summarise(env_yield = mean(yield_kgha, na.rm = T))) %>%
  ggplot(aes(env_yield, gap_kgha)) +
  geom_point(aes(color = site)) +
  stat_summary(
    fun = "mean",
    geom = "point",
    size = 5,
    color = "black",
    pch  = 17
  ) +
  geom_smooth(
    method = "lm",
    se = F,
    linetype = "dashed",
    color = "black"
  ) +
  labs(title = "Penalty by site's average yields",
       x = "Avg corn yield (kg/ha)",
       y = "Penalty (kg/ha)")

ggsave("04_answer-Qs/fig_1-gap-vs-envyld.png")

gaps_maxn %>%
  left_join(ilia_yields %>%
              group_by(site) %>%
              summarise(env_yield = mean(yield_kgha, na.rm = T))) %>%
  ggplot(aes(env_yield, gap_pct)) +
  geom_point(aes(color = site)) +
  stat_summary(
    fun = "mean",
    geom = "point",
    size = 5,
    color = "black",
    pch  = 17
  ) +
  geom_smooth(
    method = "lm",
    se = F,
    linetype = "dashed",
    color = "black"
  ) +
  guides(color = F) +
  labs(title = "Penalty at highest N rate",
       subtitle = "by site's average yield",
       x = "Avg corn yield (kg/ha)",
       y = "Penalty (%)") +
  scale_y_continuous(labels = label_percent())

ggsave("04_answer-Qs/fig_1-gap-pct-vs-envyld.png")

mod_envyld <-
  lm(gap_kgha ~ env_yield,
     data = gaps_maxn %>%
       left_join(ilia_yields %>%
                   group_by(site) %>%
                   summarise(env_yield = mean(yield_kgha, na.rm = T))))

anova(mod_envyld)
# 2. drier areas have higher gaps -----------------------------------------

pcp_lt %>% 
  left_join(gaps_maxn) %>% 
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
gaps_maxn %>% 
  left_join(tav_sc1) %>% 
  left_join(pcp_sc1) %>% 
  ggplot(aes(pcp_sc, tav_sc)) + 
  geom_point(aes(size = gap_kgha)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)

#--color by gap
gaps_maxn %>% 
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

gaps_maxn %>% 
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

gaps_maxn %>% 
  left_join(ames_nmin) %>% 
  filter(!is.na(net_miner)) %>% 
  ggplot(aes(gross_miner, gap_kgha)) + 
  geom_point() + 
  labs(title = "Ames only")

all_nmin <- 
  read_csv("01_sims-oat-by-hand/sims-no-crop/dat_no-crops.csv")

gaps_maxn %>% 
  left_join(all_nmin) %>% 
  filter(!is.na(n_min_annual)) %>% #--only Iowa right now
  filter(gap_kgha > -1000) %>% 
  pivot_longer(n_min_annual:n_min_grow) %>% 
  ggplot(aes(value, gap_kgha)) + 
  geom_point(aes(color = name), size = 3) + 
  geom_smooth(method = "lm", se = F, aes(color = name)) +
  labs(title = "N mineralization versus gap at high N, Iowa")

ggsave("04_answer-Qs/fig_4-gap-vs-nmin.png")

ilia_yields %>% 
  filter(nrate_kgha == 0) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  bind_rows(gaps_maxn) %>% 
  left_join(all_nmin) %>% 
  filter(!is.na(n_min_annual)) %>% #--only Iowa right now
  filter(gap_kgha > -1000) %>% 
  pivot_longer(n_min_annual:n_min_grow) %>% 
  ggplot(aes(value, gap_kgha)) + 
  geom_point(aes(color = name), size = 3) + 
  geom_smooth(method = "lm", se = F, aes(color = name)) +
  facet_grid(.~nrate_kgha) +
  labs(title = "N mineralization versus gap at different Nrates, Iowa")

ggsave("04_answer-Qs/fig_4-gap-vs-nmin.png")



# 5. residue vs gap -------------------------------------------------------

###old 'uncalibrated' sims----
#--comes from Box simulations
ap_res <- 
  read_csv("../../../../Box/Gina_APSIM_modeling/apsim2/data-apsim-outs/02_wrangle-apsim-output/02_all-sites-noscripts.csv")
 
ap_res %>% 
  separate(outfile, into = c("x1", "rot", "x3")) %>% 
  filter(rot == "CC") %>% 
  select(set_id, site, year, corn_buac, ResidueWTatSowing) %>% 
  left_join(gaps_maxn %>% group_by(site) %>% filter(nrate_kgha == max(nrate_kgha))) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(ResidueWTatSowing, gap_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_wrap(~site, scales = "free")

ap_res %>% 
  separate(outfile, into = c("x1", "rot", "x3")) %>% 
  filter(rot == "CC") %>% 
  select(set_id, site, year, corn_buac, ResidueWTatSowing) %>% 
  left_join(gaps_maxn) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(ResidueWTatSowing, gap_kgha)) + 
  geom_point() 
  #geom_smooth(method = "lm", se = F, color = "red") 


###new sims from mitch 2/5----

#--need obs nrateas to match
gaps_alln2 <- 
  gaps_alln %>% 
  filter(state == "IA") %>% 
  select(-cc, -sc, -state, -crop) 

dres <- 
  ia_sims %>% 
  select(-res_sowing_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(sgap_kgha = sc - cc) %>% 
  select(-cc, -sc) %>% 
  left_join(ia_sims %>% filter(rotation == "cc") %>% select(-yield_kgha)) %>% 
  left_join(gaps_alln2) %>% 
  filter(!is.na(ogap_kgha))

dres %>% 
  filter(res_sowing_kgha > 7500)

dres %>%  
  filter((!(site == "suth" & year == 2001))) %>% 
  filter(nrate_kgha %in% c(270)) %>% 
  filter(ogap_kgha > 0) %>% 
  ggplot(aes(res_sowing_kgha, ogap_kgha)) + 
  geom_point() + 
  labs(title = "Obs gap not related to simulated surface residue at planting",
       subtitle = "At 270 kgN, excludes Suth 2001, 10000 kg res") +
  #geom_smooth(method = "lm", se = F, (aes(color = nrate_kgha))) + 
  facet_wrap(~nrate_kgha, scales = "free", ncol = 2)
  #facet_grid(nrate_kgha ~ .)

###new sims from mitch again, 2/25
resdat <- 
  ilia_simsall %>% 
  filter(rotation == "cc") %>% 
  select(sim_type, state, site, year, nrate_kgha, residue_w_tat_sowing, stover_wt) %>% 
  mutate(nrate_kgha = round(nrate_kgha, 0)) %>% 
  left_join(
    gaps_alln %>% 
      select(-cc, -sc, -crop) 
  ) %>% 
  filter(!is.na(ogap_kgha))


resdat %>%
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  filter(sim_type == "cal_scripts") %>% 
  ggplot(aes(residue_w_tat_sowing, ogap_kgha)) + 
  geom_point(aes(color = sim_type), color = "gray50") + 
  geom_smooth(method = "lm", color = "red", se = F) +
  facet_wrap(~site, scales = "free") + 
  labs(title = "Penalty vs residue")

resdat %>%
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  filter(sim_type == "cal_scripts") %>% 
  ggplot(aes(stover_wt, ogap_kgha)) + 
  geom_point(aes(color = sim_type), color = "gray50") + 
  geom_smooth(method = "lm", color = "red", se = F) +
  facet_wrap(~site, scales = "free") + 
  labs(title = "Penalty vs stover prod")

resdat %>%
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  filter(sim_type == "cal_scripts") %>% 
  ggplot(aes(stover_wt, residue_w_tat_sowing)) + 
  geom_point(aes(color = sim_type), color = "gray50") + 
  geom_smooth(method = "lm", color = "red", se = F) +
  facet_wrap(~site, scales = "free") + 
  labs(title = "Penalty vs stover prod")


ilia_simsall %>% 
  select(sim_type:yield_kgha, stover_wt) %>% 
  filter(stover_wt < 0)

###prev cc yield (res)----
#--practive
ilia_yields %>% 
  mutate_if(is.numeric, round, 0) %>% 
  filter(site == "ames") %>% 
  filter(rotation == "cc") %>% 
  filter(nrate_kgha %in% c(0, 270),
         year %in% c(2002, 2003, 2004)) %>%
  arrange(nrate_kgha, year) %>% 
  group_by(nrate_kgha) %>% 
  mutate(yield_lag = lag(yield_kgha))

prev_yld <- 
  ilia_yields %>% 
  filter(rotation == "cc") %>% 
  arrange(site, nrate_kgha, year) %>% 
  group_by(site, nrate_kgha) %>%
  mutate(prev_cc_yield = lag(yield_kgha)) %>% 
  select(-rotation, -crop, -yield_kgha)

gaps_alln %>% 
  left_join(prev_yld) %>% 
  ggplot(aes(prev_cc_yield, ogap_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~nrate_kgha) + 
  labs(title = "Prev CC yield (~residue) not related to obs penalty")



# 6. gaps at 0 vs HN ------------------------------------------------------

ilia_yields %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  group_by(site) %>% 
  filter((nrate_kgha == 0 | nrate_kgha == max(nrate_kgha))) %>% 
  mutate(nrate_F = ifelse(nrate_kgha == 0, "none", "high")) %>% 
  select(-cc, -sc, -nrate_kgha) %>% 
  pivot_wider(names_from = nrate_F, values_from = gap_kgha) %>% 
  ggplot(aes(none, high)) + 
  geom_point(aes(color = site)) + 
  geom_smooth(method = "lm", se = F, aes(color = site)) + 
  labs(title = "Gap at high N not strongly related to gap at low N within a site",
       x = "Gap at 0N",
       y = "Gap at highest N")


ilia_yields %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  group_by(site) %>% 
  filter((nrate_kgha == 0 | nrate_kgha == max(nrate_kgha))) %>% 
  mutate(nrate_F = ifelse(nrate_kgha == 0, "none", "high")) %>% 
  select(-cc, -sc, -nrate_kgha) %>% 
  pivot_wider(names_from = nrate_F, values_from = gap_kgha) %>% 
  ggplot(aes(none, high)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = "red") + 
  #facet_wrap(~site, scales = "free") + 
  labs(title = "Gap at high N not strongly related to gap at low N",
       subtitle = "Across sites, confounding factors",
       x = "Gap at 0N",
       y = "Gap at highest N")

#--gap related to sc yield?
gaps_alln %>% 
  ggplot(aes(sc, ogap_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_wrap(~nrate_kgha) + 
  labs(title = "Observed penalty vs observed rotated corn yield",
       #subtitle = "Relationship strongest at 0N, reduces to no relation >150 kg/ha",
       subtitle = "At low N, gap is driven by SC yields")


#--gap at high N related to sc yield at 0N?
gaps_alln %>% 
  group_by(site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  left_join(
    ilia_yields %>% 
              filter(nrate_kgha == 0,
                     rotation == "sc") %>% 
      rename("sc0_yield" = yield_kgha) %>% 
      select(state, site, year, sc0_yield)
  ) %>% 
  ggplot(aes(sc0_yield, ogap_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = "red") +
  labs(title = "Gap at high N not related to sc yield at 0N")

gaps_alln %>% 
  ggplot(aes(cc, ogap_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = "green") +
  facet_wrap(~nrate_kgha) + 
  labs(title = "Observed penalty vs cont rotated corn yield",
       subtitle = "At high N, gap driven by CC yields")


# 7. Penalty over time? ---------------------------------------

#--obseved gaps, all sites

gaps_alln %>% 
  mutate(nrate = cut_interval(nrate_kgha, n = 3),
         nrateF = as.numeric(nrate),
         nrateF = case_when(
           nrateF == 1 ~ "Low (0-90 kgN/ha)",
           nrateF == 2 ~ "Med (90-180 kgN/ha)",
           nrateF == 3 ~ "High (180-270 kgN/ha")
  ) %>% 
  arrange(nrate) %>% 
  mutate(nrateF = fct_inorder(nrateF)) %>% 
  ggplot(aes(year, ogap_kgha)) + 
  geom_jitter() + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_grid(.~nrateF, scales = "free_x") + 
  labs(title = "Continuous corn penalty persists over time at all N rates")


#--obseved gaps by years in corn (instead of year)

gaps_alln %>%
  filter(!is.na(ogap_kgha)) %>% 
  group_by(site) %>% 
  mutate(years_in_corn = year - min(year)) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  
  bind_rows(
    gaps_alln %>%
      filter(!is.na(ogap_kgha)) %>% 
      group_by(site) %>% 
      mutate(years_in_corn = year - min(year)) %>% 
      filter(nrate_kgha == max(nrate_kgha)) %>% 
      mutate(site = "zcombined")
  ) %>% 
  
  ggplot(aes(years_in_corn, ogap_kgha)) + 
  geom_point(color = "gray30") + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_wrap(~site)




#--look at cc and sc yields in addition to gap over time

fig_gap <-
  ilia_yields %>%
  bind_rows(
    gaps_alln %>%
      mutate(
        nrate = cut_interval(nrate_kgha, n = 3),
        nrateF = as.numeric(nrate),
        nrateF = case_when(
          nrateF == 1 ~ "Low (0-90 kgN/ha)",
          nrateF == 2 ~ "Med (90-180 kgN/ha)",
          nrateF == 3 ~ "High (180-270 kgN/ha")
        )  %>%
      mutate(rotation = "gap",
             yield_kgha = ogap_kgha)
  ) %>%
  bind_rows(
    gaps_alln %>%
      mutate(
        nrate = cut_interval(nrate_kgha, n = 3),
        nrateF = as.numeric(nrate),
        nrateF = case_when(
          nrateF == 1 ~ "Low (0-90 kgN/ha)",
          nrateF == 2 ~ "Med (90-180 kgN/ha)",
          nrateF == 3 ~ "High (180-270 kgN/ha")
      )  %>%
      mutate(rotation = "gap_pct",
             yield_kgha = ogap_pct)
  ) %>% 
  mutate(
    nrate = cut_interval(nrate_kgha, n = 3),
    nrateF = as.numeric(nrate),
    nrateF = case_when(
      nrateF == 1 ~ "Low (0-90 kgN/ha)",
      nrateF == 2 ~ "Med (90-180 kgN/ha)",
      nrateF == 3 ~ "High (180-270 kgN/ha"
    )
  ) %>%
  arrange(nrate) %>%
  mutate(nrateF = fct_inorder(nrateF),
         rotation = factor(rotation, levels = c("sc", "cc", "gap", "gap_pct")))


#--separated by state
fig_gap %>% 
  ggplot(aes(year, yield_kgha)) + 
  geom_jitter(aes(color = nrateF), alpha = 0.5) + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_grid(rotation~nrateF+state, scales = "free") 

# #--179 site years
# fig_dat %>% 
#   unite(site, year, col = "siteyear") %>% 
#   select(siteyear) %>% 
#   distinct()

#--all together
fig_gap %>% 
  ggplot(aes(year, yield_kgha)) + 
  geom_jitter(color = "gray80") + 
  geom_smooth(method = "lm", se = F, color = "red", size = 2) +
  facet_grid(rotation~nrateF, scales = "free") +
  labs(title = "179 site-years, IA and IL",
       subtitle = "Continuous corn penalty has not changed over time")

ggsave("04_answer-Qs/fig_7-gap-over-time.png", height = 12)

#--stats to get slope
fig_gap

m7sc <- lmer(yield_kgha ~ year + (1|site),
           data = fig_gap %>% filter(rotation == "sc",
                                     grepl("High", nrateF)))
m7cc <- lmer(yield_kgha ~ year + (1|site),
           data = fig_gap %>% filter(rotation == "cc",
                                     grepl("High", nrateF)))
summary(m7cc)
summary(m7sc)

summary(lmer(yield_kgha ~ 1 + (1|site),
               data = fig_gap %>% filter(grepl("High", nrateF))))
  

fig_gap %>% 
  filter(grepl("High", nrateF)) %>% 
  summarise(ogap_kgha = mean(ogap_kgha, na.rm = T))

summary(lmer(ogap_kgha ~ 1 + (1|site),
             data = fig_gap %>% filter(grepl("High", nrateF))))
fig_gap %>% 
  filter(grepl("Low", nrateF)) %>% 
  summarise(ogap_kgha = mean(ogap_kgha, na.rm = T))

summary(lmer(ogap_kgha ~ 1 + (1|site),
             data = fig_gap %>% filter(grepl("Low", nrateF))))
