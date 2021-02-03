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

library(scales)

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

ilia_gaps %>%
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
     data = ilia_gaps %>%
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
  bind_rows(ilia_gaps) %>% 
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
#--comes from Box simulations

ap_res <- 
  read_csv("../../../Box/Gina_APSIM_modeling/apsim2/data-apsim-outs/02_wrangle-apsim-output/02_all-sites-noscripts.csv") 

ap_res %>% 
  separate(outfile, into = c("x1", "rot", "x3")) %>% 
  filter(rot == "CC") %>% 
  select(set_id, site, year, corn_buac, ResidueWTatSowing) %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(ResidueWTatSowing, gap_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = "red") +
  facet_wrap(~site, scales = "free")

ap_res %>% 
  separate(outfile, into = c("x1", "rot", "x3")) %>% 
  filter(rot == "CC") %>% 
  select(set_id, site, year, corn_buac, ResidueWTatSowing) %>% 
  left_join(ilia_gaps) %>% 
  filter(!is.na(gap_kgha)) %>% 
  ggplot(aes(ResidueWTatSowing, gap_kgha)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, color = "red") 

ggsave("04_answer-Qs/fig_5-gap-vs-residue.png")


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
  geom_smooth(method = "lm", se = F, aes(color = site))

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
  facet_wrap(~site, scales = "free")


