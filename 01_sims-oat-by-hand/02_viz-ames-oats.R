# Created:      3/12/2020
#
# purpose: evaluate each factor individually and in combination using CALIBRATED APSIM sims 
#          use CC sim as base, tweaking from that
#          changed apsim files by hand (ames only)
#
# author: gina vnichols@iastate.edu
#
# notes: keep apsim sims in box, all r code in github
# last edited:   3/31/2020 (I was confused...)
#                11/23/2020 cleaning, added rfv50% (oat22)

rm(list = ls())
library(saapsim) #--has some functions
library(tidysawyer2) #--has sawyer data
library(tidyverse)
library(janitor)


# read in output from read-in-sim-res code --------------------------------

#--oats
oat_key <- 
  read_csv("01_sims-oat-by-hand/rd_oats-calibrated-v1CC.csv") %>%  
  separate_rows(category, sep = ",") %>% 
  remove_empty("rows") %>% 
  remove_empty("cols") 

#--comes from 01_read-in-sim-res
apraw <- read_csv("01_sims-oat-by-hand/sims_apsim-hand-oat-raw.csv")

#--apsim cc yields
base_contc <- 
  apraw %>%
  mutate(oat_nu = parse_number(apsim_oat)) %>%
  filter(rot == "CC", apsim_oat == "base") %>% 
  mutate(dtype = "ap_contc", 
         oat_nu = 0) %>% 
  select(dtype, oat_nu, year, yield_kgha)

#--apsim cs yields, un-tweaked (this is just for reference)
base_rotc <- 
  apraw %>%
  mutate(oat_nu = parse_number(apsim_oat)) %>%
  filter(rot != "CC", apsim_oat == "base") %>% 
  filter(yield_kgha != 0) %>% #--remove soybean years
  mutate(dtype = "ap_rotc",
         oat_nu = 0) %>% 
  select(dtype, oat_nu, year, yield_kgha)


# working apd ------------------------------------------------------------------

apw <- 
  apraw %>% 
  filter(!grepl("base", apsim_oat), 
         yield_kgha != 0) %>% 
  mutate(oat_nu = parse_number(apsim_oat),
         dtype = "ap_contcoat") %>% 
  select(dtype, oat_nu, year, yield_kgha) %>% 
  arrange(oat_nu, year)

# calc yield gaps ---------------------------------------------------------

ewgap <-
  saw_cgap %>% 
  ungroup() %>% 
  filter(site == "ames") %>% 
  mutate(dtype = "exp_gap", 
         oat_nu = 0,
         gap_kgha = cgap_max) %>%  #--just renaming it lazily
  filter(!is.na(gap_kgha)) %>% 
  select(dtype, oat_nu, year, gap_kgha)


agap <- 
  apw %>% #--contc w/tweaks
  left_join(select(base_rotc, - oat_nu), by = "year") %>% 
  mutate(gap_kgha = yield_kgha.y - yield_kgha.x,
         dtype = "oat_gap") %>% 
  select(dtype, oat_nu, year, gap_kgha)

agap_notweaks <- 
  base_rotc %>% 
  left_join(select(base_contc, -dtype), by = c("year", "oat_nu")) %>% 
  mutate(gap_kgha = yield_kgha.x - yield_kgha.y,
         dtype = "oat_gapnotweaks",
         oat_nu = 99) %>% 
  select(dtype, oat_nu, year, gap_kgha)


gaps <- 
  ewgap %>% 
  bind_rows(agap) %>% 
  bind_rows(agap_notweaks) %>% 
  left_join(oat_key) 


#--what is that one year? 2000. Huh. 

gaps %>% 
  filter(!category %in% c("2 factor")) %>% 
  ggplot(aes(reorder(oat_what, gap_kgha), gap_kgha)) + 
  geom_boxplot(aes(color = oat_what %in% c("exp gap", "current apsim gap"))) +
  geom_point(aes(pch = year == 2000, size = year == 2000)) + 
  coord_flip() + 
  guides(color = F) +
  labs(title = "ames") + 
  facet_grid(category~., scales = "free")

ewgap



gaps_filt <- 
  gaps %>% 
  #--2000 was the beginning year, sims are bad at first years
  filter(year > 2000)
  

# look at gaps ------------------------------------------------------------

#--for quality check (oat number)
gaps_filt %>% 
 # filter(category %in% c("4 factor")) %>% 
  ggplot(aes(reorder(as_factor(oat_nu), gap_kgha), gap_kgha)) + 
  geom_boxplot(aes(color = oat_what %in% c("exp gap", "current apsim gap"))) +
  geom_point() + 
  coord_flip() + 
  guides(color = F) +
  labs(title = "ames") + 
  facet_wrap(~category, scales = "free")

#--all factor combos
gaps_filt %>% 
  filter(!category %in% c("2 factor")) %>% 
  ggplot(aes(reorder(oat_what, gap_kgha), gap_kgha)) + 
  geom_point(color = "gray80") + 
  geom_boxplot(aes(color = oat_what %in% c("exp gap", "current apsim gap")), alpha = 0.5) +
  coord_flip() + 
  guides(color = F) +
  labs(title = "ames") + 
  facet_grid(category~., scales = "free") + 
  theme_bw()

ggsave("01_sims-oat-by-hand/fig_gaps-boxplot.png")

#--just 1 factor
gaps_filt %>% 
  filter(category %in% c("1 factor")) %>% 
  ggplot(aes(reorder(oat_what, gap_kgha), gap_kgha)) + 
  geom_point(color = "gray80") + 
  geom_boxplot(aes(color = oat_what %in% c("exp gap", "current apsim gap")), alpha = 0.5) +
  coord_flip() + 
  guides(color = F) +
  labs(title = "ames") + 
  facet_grid(category~., scales = "free") + 
  theme_bw()


#--try windmill plots

yrs_ord <- 
  gaps_filt %>% 
  filter(oat_what == "exp gap", category == "1 factor") %>% 
  arrange(gap_kgha) %>% 
  select(year) %>% 
  pull()

gaps_filt_wind <- 
  gaps_filt %>%
  mutate(year = factor(year, levels = yrs_ord),
         oat_what = case_when(
           grepl("exp gap", oat_what) ~ "1_exp gap",
           grepl("current", oat_what) ~ "2_current apsim gap",
           TRUE ~ oat_what)) %>% 
  group_by(dtype, oat_what, category) %>% 
  mutate(mngap = mean(gap_kgha, na.rm = T))

wind_theme <-    theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank())


fac1 <- 
  gaps_filt_wind %>%
  filter(category == "1 factor") %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = oat_what),
               #alpha = oat_what %in% c("1_exp gap")),
           position = "dodge", stat = "identity", color = "black") + 
  facet_grid(oat_what~category) + 
  guides(fill = F, alpha = F) +
  coord_flip() + 
  labs(title = "Ames",
       x = NULL) +
  geom_hline(aes(yintercept = mngap), linetype = "dotted", size = 1.2) +
  wind_theme

fac1
ggsave("01_sims-oat-by-hand/fig_gaps-1fac-windmill.png")


fac2 <- 
  gaps_filt_wind %>% 
  filter(category == "3 factor") %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = oat_what),
               #alpha = oat_what %in% c("1_exp gap")),
           position = "dodge", stat = "identity", color = "black") + 
  facet_grid(oat_what ~ category) + 
  guides(fill = F, alpha = F) +
  coord_flip() + 
  geom_hline(aes(yintercept = mngap), linetype = "dotted", size = 1.2) +
  labs(title = "Ames",
       x = NULL) + 
  wind_theme

fac2
ggsave("01_sims-oat-by-hand/fig_gaps-3fac-windmill.png", width = 3.5, height = 7.5)


fac3 <- 
  gaps_filt_wind %>%
  filter(category == "4 factor") %>% 
  mutate(year = factor(year, levels = yrs_ord)) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = oat_what),
               #alpha = oat_what %in% c("1_exp gap")),
           position = "dodge", stat = "identity", color = "black") + 
  facet_grid(oat_what ~category) + 
  guides(fill = F, alpha = F) +
  coord_flip() + 
  geom_hline(aes(yintercept = mngap), linetype = "dotted", size = 1.2) +
  labs(title = "Ames",
       x = NULL) + 
  wind_theme

fac3


fac4 <- 
  gaps_filt_wind %>%
  filter(category == "5 factor") %>% 
  mutate(year = factor(year, levels = yrs_ord)) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = oat_what),
           #alpha = oat_what %in% c("1_exp gap")),
           position = "dodge", stat = "identity", color = "black") + 
  facet_grid(oat_what ~category) + 
  guides(fill = F, alpha = F) +
  coord_flip() + 
  geom_hline(aes(yintercept = mngap), linetype = "dotted", size = 1.2) +
  labs(title = "Ames",
       x = NULL) + 
  wind_theme

fac4

ggsave("01_sims-oat-by-hand/fig_gaps-5fac-windmill.png")


library(patchwork)
fac1 + fac2 #+ fac3 

ggsave("01_sims-oat-by-hand/fig_gaps-windmill.png")
