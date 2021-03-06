# Created:    april 2 2021
#
# purpose: make gap over time fig
#
# notes: 
# last edited:   

rm(list = ls())
#devtools::install_github("femiguez/nlraa")
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(fancycut)
library(patchwork)
library(scales)
library(maps)
library(ggrepel)

theme_set(theme_bw())

source("05_manu-figs/palettes.R")


# high N data --------------------------------------------------------------------

gaps_alln <-
  ilia_yields %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  mutate(ogap_kgha = sc - cc,
         ogap_pct = ogap_kgha/sc) %>% 
  mutate(nrate = cut_interval(nrate_kgha, n = 3),
         nrateF = as.numeric(nrate),
         nrateF2 = case_when(
           nrateF == 1 ~ "Low (0-90 kgN/ha)",
           nrateF == 2 ~ "Med (90-180 kgN/ha)",
           nrateF == 3 ~ "High (180-270 kgN/ha")
  ) 


gaps_highn <- 
  gaps_alln %>%
  filter(nrateF == 3)
  



# over time ---------------------------------------------------------------

fig_gap <-
  ilia_yields %>%
  bind_rows(
    gaps_alln %>%
      mutate(
        nrate = cut_interval(nrate_kgha, n = 3),
        nrateF = as.numeric(nrate),
        nrateF2 = case_when(
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
        nrateF2 = case_when(
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
    nrateF2 = case_when(
      nrateF == 1 ~ "Low (0-90 kgN/ha)",
      nrateF == 2 ~ "Med (90-180 kgN/ha)",
      nrateF == 3 ~ "High (180-270 kgN/ha"
    )
  ) %>%
  arrange(nrate) %>% 
  filter(nrateF == 3) %>% 
  filter(rotation != "gap_pct") 

fig_gap %>% pull(year) %>% max()
fig_gap %>% select(site, year) %>% distinct()
fig_gap %>% filter(state == "IA") %>% select(site) %>% distinct()

f1 <- 
  fig_gap %>%
  mutate(rot_nice = case_when(
    grepl("sc", rotation) ~ "Rotated maize",
    grepl("cc", rotation) ~ "Continuous maize",
    grepl("gap", rotation) ~ "Continuous maize penalty"),
    rot_nice = factor(rot_nice, levels = c("Rotated maize", "Continuous maize", "Continuous maize penalty"))
  ) %>% 
  ggplot(aes(year, yield_kgha/1000)) + 
  geom_jitter(aes(shape = rot_nice, color = rot_nice)) + 
  geom_smooth(method = "lm", se = F, aes(color = rot_nice), size = 2) +
  scale_color_manual(values = c("Continuous maize" = pnk1, 
                                "Rotated maize" = dkbl1,
                                "Continuous maize penalty" = grn1)) +
  scale_shape_manual(values = c("Continuous maize" = 24, 
                                "Rotated maize" = 21,
                                "Continuous maize penalty" = 22)) +
  labs(x = NULL,
       color = NULL,
       shape = NULL,
       y = expression(Maize~Grain~(dry~Mg~ha^{-1}))) + 
  theme(legend.direction = "horizontal", 
        legend.justification = c(0, 1),
        legend.position = c(0.05, 0.95),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = rel(1.1)),
        axis.title.y = element_text(angle = 90, vjust = 0.5))



f1
#ggsave("05_manu-figs/fig_gap-over-time.png")



# anor method -------------------------------------------------------------



#--use the anor method. also do a pct?

preds <- read_csv("00_empirical-n-cont/dat_preds.csv")
aonr <- read_csv("00_empirical-n-cont/dat_aonrs.csv")


dat_modA <- 
  aonr %>% 
  separate(aonr_rot, into = c("aonr", "rotation")) %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  left_join(preds) %>% 
  select(-nrate_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = pred_yield) %>% 
  mutate(gap_kgha = sc - cc) %>% 
  filter(!is.na(gap_kgha)) %>% 
  mutate(gap_kgha = ifelse(gap_kgha < 0, 0, gap_kgha)) %>% 
  mutate(gap_pct = gap_kgha/sc) %>% 
  pivot_longer(cc:gap_pct) %>% 
  mutate(year0 = year - min(year))


dat_modA %>%
  filter(name != "gap_pct") %>% 
  mutate(rot_nice = case_when(
    grepl("sc", name) ~ "Rotated maize",
    grepl("cc", name) ~ "Continuous maize",
    grepl("gap_kgha", name) ~ "Continuous maize penalty"),
    rot_nice = factor(rot_nice, levels = c("Rotated maize", "Continuous maize", "Continuous maize penalty"))
  ) %>% 
  ggplot(aes(year, value/1000)) + 
  geom_jitter(aes(shape = rot_nice, color = rot_nice)) + 
  geom_smooth(method = "lm", se = F, aes(color = rot_nice), size = 2) +
  scale_color_manual(values = c("Continuous maize" = pnk1, 
                                "Rotated maize" = dkbl1,
                                "Continuous maize penalty" = grn1)) +
  scale_shape_manual(values = c("Continuous maize" = 24, 
                                "Rotated maize" = 21,
                                "Continuous maize penalty" = 22)) +
  labs(x = NULL,
       color = NULL,
       shape = NULL,
       y = expression(Maize~Grain~(dry~Mg~ha^{-1}))) + 
  theme(legend.direction = "horizontal", 
        legend.justification = c(0, 1),
        legend.position = c(0.05, 0.95),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = rel(1.1)),
        axis.title.y = element_text(angle = 90, vjust = 0.5))

ggsave("05_manu-figs/fig_gap-over-time.png")


