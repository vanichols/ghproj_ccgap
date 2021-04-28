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


# map ---------------------------------------------------------------------

iail <- 
  map_data("state") %>% 
  filter(region %in% c("iowa", "illinois"))

fmap <- 
  ggplot() + 
  geom_polygon(data = iail, 
               aes(x = long, y = lat, group = group), color = "white", fill = "gray80") +
  geom_point(data = ilia_siteinfo,
             aes(x = long, y = lat, size = n), fill = ylw1, pch = 21) + 
  geom_text(data = ilia_siteinfo %>% mutate(lat = ifelse(manu_id == "IL-7", lat - 0.2, lat)),
           aes(x = long + 0.25, y = lat, label = manu_id),
           hjust = 0) + 
  labs(x = "Longitude", 
       y = "Latitude",
       size = "Site-years") + 
  coord_cartesian() + 
  theme(legend.justification = c(0, 0),
        legend.position = c(0.1, 0.1),
        legend.background = element_rect(color = "black"))

ggsave("05_manu-figs/fig_map.png")

# data --------------------------------------------------------------------

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
       y = "Maize Grain\n(dry Mg ha-1)") + 
  theme(legend.direction = "horizontal", 
        legend.justification = c(0, 1),
        legend.position = c(0.05, 0.95),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = rel(1.1)),
        axis.title.y = element_text(angle = 90, vjust = 0.5))



f1
ggsave("05_manu-figs/fig_gap-over-time.png", width = 6.5)


# combine -----------------------------------------------------------------

library(patchwork)

fmap + f1 + plot_layout(widths = c(1.8, 2))

ggsave("05_manu-figs/fig_map-pen-over-time.png", width = 13)
