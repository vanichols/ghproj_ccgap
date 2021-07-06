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

ilia_siteinfo

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
  coord_quickmap() +
  #coord_cartesian() + 
  theme(legend.justification = c(0, 0),
        legend.position = c(0.1, 0.1),
        legend.background = element_rect(color = "black"))

ggsave("05_manu-figs/fig_map.png")
