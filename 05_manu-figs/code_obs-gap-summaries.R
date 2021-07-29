# Created:    july 29 2021
#
# purpose: look at observed gaps
#
# notes: 
# last edited:   

rm(list = ls())
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(patchwork)
library(scales)
library(ggrepel)

theme_set(theme_bw())

source("05_manu-figs/palettes.R")




# gap  --------------------------------------------------------------------

dat <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap)



# look at gaps ------------------------------------------------------------

p_box <- 
  dat %>% 
  left_join(ilia_siteinfo) %>% 
  filter(!is.na(nonngap)) %>% 
  ggplot(aes(reorder(manu_id, -nonngap), nonngap)) + 
  geom_boxplot(aes(fill = lat)) + 
  scale_fill_gradient(low = dkbl1, 
                      high = ylw2) + 
  labs(#title = "Ames",
    x = NULL,
    fill = "Latitude\n(deg N)",
    y = (expression(atop("Continuous maize penalty", paste("(kg "~ha^-1*")"))))) 

p_box

# map ---------------------------------------------------------------------

iail <- 
  map_data("state") %>% 
  filter(region %in% c("iowa", "illinois"))

ilia_siteinfo

ilia_gap <- 
  dat %>% 
  filter(!is.na(nonngap)) %>% 
  group_by(site) %>% 
  summarise(mngap = mean(nonngap)) %>% 
  left_join(ilia_siteinfo) 

p_map <- 
  ggplot() + 
  geom_polygon(data = iail, 
               aes(x = long, y = lat, group = group), color = "white", fill = "gray80") +
  # geom_point(data = ilia_gap,
  #            aes(x = long, y = lat, fill = lat), pch = 21, fill = "black", size = 1) + 
  geom_text(data = ilia_gap %>% mutate(lat = ifelse(manu_id == "IL-7", lat - 0.3, lat)),
            aes(x = long + 0.25, y = lat, label = manu_id),
            #hjust = 0, 
            size = 3) + 
  scale_fill_gradient(low = dkbl1, 
                      high = ylw2) + 
  labs(x = NULL, y = NULL) + 
  coord_quickmap() +
  guides(fill = F) +
  ggpubr::theme_pubclean() +
  #coord_cartesian() + 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        #plot.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


p_map

# together ----------------------------------------------------------------

p_box + inset_element(p_map, 0.52, 0.52, 1, 1)

ggsave("05_manu-figs/fig_gap-smy-map.png", width = 9.45, height = 7.31)
