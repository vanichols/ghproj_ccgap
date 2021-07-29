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
library(fancycut)
library(patchwork)
library(scales)
library(naniar)


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
    y = (expression(atop("Continuous maize penalty", paste("(kg "~ha^-1*")"))))) 


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
  geom_point(data = ilia_gap,
             aes(x = long, y = lat, size = mngap, fill = lat), pch = 21) + 
  geom_text(data = ilia_gap %>% mutate(lat = ifelse(manu_id == "IL-7", lat - 0.2, lat)),
            aes(x = long + 0.25, y = lat, label = manu_id),
            hjust = 0) + 
  scale_fill_gradient(low = dkbl1, 
                      high = ylw2) + 
  labs(x = "Longitude", 
       y = "Latitude",
       size = bquote("Mean Penalty (kg "~ha^-1*")")) + 
  coord_quickmap() +
  guides(fill = F) +
  #coord_cartesian() + 
  theme(legend.justification = c(0, 0),
        legend.position = c(0.1, 0.1),
        legend.background = element_rect(color = "black"),
        plot.background = element_blank())



# together ----------------------------------------------------------------
library(patchwork)

p_box + inset_element(p_map, 0.6, 0.6, 1, 1)

ggsave("05_manu-figs/fig_map.png")
