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



# boxplor ------------------------------------------------------------

#--mean values
mn_box <- 
  dat %>% 
  left_join(ilia_siteinfo) %>% 
  filter(!is.na(nonngap)) %>% 
  mutate(nonngap = nonngap/1000) %>% # change to Mg ha
  group_by(manu_id) %>% 
  summarise(nonngap_lab = round(mean(nonngap, na.rm = T), 1)) %>% 
  left_join(dat %>% 
              left_join(ilia_siteinfo) %>% 
              filter(!is.na(nonngap)) %>% 
              group_by(site, manu_id) %>% 
              tally()
  )


p_box <- 
  dat %>% 
  left_join(ilia_siteinfo) %>% 
  filter(!is.na(nonngap)) %>% 
  mutate(nonngap = nonngap/1000) %>% # change to Mg ha
  ggplot(aes(reorder(manu_id, -nonngap), nonngap)) + 
  geom_boxplot(aes(fill = lat)) + 
  geom_text(data = mn_box, 
            aes(x = reorder(manu_id, -nonngap_lab), label = nonngap_lab), 
            y = -0.1, fontface = "italic") +
  scale_fill_gradient(low = dkbl1, 
                      high = ylw2) + 
  theme(
#    legend.position = c(0, 0),
    legend.direction = "horizontal",
    legend.position = "top",
   legend.justification = c(0, 0),
    legend.background = element_blank()
  ) +
  labs(#title = "Ames",
    x = NULL,
    fill = "Latitude (deg N)",
    y = (expression(atop("Continuous maize penalty", paste("(Mg "~ha^-1*")"))))) 

p_box

#--legend inside
p_box2 <- 
  dat %>% 
  left_join(ilia_siteinfo) %>% 
  filter(!is.na(nonngap)) %>% 
  mutate(nonngap = nonngap/1000) %>% # change to Mg ha
  ggplot(aes(reorder(manu_id, -nonngap), nonngap)) + 
  geom_boxplot(aes(fill = lat)) + 
  geom_text(data = mn_box, 
            aes(x = reorder(manu_id, -nonngap_lab), label = nonngap_lab), 
            y = -0.25, fontface = "italic") +
  geom_text(data = mn_box, 
            aes(x = reorder(manu_id, -nonngap_lab), label = paste0("n=",n)), 
            y = -0.6, fontface = "italic", color= "gray50") +
  scale_fill_gradient(low = dkbl1, 
                      high = ylw2) + 
  theme(
        legend.position = c(1, 1),
    #legend.direction = "horizontal",
    #legend.position = "top",
    legend.justification = c(1, 1),
    legend.background = element_blank()
  ) +
  scale_y_continuous(limits = c(-0.7, 5)) +
  labs(#title = "Ames",
    x = NULL,
    fill = "Latitude\n(deg N)",
    y = (expression(atop("Continuous maize penalty", paste("(Mg "~ha^-1*")"))))) 

p_box2

#--ordered by year
#p_box3 <- 
  dat %>% 
  left_join(ilia_siteinfo) %>% 
  filter(!is.na(nonngap)) %>% 
  mutate(nonngap = nonngap/1000,
         yearF = as.factor(year)) %>% # change to Mg ha
  ggplot(aes(reorder(yearF, -nonngap), nonngap)) + 
  geom_boxplot(fill = dkpnk1) + 
  theme(
    legend.position = c(1, 1),
    #legend.direction = "horizontal",
    #legend.position = "top",
    legend.justification = c(1, 1),
    legend.background = element_blank()
  ) +
  scale_y_continuous(limits = c(-0.5, 5)) +
  labs(#title = "Ames",
    x = NULL,
    fill = "Latitude\n(deg N)",
    y = (expression(atop("Continuous maize penalty", paste("(Mg "~ha^-1*")"))))) 


  ggsave("05_manu-figs/sfig_penalty-by-year.png")



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



# histogram ---------------------------------------------------------------

gaps <- 
  read_csv("00_empirical-n-cont/dat_aonrs.csv") %>% 
  filter(aonr_rot == "aonr_sc") %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  left_join(
    read_csv("00_empirical-n-cont/dat_preds.csv") %>% 
      filter(rotation == "sc")
  ) %>% 
  left_join(read_csv("00_empirical-n-cont/dat_gap-components.csv")) %>% 
  mutate(gap_pct = nonngap/pred_yield) %>% 
  filter(!is.na(nonngap)) %>% 
  select(site, year, nonngap, gap_pct)


p_hist <- 
  gaps %>% 
  mutate(gap_pct_cut = cut(gap_pct, 
                           labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
                                      "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                           breaks = c(seq(from = 0, to = 1, by = 0.1)), right = F, left = T)) %>% 
  group_by(gap_pct_cut) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(gap_pct_cut, n)) + 
  geom_col(fill = dkpnk1, color = "black") +
  labs(
    y = "Number of site-years",
    x = "Penalty as percentage of rotated-maize yield") 

  
p_hist

# together ----------------------------------------------------------------

#(p_box + inset_element(p_map, 0.52, 0.52, 1, 1)) / p_hist + plot_layout(heights = c(2, 1))

#(p_box + inset_element(p_map, 0.52, 0.52, 1, 1)) + p_hist + plot_layout(widths = c(2, 1.5))
#ggsave("05_manu-figs/fig_gap-smy-map.png", width = 9.45, height = 10.31)

(p_map + p_hist) /p_box2 + plot_layout(heights = c(1, 1.5))
#ggsave("05_manu-figs/fig_gap-smy-map.png")
ggsave("05_manu-figs/fig_gap-smy-map.png", width = 9.95, height = 7.31)
