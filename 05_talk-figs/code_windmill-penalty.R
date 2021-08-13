# Created:    aug 13 2021
#
# purpose: just showing obs gaps
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

myyieldlab <- (expression(atop("Continuous maize penalties", paste("(Mg "~ha^-1*")"))))



# gap components --------------------------------------------------------------------
#--I want gaps as %s

#--need sc yields at aonr
yld <- 
  read_csv("00_empirical-n-cont/dat_aonrs.csv") %>% 
  filter(aonr_rot == "aonr_sc") %>% 
  rename("nrate_kgha" = aonr_kgha) %>% 
  left_join(read_csv("00_empirical-n-cont/dat_preds.csv") %>% 
              filter(rotation == "sc"))

dat <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap)





# windmill %----------------------------------------------------------------


dat %>% 
  left_join(yld) %>% 
  mutate(gap_pct = nonngap/pred_yield) %>% 
  arrange(-gap_pct) %>% 
  mutate(id = 1:n(),
         gap_pct = ifelse(is.na(gap_pct), 0, gap_pct)) %>% 
  ggplot(aes(id, gap_pct)) + 
  geom_rect(xmin = 0, xmax = 109, 
            ymin = 0 , ymax = 1, fill = ylw1, alpha = 0.2) +
  geom_rect(xmin = 109, xmax = 121, 
            ymin = 0 , ymax = 1, fill = "white", alpha = 0.2) +
  geom_rect(xmin = 121, xmax = 147, 
            ymin = 0 , ymax = 1, fill = "gray80", alpha = 0.2) +
  geom_col(width = 1, fill = grn1, color = "black") +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_text(x = 134, y = .3, 
            label = "36 site-years,\nundetermined",
            hjust = 0.5, check_overlap = T, 
            fontface = "italic", color = "gray10", size = 5) +
  geom_text(x = 115, y = .55, 
            label = "12 site-years,\nno penalties",
            hjust = 0.5, check_overlap = T, 
            fontface = "italic", color = "gray10", size = 5) +
  geom_text(x = 55, y = .7, 
            label = "109 site-years,\npenalty",
            hjust = 0.5, check_overlap = T, 
            fontface = "italic", color = "gray10", size = 5) +
  geom_hline(yintercept = 0.1, color = rd2) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = c(0, 0.1, .25, .5, .75, 1),
                     labels = label_percent()) +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    axis.text.y = element_text(color = c("black", "red",
                                         "black","black",
                                         "black","black")),
    axis.title = element_text(size = rel(1.5)),
    axis.text = element_text(size = rel(1.4))) +
  scale_x_continuous(limits = c(0, 150)) +
  labs(fill = NULL,
       y = myyieldlab,
       x = "Site-year") 


ggsave("05_talk-figs/fig_windmill-horiz.png", width = 10.2, height = 4.7)


# windmill % simple----------------------------------------------------------------


dat %>% 
  left_join(yld) %>% 
  mutate(gap_pct = nonngap/pred_yield) %>% 
  arrange(-gap_pct) %>% 
  mutate(id = 1:n(),
         gap_pct = ifelse(is.na(gap_pct), 0, gap_pct)) %>% 
  ggplot(aes(id, gap_pct)) + 
  # geom_rect(xmin = 0, xmax = 109, 
  #           ymin = 0 , ymax = 1, fill = ylw1, alpha = 0.2) +
  # geom_rect(xmin = 109, xmax = 121, 
  #           ymin = 0 , ymax = 1, fill = "white", alpha = 0.2) +
  # geom_rect(xmin = 121, xmax = 147, 
  #           ymin = 0 , ymax = 1, fill = "gray80", alpha = 0.2) +
  geom_col(width = 1, fill = grn1, color = "black") +
  # geom_text(x = 134, y = .3, 
  #           label = "36 site-years,\nundetermined",
  #           hjust = 0.5, check_overlap = T, 
  #           fontface = "italic", color = "gray10", size = 5) +
  # geom_text(x = 115, y = .55, 
  #           label = "12 site-years,\nno penalties",
  #           hjust = 0.5, check_overlap = T, 
  #           fontface = "italic", color = "gray10", size = 5) +
  # geom_text(x = 55, y = .7, 
  #           label = "109 site-years,\npenalty",
  #           hjust = 0.5, check_overlap = T, 
  #           fontface = "italic", color = "gray10", size = 5) +
  # geom_hline(yintercept = 0.1, color = rd2) +
  scale_y_continuous(limits = c(0, 1), 
#                     breaks = c(0, 0.1, .25, .5, .75, 1),
                     labels = label_percent()) +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    # axis.text.y = element_text(color = c("black", "red",
    #                                      "black","black",
    #                                      "black","black")),
    axis.title = element_text(size = rel(1.5)),
    axis.text = element_text(size = rel(1.4))) +
  scale_x_continuous(limits = c(0, 150)) +
  labs(fill = NULL,
       y = myyieldlab,
       x = "Site-year") 


ggsave("05_talk-figs/fig_windmill-horiz-plain.png", width = 10.2, height = 4.7)






# windmill raw Mg ha-1----------------------------------------------------------------


dat %>%
  arrange(-nonngap) %>% 
  mutate(id = 1:n(),
         nonngap = ifelse(is.na(nonngap), 0, nonngap)) %>% 
  ggplot(aes(id, nonngap/1000)) + 
  geom_rect(xmin = 0, xmax = 109, 
            ymin = 0 , ymax = 5, fill = ylw1, alpha = 0.2) +
  geom_rect(xmin = 109, xmax = 121, 
            ymin = 0 , ymax = 5, fill = "white", alpha = 0.2) +
  geom_rect(xmin = 121, xmax = 147, 
            ymin = 0 , ymax = 5, fill = "gray80", alpha = 0.2) +
  geom_col(width = 1, fill = grn1, color = "black") +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_text(x = 134, y = 2, label = "36 site-years,\nundetermined",
            hjust = 0.5, check_overlap = T, fontface = "italic", color = "gray10") +
  geom_text(x = 115, y = 3, label = "12 site-years,\nno penalties",
            hjust = 0.5, check_overlap = T, fontface = "italic", color = "gray10") +
  geom_text(x = 55, y = 4, label = "109 site-years,\npenalty",
            hjust = 0.5, check_overlap = T, fontface = "italic", color = "gray10") +
  # expand_limits(y = 5) +
  # scale_y_continuous(breaks = c(-2, 0, 2, 4),
  #                    labels = c(2, 0, 2, 4)) +
   theme(
    #legend.justification = c(0,0),
    #   legend.position = c(0.05, 0.05),
    legend.position = "bottom",
    legend.background = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  labs(fill = NULL,
       y = myyieldlab,
       x = "Site-year") 






