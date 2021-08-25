# Created:    aug 13 2021
#
# purpose: just showing obs gaps
#
# notes: 
# last edited:   aug 15 changing colors

rm(list = ls())
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(fancycut)
library(patchwork)
library(scales)
library(naniar)


source("05_talk-figs/talk-palette3.R")

raw_lab <- (expression(atop("Continuous corn penalty", paste("(Mg"~ha^-1*")"))))
pct_lab <- (expression(atop("Continuous corn penalty", paste("(%)"))))

theme_set(theme_bw())

wind_theme <- 
  theme(
  legend.position = "bottom",
  legend.background = element_blank(),
  axis.title.y = element_text(angle = 0, vjust = 0.5),
  axis.text.y = element_text(color = c("black", clr_blu,
                                       "black","black",
                                       "black","black")),
  axis.title = element_text(size = rel(1.5)),
  axis.text = element_text(size = rel(1.4)))



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
  geom_rect(xmin = 1, xmax = 109, 
            ymin = 0 , ymax = 1, fill = "white", alpha = 0.2, color = "black") +
  geom_rect(xmin = 109, xmax = 121, 
            ymin = 0 , ymax = 1, fill = "gray80", color = "black") +
  # geom_rect(xmin = 121, xmax = 147, 
  #           ymin = 0 , ymax = 1, fill = "gray80", alpha = 0.2) +
  geom_col(width = 1, fill = clr_red, color = "black") +
  geom_hline(yintercept = 0, color = "gray70") +
  # geom_text(x = 55, y = .7, 
  #           label = "109 site-years,\npenalty",
  #           hjust = 0.5, check_overlap = T, 
  #           fontface = "italic", color = "gray10", size = 5) +
  # geom_text(x = 115, y = .7, 
  #           label = "12 site-years,\nno penalties",
  #           hjust = 0.5, check_overlap = T, 
  #           fontface = "italic", color = "gray10", size = 5) +
  geom_segment(aes(x = 0, xend = 121,
                   y = 0.1, yend = 0.1), color = clr_blu, size = 3) +
  scale_y_continuous(limits = c(0, 1), 
                     breaks = c(0, 0.1, .25, .5, .75, 1),
                     labels = label_percent()) +
  scale_x_continuous(limits = c(0, 121)) +
  wind_theme +
  labs(fill = NULL,
       y = pct_lab,
       x = "Site year",
       caption = "Penalty (109);\n No penalty (12);\nIn-estimable (36)") 


ggsave("05_talk-figs/fig_windmill-horiz-pct.png", width = 7.26, height = 4.7)



# windmill raw Mg ha-1 no mean line----------------------------------------------------------------


dat %>%
  filter(!is.na(nonngap)) %>% 
  arrange(-nonngap) %>% 
  mutate(id = 1:n(),
         nonngap = ifelse(nonngap == 0, 10, nonngap)) %>% 
  ggplot(aes(id, nonngap/1000)) + 
  geom_col(width = 1, fill = clr_red, color = "black") +
   scale_x_continuous(limits = c(0, 121), ) +
  labs(fill = NULL,
       y = "Continuous\nCorn\nPenalty",
       x = "Site-year",
       caption = "Penalty (109);\n No penalty (12);\nIn-estimable (36)") +
  wind_theme +
  theme(axis.text.y = element_text(color = "black"))

ggsave("05_talk-figs/fig_horiz-raw1.png", width = 10.2, height = 4.7)

# windmill raw Mg ha-1----------------------------------------------------------------


dat %>%
  filter(!is.na(nonngap)) %>% 
  arrange(-nonngap) %>% 
  mutate(id = 1:n(),
         nonngap = ifelse(nonngap == 0, 10, nonngap)) %>% 
  ggplot(aes(id, nonngap/1000)) + 
  # geom_rect(xmin = 1, xmax = 109, 
  #           ymin = 0 , ymax = 5, fill = "white", color = "black") +
  # geom_rect(xmin = 109, xmax = 121, 
  #           ymin = 0 , ymax = 5, fill = "gray80", color= "black") +
  # geom_rect(xmin = 121, xmax = 147, 
  #           ymin = 0 , ymax = 5, fill = "gray80", alpha = 0.2) +
  geom_col(width = 1, fill = clr_red, color = "black") +
  geom_segment(aes(x = 0, xend = 121,
                   y = 1, yend = 1), color = clr_blu, size = 3) +
  geom_hline(yintercept = 0, color = "gray70")  +
  scale_x_continuous(limits = c(0, 121), ) +
  labs(fill = NULL,
       y = "Continuous\nCorn\nPenalty",
       x = "Site-year",
       caption = "Penalty (109);\n No penalty (12);\nIn-estimable (36)") +
  wind_theme 

ggsave("05_talk-figs/fig_horiz-raw2.png", width = 10.2, height = 4.7)


# -------------------------------------------------------------------------

dat %>% 
  left_join(yld) %>% 
  mutate(gap_pct = nonngap/pred_yield) %>% 
  arrange(-gap_pct) %>% 
  ggplot(aes(pred_yield/1000, gap_pct)) + 
  geom_point(fill = clr_red, size = 4, pch= 21) + 
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  labs(x = raw_lab,
       y = pct_lab) + 
  wind_theme + 
  theme(axis.text.y = element_text(color= "black"))

ggsave("05_talk-figs/fig_raw-vs-pct.png")  




