# Created:    april 17 2021
#
# purpose: comparing n- and non-n gaps
#
# notes: 
# last edited:   7/6/2021, 8/2/2021 (fixing subscripts)

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

dat <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  select(site, year, nonngap, ngap)

dat %>%
  arrange(nonngap, ngap) %>% 
  filter(!is.na(nonngap)) %>% 
  mutate(id = 1:n()) %>% 
  filter(nonngap > 0)



# vertical, pyramid--------------------------------------------------------------


fig_pyr <- 
  dat %>%
  arrange(nonngap, ngap) %>% 
  filter(is.na(nonngap)) %>% 
  bind_rows(
    dat %>%
      arrange(nonngap, ngap) %>% 
      filter(!is.na(nonngap))
  ) %>% 
  mutate(id = 1:n(),
         nonngap = ifelse(nonngap == 0 & ngap == 0, 0, nonngap),
         ngap = ifelse(is.na(nonngap), 0, ngap),
         nonngap = ifelse(is.na(nonngap), 0, nonngap),
         ngap = -ngap) %>% 
  pivot_longer(nonngap:ngap) %>%
  mutate(name = ifelse(name == "ngap", "Nitrogen-compensatable penalty", "Observed penalty")) %>% 
  ggplot(aes(id, value/1000)) + 
  geom_col(aes(fill = name), width = 1) +#, color = "black") + 
 # geom_vline(xintercept = 0, color = "gray70") +
#  geom_vline(xintercept = 36.5,  color = "gray70") +
#  geom_vline(xintercept = 42.5,  color = "gray70") +
#  geom_vline(xintercept = 48.5, color = "gray70") +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_text(x = 18, y = 1, label = "36 site-years, undetermined",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 40, y = 1, label = "6 site-years, no penalties",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 50, y = 1, label = "6 site-years, N-compensatable penalty",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  geom_text(x = 90, y = 1, label = "109 site-years, observed penalty",
            hjust = 0, check_overlap = T, fontface = "italic", color = "gray50") +
  scale_fill_manual(values = c(ylw1, grn1)) +
  expand_limits(y = 5) +
  scale_y_continuous(breaks = c(-2, 0, 2, 4),
                     labels = c(2, 0, 2, 4)) +
  theme(
    #legend.justification = c(0,0),
    #   legend.position = c(0.05, 0.05),
    legend.position = "bottom",
    legend.background = element_blank(),
    axis.title.y = element_text(angle = 90, vjust = 0.5)) +
  #  theme(legend.position = "top") +
  # guides(fill = F) +
  labs(fill = NULL,
       y = myyieldlab,
       x = "Site-year") +
  coord_flip()


fig_pyr


# gap type densities ------------------------------------------------------
#--histo

fig_histo <- 
  dat %>% 
  select(site, year, nonngap, ngap) %>% 
  pivot_longer(nonngap:ngap) %>% 
  mutate(name = ifelse(name == "ngap", "Nitrogen penalty", "Yield penalty")) %>% 
  ggplot(aes(value/1000)) + 
  geom_density(aes(fill = name), alpha = 0.6) + 
  scale_fill_manual(values = c(ylw1, grn1)) +
  labs(
    x = myyieldlab,
     y = "Data density",
       fill = NULL) +
  guides(fill = F) +
  theme(axis.text.y = element_blank(),
        #legend.justification = c(1, 1),
        #legend.position = c(0.9, 0.9),
        legend.background = element_blank(),
        legend.position = "bottom")

fig_histo

# combibne them -----------------------------------------------------------

#--horiz
fig_pyr + fig_histo + plot_layout(guides = "collect", widths = c(2.5, 1)) & theme(legend.position = 'top')

ggsave("05_manu-figs/fig_gap-types-histo.png", height = 4, width = 9.2)


