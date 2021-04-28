# Created:    april 2 2021
#
# purpose: make Penalty over time fig
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

theme_set(theme_bw())
source("05_manu-figs/palettes.R")


# what is driving Penalty --------------------------------------------------------------------

dat <- 
  tibble(scen = c(rep(c("Penalty driven by\ncontinuous maize yields", 
                            "Penalty driven by\nrotated maize yields", 
                            "Penalty driven by both"), each = 2)), 
       rot = c(rep(c("Rotated maize", "Continuous maize"), times = 3)),
       yield = c(10, 8, 12, 10, 11, 9)) %>% 
  mutate(scen = factor(scen, levels = c("Penalty driven by\nrotated maize yields",
                                        "Penalty driven by\ncontinuous maize yields",
                                        "Penalty driven by both")),
         rot = factor(rot, levels = c("Rotated maize", "Continuous maize")))

dat %>% 
  ggplot(aes(scen, yield)) + 
  geom_col(aes(fill = rot), position = position_dodge(width = 0.8)) + 
  geom_hline(yintercept = 10, color = "red", size = 7) + 
  geom_text(x = 0.7, y = 10, label = "Yield potential", check_overlap = T, fontface = "italic", color = "white") + 
  scale_fill_manual(values = c("Rotated maize" = dkbl1,
                               "Continuous maize" = ylw1)) + 
  labs(x = NULL,
       fill = NULL,
       y = "Maize grain yield (Mg ha-1)") + 
  theme(legend.position = "top")

ggsave("05_manu-figs/fig_conceptual-Penalty.png")

