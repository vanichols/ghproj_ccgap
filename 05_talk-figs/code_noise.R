# Created:      8/17/2021
#
# purpose: talk about noise
#
#
# notes: cis from iowa only


rm(list = ls())
library(saapsim) #--has some functions
library(tidysawyer2) #--has sawyer data
library(tidyverse)
library(janitor)


theme_set(theme_bw())
source("05_talk-figs/talk-palette2.R")


cis <- 
  read_csv("00_exp-variability/dat_gap-cis.csv") %>% 
  left_join(ilia_siteinfo)

se_ord <- 
  cis %>% 
  group_by(site) %>% 
  summarise(gap_se_mean = mean(gap_se, na.rm = T)) %>% 
  arrange(gap_se_mean) %>% 
  pull(site)
  

gap_lab <- (expression(atop("Continuous Corn Penalty", paste("(Mg "~ha^-1*")"))))

#--order sites by average se
cis %>% 
  arrange(site, -gap_kgha) %>% 
  group_by(site) %>% 
  mutate(n = 1:n(),
         site = factor(site, levels = se_ord)) %>% 
  arrange(site) %>% 
  mutate(manu_id = fct_inorder(manu_id)) %>% 
  ggplot(aes(n, gap_kgha/1000)) + 
  geom_linerange(aes(ymin = gap_lo/1000,
                     ymax = gap_hi/1000,
                     color = site),
                 size = 2) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  guides(color = F) +
  facet_grid(.~manu_id) +
  scale_color_manual(values = c(clr_blu, clr_cc, clr_div, clr_ltpur,
                                clr_or, clr_red, clr_rot)) +
  bigtheme + 
  labs(x = "Site-year, ordered from largest to smallest penalty",
       y = gap_lab) +
  theme(axis.text.x = element_blank(),
        )

ggsave("05_talk-figs/fig_noise.png", width = 10.6, height = 6.25)
