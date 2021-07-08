# Created:    april 17 2021
#
# purpose: comparing n- and non-n gaps
#
# notes: 
# last edited:   7/8/2021

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



# site and year var comps ---------------------------------------------------------------

dat <- 
  read_csv("02_variance-comps/dat_site-year-var-decomp.csv") %>% 
  filter(resp == "nonngap")

#--let's trust the spectr percent
dat %>% 
  summarise_if(is.numeric, sum)

dat_pie <- 
  dat %>%  
  group_by(resp) %>% 
  nest() %>% 
  mutate(data2 = data %>% purrr::map(. %>% 
                                       arrange(specr_pct) %>% 
                                       mutate(half = specr_pct/2,
                                              prev = lag(specr_pct),
                                              prev = ifelse(is.na(prev), 0, prev),
                                              cumprev = cumsum(prev),
                                              pos = half + cumprev) %>% 
                                       select(-c(half, prev, cumprev)))) %>% 
  unnest(data2)

dat_pie

#--fig
f_pie <- dat_pie %>% 
  mutate(pct_lab = round(specr_pct + 0.1, 0)/100) %>% #--it rounds to the nearest even number, WTF
  mutate(resp = ifelse(resp == "ngap", "Yield gap\nfrom N factors", "Observed yield penalty"),
         grp = case_when(
           grepl("Residual", grp) ~ "Site-Year",
           grepl("year", grp) ~ "Midwest-Year",
           grepl("site", grp) ~ "Site"),
         grp = factor(grp, levels = c("Site-Year", "Midwest-Year", "Site"))) %>% 
  ggplot(aes(x = "", y = specr_pct, fill = grp)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = pos,
                label = percent(pct_lab, accuracy = 1)), size = 5) +
  facet_wrap(~resp, ncol = 1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom",
        legend.direction = "vertical") +
  scale_fill_manual(values = c(ylw1, pnk1, grn1)) +
  labs(fill = NULL)


f_pie



# correlation -------------------------------------------------------------

dat_comps <- read_csv("00_empirical-n-cont/dat_gap-components.csv")


#f_corr <- 
dat_comps %>% 
  ggplot(aes(ngap/1000, nonngap/1000)) + 
  geom_hex(bins = 5, color = "black") +
  geom_point(color = "gray80", size = 3) +
  #  geom_abline() +
  labs(x = "Yield gap from N factors\n(Mg ha-1)",
       y = "Yield gap from other factors\n(Mg ha-1)", 
       fill = NULL) + 
  coord_cartesian(xlim = c(0, 5),
                  ylim = c(0, 5)) +
  theme(axis.title = element_text(size = rel(1.2)),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        # = "horizontal",
        legend.background = element_rect(color = "black")) + 
  scale_fill_viridis_c(option = "magma") + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) 

f_corr <- 
  dat_comps %>% 
  ggplot(aes(ngap/1000, nonngap/1000)) + 
  geom_point(color = dkbl1, size = 3) +
  geom_abline() +
  labs(x = "N-compensatable penalty\n(Mg ha-1)",
       y = "Observed penalty\n(Mg ha-1)", 
       fill = NULL) + 
  coord_cartesian(xlim = c(0, 5),
                  ylim = c(0, 5)) +
  theme(axis.title = element_text(size = rel(1.2)),
        legend.position = c(0.95, 0.95),
        legend.justification = c(1, 1),
        # = "horizontal",
        legend.background = element_rect(color = "black")) + 
  scale_fill_viridis_c(option = "magma") + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) 

f_corr
# together ----------------------------------------------------------------

library(cowplot)


plot_grid(f_pie, f_corr, rel_widths = c(1.2,2.5))

ggsave("05_manu-figs/fig_var-decomp-corr.png", width = 8.39, height = 5.19)



###### OLD ###########################

# site only ---------------------------------------------------------------


# variance components --------------------------------------------------------------------

dat <- 
  read_csv("02_variance-comps/dat_site-var-decomp.csv")

# pie chart ---------------------------------------------------------------

#--try using purrr

dat_pie <- 
  dat %>% 
  group_by(resp) %>% 
  nest() %>% 
  mutate(data2 = data %>% purrr::map(. %>% 
                                       arrange(specr_pct) %>% 
                                       mutate(half = specr_pct/2,
                                              prev = lag(specr_pct),
                                              prev = ifelse(is.na(prev), 0, prev),
                                              cumprev = cumsum(prev),
                                              pos = half + cumprev) %>% 
                                       select(-c(half, prev, cumprev)))) %>% 
  unnest(data2)

dat_pie

#--nice fig

f_pie <- 
  dat_pie %>%  
  mutate(resp = ifelse(resp == "ngap", "Yield gap\nfrom N factors", "Yield gap\nfrom other factors"),
         grp = ifelse(grp == "Residual", "Year", "Site"),
         grp = factor(grp, levels = c("Year", "Site"))) %>% 
  ggplot(aes(x = "", y = specr_pct, fill = grp)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = pos,
                label = percent(icc, accuracy = 2)), size = 5) +
  facet_wrap(~resp, ncol = 1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom") +
  scale_fill_manual(values = c(ylw1, grn1)) +
  labs(fill = NULL)


f_pie

