# Created:       april 29 2021
# last edited:   
#
# purpose: see if 2009 means the same thing in each site
#
# notes: 


rm(list = ls())
library(tidysawyer2) 
library(tidyverse)
library(saapsim)

theme_set(theme_bw())

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


#--the years I have yield data for
mysiteyears <- 
  ilia_wea %>% 
  select(state, site, year) %>% 
  distinct()

mytheme <- theme(legend.position = "bottom",
                 axis.text.x = element_blank())


# full season precip-------------------------------------------------------------

ilia_wealt %>% 
  group_by(state, site, year) %>% 
  summarise(precip_tot = sum(precip_mm, na.rm = T)) %>% 
  ggplot(aes(year, precip_tot)) + 
  geom_line() + 
  facet_wrap(~site) + 
  labs(title = "Total precip")


ilia_wealt %>% 
  group_by(site, year) %>% 
  summarise(precip_tot = sum(precip_mm, na.rm = T)) %>%
  group_by(site) %>% 
  mutate(precip_sc = scale_this(precip_tot)) %>% 
  ggplot(aes(year, precip_sc)) + 
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~site) + 
  labs(title = "scaled precip")

#--scaled long-term precip
p_sc <- 
  ilia_wealt %>% 
  group_by(state, site, year) %>% 
  summarise(precip_tot = sum(precip_mm, na.rm = T)) %>%
  group_by(state, site) %>% 
  mutate(precip_sc = scale_this(precip_tot)) %>% 
  semi_join(mysiteyears) 

f_p <- 
  p_sc %>% 
    group_by(year) %>% 
    mutate(yearmn = mean(precip_sc)) %>% 
  ggplot(aes(site, precip_sc)) + 
  geom_point(aes(color = state), size = 4) + 
  geom_hline(yintercept = 0) +
    geom_hline(aes(yintercept = yearmn), linetype = "dashed") +
  facet_wrap(~year) +
  labs(x = "Site",
       y = "Yearly total precipitation, scaled",
       title = "Does 2006 mean the same thing at each site?",
       subtitle = "Yearly precip") + 
  scale_color_manual(values = c("purple", "darkorange")) +
  mytheme

f_p

# full season temp-------------------------------------------------------------

ilia_wealt %>% 
  mutate(avgt_c = (maxt_c + mint_c)/2) %>% 
  group_by(state, site, year) %>% 
  summarise(avgt_c = mean(avgt_c, na.rm = T)) %>% 
  ggplot(aes(year, avgt_c)) + 
  geom_line() + 
  facet_wrap(~site) + 
  labs(title = "Average temp")

ilia_wealt %>% 
  mutate(avgt_c = (maxt_c + mint_c)/2) %>% 
  group_by(state, site, year) %>% 
  summarise(avgt_c = mean(avgt_c, na.rm = T)) %>% 
  group_by(site) %>% 
  mutate(avgt_sc = scale_this(avgt_c)) %>% 
  ggplot(aes(year, avgt_sc)) + 
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~site) + 
  labs(title = "scaled avg temp")

#--scaled long-term precip
t_sc <- 
  ilia_wealt %>% 
  mutate(avgt_c = (maxt_c + mint_c)/2) %>% 
  group_by(state, site, year) %>% 
  summarise(avgt_c = mean(avgt_c, na.rm = T)) %>% 
  group_by(state, site) %>% 
  mutate(avgt_sc = scale_this(avgt_c))  %>% 
  semi_join(mysiteyears) 

f_t <- 
  t_sc %>% 
  group_by(year) %>% 
  mutate(yearmn = mean(avgt_sc)) %>% 
  ggplot(aes(site, avgt_sc)) + 
  geom_point(aes(color = state), size = 4) + 
  geom_hline(yintercept = 0) +
  geom_hline(aes(yintercept = yearmn), linetype = "dashed") +
  facet_wrap(~year) +
  labs(x = "Site",
       y = "Yearly average temperature, scaled",
       title = "Does 2006 mean the same thing at each site?",
       subtitle = "Yearly temperature") + 
  scale_color_manual(values = c("blue", "red")) +
  mytheme

f_t

# growing season precip-------------------------------------------------------------

gs_start <- saf_date_to_doy("2001-03-01")
gs_end <- saf_date_to_doy("2001-09-01")


#--scaled long-term precip
pgs_sc <- 
  ilia_wealt %>%
  filter(day > gs_start, day < gs_end) %>% 
  group_by(state, site, year) %>% 
  summarise(precip_tot = sum(precip_mm, na.rm = T)) %>%
  group_by(state, site) %>% 
  mutate(precip_sc = scale_this(precip_tot)) %>% 
  semi_join(mysiteyears) 

f_pgs <- 
  pgs_sc %>% 
  group_by(year) %>% 
  mutate(yearmn = mean(precip_sc)) %>% 
  ggplot(aes(site, precip_sc)) + 
  geom_point(aes(color = state), size = 4, pch = 18) + 
  geom_hline(yintercept = 0) +
  geom_hline(aes(yintercept = yearmn), linetype = "dashed") +
  facet_wrap(~year) +
  labs(x = "Site",
       y = "Growing season precipitation, scaled",
       title = "Does 2006 mean the same thing at each site?",
       subtitle = "Growing season precip") + 
  scale_color_manual(values = c("purple", "darkorange")) +
  mytheme


f_pgs

# gs season temp-------------------------------------------------------------

tgs_sc <- 
  ilia_wealt %>%
  filter(day > gs_start, day < gs_end) %>% 
  group_by(state, site, year) %>% 
  mutate(avgt_c = (maxt_c + mint_c)/2) %>% 
  group_by(state, site, year) %>% 
  summarise(avgt_c = mean(avgt_c, na.rm = T)) %>% 
  group_by(state, site) %>% 
  mutate(avgt_sc = scale_this(avgt_c)) %>% 
  semi_join(mysiteyears)

f_tgs <- 
  tgs_sc %>% 
  group_by(year) %>% 
  mutate(yearmn = mean(avgt_sc)) %>% 
  ggplot(aes(site, avgt_sc)) + 
  geom_point(aes(color = state), size = 4, pch = 18) + 
  geom_hline(yintercept = 0) +
  geom_hline(aes(yintercept = yearmn), linetype = "dashed") +
  facet_wrap(~year) +
  labs(x = "Site",
       y = "GS temperature, scaled",
       title = "Does 2006 mean the same thing at each site?",
       subtitle = "GS temperature") + 
  scale_color_manual(values = c("blue", "red")) +
  mytheme

f_tgs

# together ----------------------------------------------------------------

library(patchwork)

f_t + f_tgs
ggsave("00_exp-explore/fig_weather-year-temperature.png")

f_p + f_pgs
ggsave("00_exp-explore/fig_weather-year-precip.png")



# curiouis ----------------------------------------------------------------

gaps <- 
  read_csv("00_empirical-n-cont/dat_gap-components.csv") %>% 
  filter(!is.na(ngap_frac))

gaps %>% 
  left_join(pgs_sc) %>%
  select(site, year, nonngap, ngap, precip_sc) %>% 
  pivot_longer(nonngap:ngap) %>% 
  ggplot(aes(value, precip_sc)) + 
  geom_point(size = 5, aes(color = name)) + 
  geom_hline(yintercept = 0) +
  facet_wrap(~name)

gaps %>% 
  left_join(pgs_sc) %>%
  select(site, year, nonngap, ngap, precip_sc) %>% 
  pivot_longer(nonngap:ngap) %>% 
  ggplot(aes(value, precip_sc)) + 
  geom_hex(bins = 5, color = "black") +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_text(x = 4000, y = 2, label = "wet", check_overlap = T) +
  geom_text(x = 4000, y = -1.5, label = "dry", check_overlap = T) +
  facet_wrap(~name) + 
  scale_fill_viridis_c()


gaps %>% 
  left_join(tgs_sc) %>%
  select(site, year, nonngap, ngap, avgt_sc) %>% 
  pivot_longer(nonngap:ngap) %>% 
  ggplot(aes(value, avgt_sc)) + 
  geom_hex(bins = 5, color = "black") +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_text(x = 5000, y = 3, label = "hot", check_overlap = T) +
  geom_text(x = 5000, y = -1, label = "cool", check_overlap = T) +
  facet_wrap(~name) + 
  scale_fill_viridis_c() +
  facet_wrap(~name)

gaps %>% 
  ggplot(aes(ngap, nonngap)) + 
  geom_hex(bins = 5, color = "black") +
  geom_point() + 
  geom_abline() +
  scale_fill_viridis_c() + 
  coord_cartesian(ylim = c(0, 6000),
                  xlim = c(0, 6000))


#--%N vs tot gap colored by weather

gaps %>% 
  left_join(tgs_sc) %>% 
  left_join(pgs_sc) %>% 
  mutate(tot_gap = ngap + nonngap) %>% 
  ggplot(aes(tot_gap, ngap_frac)) + 
  geom_point(aes(color = precip_sc), size = 4) + 
  scale_color_viridis_c()

gaps %>% 
  left_join(tgs_sc) %>% 
  left_join(pgs_sc) %>% 
  mutate(tot_gap = ngap + nonngap) %>% 
  ggplot(aes(tot_gap, ngap_frac)) + 
  geom_point(aes(color = avgt_sc), size = 4) + 
  scale_color_gradient2(midpoint = 0)
    
    scale_colour_brewer(
      ...,
      type = "seq",
      palette = 1,
      direction = 1,
      aesthetics = "colour"
    )
  )
  scale_color_viridis_c()
