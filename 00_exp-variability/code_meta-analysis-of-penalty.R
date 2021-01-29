# Gina Nichols, Jan 29 2021
# Goal: Do a meta-analysis on data

library(tidyverse)
library(tidysawyer2) #--this is a private repo right now, sorry
library(grafify)
library(metafor)

theme_set(theme_bw())


#--write it to use in other places
datraw <- read_csv("00_exp-variability/dat_gap-cis.csv")

dat <- 
  datraw %>% 
  mutate(yi = gap_kgha,
         vi = gap_sd^2) %>% 
  select(yi, vi, site)


mod1 <- rma.mv(yi, vi, random = ~ 1 | site, data=dat)

summary(mod1)

ranef.rma.mv(mod1) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "site") %>% 
  as_tibble() %>% 
  ggplot(aes(reorder(site, -site.intrcpt), site.intrcpt)) + 
  geom_linerange(aes(ymin = site.pi.lb, 
                     ymax = site.pi.ub))



# map the sites -----------------------------------------------------------

library(maps)
library(ggrepel)
# this tibble includes all the states
myiamap <- as_tibble(map_data('state')) %>% 
  filter(region %in% c("iowa")) 

ia_dat <- 
  ia_siteinfo %>% 
  mutate(long = lon)

# for maps, we use geom_polygon and coord_quickmap
fig_map <- 
  ggplot() +
  geom_polygon(data = myiamap, 
               aes(x = long, y = lat, group = group), 
               color = "black", fill = "white") +
  geom_point(data = ia_dat, aes(x = long, y = lat), size = 5) +
  geom_label_repel(data = ia_dat, aes(x = long, y = lat, label = site_name, fill = site_name), size = 5) + 
  scale_fill_grafify() +
  guides(fill = F) +
  coord_quickmap()


# add map to random effects -----------------------------------------------

fig_ranef <- 
  ranef.rma.mv(mod1) %>%
  as.data.frame() %>%
  rownames_to_column(var = "site") %>%
  mutate(site = str_to_title(site)) %>%
  as_tibble() %>%
  ggplot(aes(reorder(site,-site.intrcpt), site.intrcpt)) +
  geom_linerange(aes(ymin = site.pi.lb,
                     ymax = site.pi.ub, 
                     color = site), size = 2) +
  geom_hline(yintercept = 0) +
  scale_color_grafify() +
  guides(color = F) +
  labs(x = NULL, y = "Random effect\n(deviation from grand mean)")

library(patchwork)

fig_ranef + fig_map  

ggsave("00_exp-variability/fig_ranef-site.png")

