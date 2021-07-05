# goal: create a tidy df with yield data
# created: 10/8/20
# last modified:
#
# notes:

#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

library(tidyverse)
library(tidysawyer2) #--has data in it
library(patchwork)


siteinfo <- read_csv("dat_siteinfo.csv")
summary(factor(saw_tidysawyer$site))

#--make IL sites into 4 letter codes
il_yields2 <- 
  il_yields %>% 
  mutate(site = case_when(
    grepl("DK", site) ~ "deka",
    grepl("MN", site) ~ "monm",
    grepl("DSB", site) ~ "dslo")) %>% 
  filter(!is.na(site)) 
  

#--combine IL and IA yields
yields <- 
  siteinfo %>% 
  select(site, state) %>% 
  left_join(il_yields2 %>% 
              bind_rows(saw_tidysawyer)) %>% 
  select(-sd_kgha, -nreps)

#--yeilds at max N
yields2 <-
  yields %>% 
  group_by(site, year, rotation) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  rename(nrate_max_kgha = "nrate_kgha",
         obs_yield_kgha = "yield_kgha")


yields2 %>% write_csv("dat_obsyields.csv")



# figs --------------------------------------------------------------------

fig_ncurve <- 
  yields %>% 
  ggplot(aes(nrate_kgha, yield_kgha, group = interaction(site, year, rotation))) + 
  geom_line(aes(color = rotation), size = 1.2) + 
  facet_wrap(~state, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("sc" = "green4",
                                "cc" = "orange2")) +
  labs(x = "N rate (kg/ha)",
       y = "Corn Yield (kg/ha)",
       title = "Corn Yield Response to Nitrogen") +
  theme_bw()+ 
  theme(legend.position = "top")


fig_maxn <- 
  yields2 %>% 
  group_by(site, rotation) %>% 
  ggplot(aes(site, obs_yield_kgha, color = rotation)) + 
  stat_summary(size = 2, position=position_dodge(width=0.2)) + 
  scale_color_manual(values = c("sc" = "green4",
                                "cc" = "orange2")) +
  facet_wrap(~state, ncol = 1, scales = "free_x") +
  #guides(color = F) +
  theme_bw() + 
  theme(legend.position = "top") +
  labs(x = NULL,
       y = "Corn Yields (kg/ha)",
       title = "Corn Yields at Maximum N Rate")


fig_ncurve | fig_maxn

ggsave("fig_obsyields.png")
