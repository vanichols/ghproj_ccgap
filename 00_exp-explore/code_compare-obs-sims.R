# Created:       nov 16 2020
# last edited:   
#
# purpose: compare obs and simulated data
#
# notes: 


rm(list = ls())
#devtools::install_github("wilkelab/ungeviz")
library(tidysawyer2) 
library(tidyverse)
library(grafify)
library(ungeviz)



# all nrates --------------------------------------------------------------

obssim <- 
  ia_yields %>% 
  mutate(rotation2 = ifelse(rotation == "cs", "sc", rotation),
         datatype = "obs") %>% 
  bind_rows(
    saw_tidyapsim %>% 
      mutate(datatype = "sim") 
  )

obssim %>% 
  ggplot(aes(nrate_kgha, yield_kgha)) + 
  geom_point(aes(color = rotation2)) + 
  facet_grid(rotation2~datatype)


do_asymp <- function(mydata, myrot, mydt) {
  
  dtmp <- mydata %>% 
    filter(rotation2 == myrot,
           datatype == mydt)
  
  fmtmp <- nls(
    yield_kgha ~ SSasymp(nrate_kgha, Asym, R0, lrc),
    data = dtmp)
  
  dtmp$preds <- predict(fmtmp)
  
  return(dtmp)
  
}
  
obs_cc <- do_asymp(mydata = obssim, myrot = "cc", mydt = "obs")
obs_sc <- do_asymp(mydata = obssim, myrot = "sc", mydt = "obs")
sim_cc <- do_asymp(mydata = obssim, myrot = "cc", mydt = "sim")
sim_sc <- do_asymp(mydata = obssim, myrot = "sc", mydt = "sim")


obssim_preds <- 
  obs_cc %>% 
  bind_rows(obs_sc) %>% 
  bind_rows(sim_cc) %>% 
  bind_rows(sim_sc) 


obssim_preds %>% 
  ggplot(aes(nrate_kgha, preds)) +
  geom_point(aes(x = nrate_kgha, y = yield_kgha, color = rotation2), alpha = 0.1) +
  geom_line(aes(color = rotation2), size = 2) + 
  scale_color_grafify() +
  labs(y = "Corn yield (kg/ha)",
       x = "Nitrogen rate (kg/ha)") +
  facet_grid(.~datatype) + 
  theme_bw() + 
  theme(strip.text = element_text(size = rel(1.4)),
        legend.text = element_text(size = rel(1.4)),
        axis.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.2)))                                
  
ggsave("00_exp-explore/fig_obs-vs-sim-allnates.png")



# at max n rate -----------------------------------------------------------

obs_ylds <- 
  ilia_yields %>% 
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rot2 = ifelse(rotation == "cs", "sc", rotation),
         datatype = "obs")

sim_ylds <- 
  saw_tidyapsim %>%
  mutate(state = "IA") %>% 
  group_by(state, site) %>% 
  filter(nrate_kgha == max(nrate_kgha)) %>% 
  mutate(rot2 = ifelse(rotation == "cs", "sc", rotation),
         datatype = "sim")

ylds <- 
  obs_ylds %>% 
  bind_rows(sim_ylds) %>% 
  mutate(datatype = factor(datatype, levels = c("sim", "obs")),
         rot2 = factor(rot2, levels = c("sc", "cc"))) %>% 
  group_by(state, rot2, datatype) %>% 
  mutate(yield_mean = mean(yield_kgha, na.rm = T))
  
ylds_mn <- 
  ylds %>% 
  select(rot2, yield_mean) %>% 
  distinct() %>% 
  pivot_wider(names_from = rot2, values_from = yield_mean)


# plots -------------------------------------------------------------------


ylds %>%
  ggplot(aes(rot2, yield_kgha)) +
  geom_violin(aes(fill = state)) +
  geom_hpline(width = 0.05, size = 0.5, color = "gray20") +
  geom_hpline(aes(rot2, yield_mean), width = 0.2, size = 3, color = "red3") +
  geom_hpline(aes(rot2, yield_mean), width = 0.2, size = 3, color = "red3") +
  geom_segment(data = ylds_mn, x = 1.5, xend = 1.5, 
               aes(y = sc, yend = cc), arrow = arrow(length = unit(0.2, "cm")),
               size = 2) +
  guides(fill = F) +
  scale_fill_grafify() +
  labs(y = "Corn Yields (kg/ha)",
       x = NULL,
       title = "Continuous Corn Penalty\nat High (>250 kg/ha) Nitrogen Rates") +
  facet_grid(state~datatype) + 
  theme_bw()

ggsave("00_exp-explore/fig_obs-vs-sim-highn.png", height = 10, width = 7)


#--
obs_ylds %>%
  select(state, site, year, rot2, yield_kgha) %>% 
  pivot_wider(names_from = rot2, values_from = yield_kgha) %>% 
  mutate(
    env_ind = (cc + sc)/2,
    gap_kgha = sc-cc) %>% 
  ggplot(aes(env_ind, gap_kgha)) + 
  geom_point(aes(color = state))
  
