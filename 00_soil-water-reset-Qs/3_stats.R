# goal: is cc wetter than sc?
# created: 6/13/2021
# updated: 
#
# notes: 

rm(list = ls())

library(tidyverse)
library(tidysawyer2)
library(saapsim)
library(apsimx)

dat <- read_csv("00_soil-water-reset-Qs/sw_all.csv") %>% select(-Date, -dul_mm)



swtop <- 
  dat %>% 
  group_by(site) %>% 
  filter(depth_cm == min(depth_cm)) %>% 
  #--combine sc and sc
  mutate(rot = ifelse(rot == "cs", "sc", rot)) %>% 
  # make new factor variables and convert old trt/block variables into factors
  mutate(
    id = as.factor(paste(rot, year, site, sep = "_")),
    rot = as.factor(rot),
    year = as.factor(year),
    site = as.factor(site))


swtop %>% 
  filter(rot == "sc", day == 1)


ames <- 
  swtop %>% filter(site == "ames")

deka <- 
  swtop %>% filter(site == "deka")

lewi <- 
  swtop %>% filter(site == "lewi")

monm <- 
  swtop %>% filter(site == "monm")


# fit models --------------------------------------------------------------

mod_v <- mgcv::gam(value ~ s(day, by = rot, bs = "cr", k = 35) + rot,
                    data = ames, 
                    method = "REML")

mod_v_prd <- predict(mod_v, se.fit = TRUE)
ames_prd <- cbind(ames, Estimate = mod_v_prd$fit, 
                    Q2.5 = mod_v_prd$fit - 1.96 * mod_v_prd$se.fit,
                    Q97.5 = mod_v_prd$fit + 1.96 * mod_v_prd$se.fit)

## GAM fit with 95% confidence bands
ggplot(data = ames_prd, 
       aes(x = day, y = value, color = rot)) + 
  #geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot, color = NULL), alpha = 0.3)

## How does the model change when we incorporate the random effect of block?
mod_v2 <- mgcv::gam(value ~ s(day, by = rot, bs = "cr", k = 35) + rot + s(year, bs = "re"),
                    data = ames, method = "REML")

mod_v2_prd <- predict(mod_v2, se.fit = TRUE, exclude = "s(year)")
ames2 <- cbind(ames, Estimate = mod_v2_prd$fit, 
                     Q2.5 = mod_v2_prd$fit - 1.96 * mod_v2_prd$se.fit,
                     Q97.5 = mod_v2_prd$fit + 1.96 * mod_v2_prd$se.fit)

ggplot(data = ames2, aes(x = day, y = value, color = rot)) + 
  #geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot, color = NULL), alpha = 0.3)


#--make a function to do to each site

fit_gam <- function(mydata = mydata){
  
  mod_fixed <- mgcv::gam(value ~ s(day, by = rot, bs = "cr", k = 35) + rot,
                     data = mydata, 
                     method = "REML")
  
  mod_fixed_prd <- predict(mod_fixed, se.fit = TRUE)
  
  data_fixed <- 
    cbind(mydata, 
          est = mod_fixed_prd$fit, 
          cilo = mod_fixed_prd$fit - 1.96 * mod_fixed_prd$se.fit,
          cihi = mod_fixed_prd$fit + 1.96 * mod_fixed_prd$se.fit) %>% 
    mutate(modtype = "fixed")
          
    
  mod_ran <- mgcv::gam(value ~ s(day, by = rot, bs = "cr", k = 35) + rot + s(year, bs = "re"),
                      data = mydata, method = "REML")
  
  mod_ran_prd <- predict(mod_ran, se.fit = TRUE, exclude = "s(year)")
  
  data_ran <- 
    cbind(mydata, 
          est = mod_ran_prd$fit, 
          cilo = mod_ran_prd$fit - 1.96 * mod_ran_prd$se.fit,
          cihi = mod_ran_prd$fit + 1.96 * mod_ran_prd$se.fit) %>% 
    mutate(modtype = "ran")
  
  
  data_prd <- bind_rows(data_fixed, data_ran)
  
  return(data_prd)
  
  
}


mod_ames <- fit_gam(ames)

mod_lewi <- fit_gam(lewi)

mod_monm <- fit_gam(monm)

mod_deka <- fit_gam(deka)


mod_all <- 
  mod_ames %>% 
  bind_rows(mod_lewi) %>% 
  bind_rows(mod_monm) %>% 
  bind_rows(mod_deka)

ggplot(data = mod_all, aes(x = day, y = value, color = rot)) + 
  geom_line(aes(y = est)) + 
  geom_ribbon(aes(ymin = cilo, ymax = cihi, fill = rot, color = NULL), alpha = 0.3) + 
  facet_grid(modtype~site)

ggplot(data = mod_all %>% filter(modtype == "ran"),
       aes(x = day, y = value, color = rot)) + 
  geom_line(aes(y = est)) + 
  geom_ribbon(aes(ymin = cilo, ymax = cihi, fill = rot, color = NULL), alpha = 0.3) + 
  facet_grid(modtype~site)+ 
  labs(title = "Soil water at 7cm",
       subtitle = "GAM fit with year as random effect",
       x = NULL,
       y = "soil volumetric water")

ggsave("00_soil-water-reset-Qs/fig_sw7cm.png")




