# Gina Nichols, Jan 18 2021
# Goal: explore variability in difference estimate
# Thanks Miranda!


library(tidyverse)
# library(tidysawyer2) #--this is a private repo right now, sorry

dat_miranda <- ia_yields_se %>%  
  group_by(site) %>% #--each site has it's own maximum nrate
  filter(nrate_kgha == max(nrate_kgha))

dat_miranda %>% 
  write_csv("00_exp-variability/dat_miranda.csv")

ylds <- 
  dat_miranda %>% 
  select(site, year, rotation, nrate_kgha, yield_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = yield_kgha) %>% 
  filter(!is.na(sc)) 
  
ylds

sds <- 
  dat_miranda %>% 
  select(site, year, rotation, nrate_kgha, sd_kgha) %>% 
  pivot_wider(names_from = rotation, values_from = sd_kgha) %>% 
  rename("cc_sd" = cc,
         "sc_sd" = sc) %>% 
  filter(!is.na(sc_sd))
sds

dat <- 
  ylds %>% 
  left_join(sds) %>% 
  mutate(
    gap_kgha = sc - cc
  )
dat    

#1. How to estimate the uncertainty around the DIFFERENCE of cc and sc
# NOTE: THIS IS WRONG
dat2 <-
  dat %>%
  mutate(
    term1 = (cc_sd) ^ 2 / 4,
    term2 = (sc_sd) ^ 2 / 4,
    gap_sd = sqrt(term1 + term2)
  )


dat2 %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_linerange(aes(ymin = gap_kgha - gap_sd,
                     ymax = gap_kgha + gap_sd,
                     color = site)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(nrate_kgha~site)

#--what I actually want is 95% confidence intervals around the difference value...


#2. What years were the differences 'real'? I can assess this visually or programmatically (does the 95 CI include 0). 

#3. Given the variation we see in measuring yields at this N rate (note the variation itself varies each year), can I figure out what the least significnat difference I am able to detect is? Can I get this estimate on a site-basis?

#4. Flip side, can I make a graph of replicates by smallest detectable difference for each site? To help each site decide if it's feasible, or to identify sites that might give more precise values. Or perhaps it's better to pool information across sites. 

sds %>% 
  pivot_longer(cc_sd:sc_sd) %>% 
  ggplot(aes(year, value)) + 
  geom_point() + 
  facet_grid(.~site, scales = "free")
