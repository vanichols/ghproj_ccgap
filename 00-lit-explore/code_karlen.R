# goal: look at data from karlen et al 2014 
# https://link.springer.com/article/10.1007/s12155-014-9419-7
# created: 8/27/2021

rm(list = ls())

library(tidyverse)
library(readxl)

library(lme4)
library(lmerTest)
library(emmeans)

# data --------------------------------------------------------------------

datraw <- read_excel("00-lit-explore/karlan2014-table5.xlsx")

#--this is problematic bc long might be confounded with CC or CS. Should isolate it to one system. 
datraw %>% 
  mutate(yld_bump_mod = yld_modremoval - yld_noremoval,
         yld_bump_hi = yld_hiremoval - yld_noremoval,
         long = -longitude) %>% 
  select(long, yld_bump_mod, yld_bump_hi) %>% 
  pivot_longer(yld_bump_mod:yld_bump_hi) %>% 
  ggplot(aes(long, value, color = name)) + 
    geom_point(size = 5) + 
  scale_color_manual(values = c("darkred", "red")) + 
  facet_wrap(~name)
