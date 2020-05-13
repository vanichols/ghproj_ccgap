# Created:       4/30/2020
# last edited:   
# 
# purpose: explore soils data
#


rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2", force = T)
library(tidysawyer2) #--saw_xx data
library(lme4)

soi <- read_csv("01_create-features/cf_pred-soil.csv")


# look at things ----------------------------------------------------------

soi %>% 
  ggplot(aes(iacsr, soc_30cm_pct)) + 
  geom_point( size = 3) + 
  geom_smooth(method = "lm") + 
  labs(x = "Iowa Corn Suitability Rating",
       y = "SOC in top 30 cm (%)")
