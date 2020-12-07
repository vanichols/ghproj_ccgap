# Created:       nov 20 2020
# last edited:   
#
# purpose: visualize new scripts
#
# notes: 


rm(list = ls())
library(tidyverse)

kill <- read_csv("../../../Box/Gina_APSIM_modeling/apsim/data_apsim-outs/01_create-parameter-grids/d01_param-grid-oat-kill.csv")



# visualize moisture-killing plants ---------------------------------------

killmois <- 
  kill %>% 
  filter(grepl("xMoistString_yMoistString|defaults", oat_id)) %>% 
  select(oat_desc, xMoistString, yMoistString) %>% 
  separate(xMoistString, into = c("x1", "x2", "x3", "x4", "x5"), sep = ",") %>% 
  separate(yMoistString, into = c("y1", "y2", "y3", "y4", "y5"), sep = ",") %>% 
  mutate(oat_desc = factor(oat_desc)) %>% 
  mutate_if(is.character, parse_number) 


fd_killmois <- 
  killmois %>% 
  pivot_longer(x1:x5, values_to = "xcoord") %>%
  mutate(name = parse_number(name),
         oat_desc = paste(oat_desc, name)) %>% 
  select(oat_desc, xcoord) %>%
  left_join(
    killmois %>% 
      pivot_longer(y1:y5, values_to = "ycoord")  %>%
      mutate(name = parse_number(name),
             oat_desc = paste(oat_desc, name)) %>%  
      select(oat_desc, ycoord)
    ) %>% 
  mutate(oat_desc = str_sub(oat_desc, 0, -3))


fd_killmois  %>%  
  ggplot(aes(xcoord, ycoord, group = oat_desc)) + 
#  geom_point() + 
  geom_line() +
  geom_line(data = fd_killmois %>% filter(grepl("defaults", oat_desc)), color = "red", size = 2) +
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  labs(x = "Average Soil Moisture (0 = LL, 1 = FC, 2 = SAT)",
       y = "Kill Fraction",
       title = "Plants killed due to moisture")

ggsave("00_exp-explore/fig_viz-KILL-mois.png", width = 4, height = 4)


# visualize temperature-killing plants ---------------------------------------

killtemp <- 
  kill %>% 
  filter(grepl("xTempString_yTempString|defaults", oat_id)) %>% 
  select(oat_desc, xTempString, yTempString) %>% 
  separate(xTempString, into = c("x1", "x2", "x3", "x4", "x5"), sep = ",") %>% 
  separate(yTempString, into = c("y1", "y2", "y3", "y4", "y5"), sep = ",") %>% 
  mutate(oat_desc = factor(oat_desc)) %>% 
  mutate_if(is.character, parse_number) 


fd_killtemp <- 
  killtemp %>% 
  pivot_longer(x1:x5, values_to = "xcoord") %>%
  mutate(name = parse_number(name),
         oat_desc = paste(oat_desc, name)) %>% 
  select(oat_desc, xcoord) %>%
  left_join(
    killtemp %>% 
      pivot_longer(y1:y5, values_to = "ycoord")  %>%
      mutate(name = parse_number(name),
             oat_desc = paste(oat_desc, name)) %>%  
      select(oat_desc, ycoord)
  ) %>% 
  mutate(oat_desc = str_sub(oat_desc, 0, -3))


fd_killtemp  %>%  
  ggplot(aes(xcoord, ycoord, group = oat_desc)) + 
  #  geom_point() + 
  geom_line() +
  geom_line(data = fd_killtemp %>% filter(grepl("defaults", oat_desc)), color = "red", size = 2) +
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  labs(x = "Average Soil Temp (degC)",
       y = "Kill Fraction",
       title = "Plants killed due to low temperature")

ggsave("00_exp-explore/fig_viz-KILL-temp.png", width = 4, height = 4)

