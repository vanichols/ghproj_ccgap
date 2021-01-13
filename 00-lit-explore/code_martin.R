library(tidyverse)
library(readxl)


dat <- read_excel("00-lit-explore/Martin1990_data.xlsx")

dat %>% 
  pivot_longer(corn:soybean) %>% 
  mutate(value2 = ifelse(grepl("length", response), 100-value, value),
         response2 = str_remove_all(response, "reduction") %>% str_trim(.)) %>% 
  filter(residue_trt == "raw") %>% 
  ggplot(aes(residue_trt, value2)) + 
  geom_col(aes(fill = name), position = position_dodge()) + 
  facet_grid(.~response2) + 
  scale_fill_manual(values = c("gold2", "green4")) + 
  labs(x = NULL,
       y = "Percent of water control (%)",
       title = "Martin et al. 1990")

ggsave("00-lit-explore/fig_martin1990-allelopathy.png")
