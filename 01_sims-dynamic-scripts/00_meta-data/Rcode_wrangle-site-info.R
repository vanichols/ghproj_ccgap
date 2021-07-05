# goal: create a tidy df with site info
# created: 10/8/20
# last modified:
#
# notes:

#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

library(tidysawyer2) #--has data in it
library(maps)
library(ggrepel)


siteinfo <- 
  saw_siteinfo %>% 
  bind_rows(il_siteinfo) %>% 
  select(site_name, site, lat, lon) %>% 
  mutate(site = case_when(
    grepl("DeKalb", site_name) ~ "deka",
    grepl("Monmouth", site_name) ~ "monm",
    grepl("Bottomland", site_name) ~ "dslo",
    TRUE ~ site)) %>% 
  filter(!site %in% c("UR", "OR", "BT", "DSU",
                      "lewi", "suth", "kana", "craw")) %>% 
  rename(long = lon) %>% 
  mutate(state = case_when(
    (site == "ames" | site == "mcna" | site == "nash") ~ "IA",
    (site == "deka" | site == "monm" | site == "dslo") ~ "IL"
  )) 
  
siteinfo %>% write_csv("dat_siteinfo.csv")



states <- map_data("state") %>% 
  filter(region %in% c("iowa", "illinois"))

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray80", color = "white") + 
  geom_point(data = siteinfo, aes(x = long, y = lat), size = 3) + 
  geom_text_repel(data = siteinfo, aes(x = long, y = lat, label = site_name)) +
  coord_cartesian() + 
  theme_bw()

ggsave("fig_siteinfo.png")


