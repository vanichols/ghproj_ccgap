# goal: look at sotiris sims to see if water is reset
# created: 5/18/2021
# updated: 
#
# notes: 

rm(list = ls())

library(tidyverse)
library(tidysawyer2)
library(saapsim)
library(apsimx)


#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())


# get soil profile --------------------------------------------------------
#--fail
extd.dir <- getwd()
inspect_apsim("ames-for-soil-prof.apsim", src.dir = extd.dir, 
              node = "Soil", soil.child = "Water") %>% 
  as_tibble()


depthcats <- 
  dat_cc2 %>% 
  select(depth_cm) %>% 
  distinct() %>% 
  mutate(
    n = 1:n(),
    dcat = paste0("d", n)) %>%
  select(-n) %>% 
  pivot_wider(names_from = dcat, values_from = depth_cm)


soi <- 
  readxl::read_excel("ames-soil-prof.xlsx") %>% 
  as_tibble() %>% 
  select(depth_cm, dul_mm) %>% 
  separate(depth_cm, into = c("di", "df"), sep = "-") %>% 
  mutate(davg = (as.numeric(di) + as.numeric(df))/2) 

dul <- 
  expand_grid(soi, depthcats) %>% 
  mutate(
    dcat = case_when(
      (davg <= d1+1) ~ d1,
      (davg >= d1+1 & davg <= d2) ~ d2,
      (davg >= d2 & davg <= d3) ~ d3,
      (davg >= d3 & davg <= d4) ~ d4,
      (davg >= d4 & davg <= d5) ~ d5,
      #davg >= depthcats[1] & davg < depthcats[2] ~ depthcats[2],
      TRUE ~ 999)
  ) %>% 
  filter(dcat != 999) %>% 
  group_by(dcat) %>% 
  summarise(dul_mm = mean(dul_mm)) %>% 
  rename("depth_cm" = dcat)



# read in sims ------------------------------------------------------------

helper_readrawoutfile <- function(path) {
  suppressMessages({
    myrawdatnames <- names(readr::read_table(path, skip = 2))
    myrawdat <- readr::read_table(path, skip = 4, na = "?", 
                                  col_names = myrawdatnames) %>% dplyr::mutate(Date = lubridate::dmy(Date), 
                                                                               doy = lubridate::yday(Date))
    return(myrawdat)
  })
}

dat_cc <- helper_readrawoutfile("forGinaExample/A_CC_203_daily.out") %>% 
  mutate(rot = "cc")
dat_cs <- helper_readrawoutfile("forGinaExample/A_CS_203_daily.out") %>% 
  mutate(rot = "cs")
dat_sc <- helper_readrawoutfile("forGinaExample/A_SC_203_daily.out") %>% 
  mutate(rot = "sc")

sw <- 
  dat_cc %>% 
  bind_rows(dat_cs) %>% 
  bind_rows(dat_sc) %>% 
  select(rot, Date, day, contains("sw")) %>% 
  pivot_longer(sw7cm:sw98cm) %>% 
  mutate(name = parse_number(name)) %>% 
  rename("depth_cm" = name)



# combine -----------------------------------------------------------------

dat <- 
  sw %>% 
  left_join(dul) %>% 
  mutate(year = lubridate::year(Date), 
         depth_cm = paste(depth_cm, "cm depth"),
         depth_cm = fct_inorder(depth_cm)) %>% 
  filter(year %in% c(ia_yields %>% filter(site == "ames") %>% pull(year) %>% unique())) 


# viz ---------------------------------------------------------------------


saf_date_to_doy("2001-04-15")
dat %>% 
  ggplot(aes(day, value, group = year)) + 
  geom_line(aes(color = as.factor(year))) + 
  geom_hline(aes(yintercept = dul_mm)) + 
  geom_vline(xintercept = 105) + 
  facet_grid(depth_cm ~ rot) + 
  guides(color = F) +
  labs(title = "Ames cont corn soil moisture 1999 - 2016",
       subtitle = "horizontal line = DUL",
       x = "day of year",
       y = "soil mois, mm/mm")


#--7 cm depth
dat %>% 
  filter(depth_cm == "7 cm depth",
         day < 105) %>% 
  ggplot(aes(day, value, group = year)) + 
  geom_line(aes(color = as.factor(year))) + 
  geom_hline(aes(yintercept = dul_mm)) + 
  geom_vline(xintercept = 105) + 
  facet_wrap(~rot) + 
  labs(title = "Ames soil moisture 1999 - 2016, 7 cm depth",
       subtitle = "horizontal line = DUL",
       x = "day of year",
       y = "soil mois, mm/mm")


#--38 cm depth
dat %>% 
  filter(depth_cm == "38 cm depth",
         day < 105) %>% 
  ggplot(aes(day, value, group = year)) + 
  geom_line(aes(color = as.factor(year))) + 
  geom_hline(aes(yintercept = dul_mm)) + 
  geom_vline(xintercept = 105) + 
  facet_wrap(~rot) + 
  labs(title = "Ames soil moisture 1999 - 2016, 38 cm depth",
       subtitle = "horizontal line = DUL",
       x = "day of year",
       y = "soil mois, mm/mm")

dat %>% 
  filter(#depth_cm == "38 cm depth",
         year >= 2011, 
         year <= 2014) %>% 
  ggplot(aes(Date, value)) + 
  geom_line() + 
  geom_hline(aes(yintercept = dul_mm)) + 
  facet_grid(depth_cm~rot) 

