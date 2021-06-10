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


#data(package = "tidysawyer2")

# helper functions --------------------------------------------------------

helper_readrawoutfile <- function(path) {
  suppressMessages({
    myrawdatnames <- names(readr::read_table(path, skip = 2))
    myrawdat <- readr::read_table(path, skip = 4, na = "?", 
                                  col_names = myrawdatnames) %>% dplyr::mutate(Date = lubridate::dmy(Date), 
                                                                               doy = lubridate::yday(Date))
    return(myrawdat)
  })
}

# get soil profile --------------------------------------------------------


# ames --------------------------------------------------------------------

#--simulation soil profile
ames_soi <- 
  readxl::read_excel("00_soil-water-reset-Qs/ames-soil-prof.xlsx") %>% 
  as_tibble() %>% 
  select(depth_cm, dul_mm) %>% 
  separate(depth_cm, into = c("di", "df"), sep = "-") %>% 
  mutate(davg = (as.numeric(di) + as.numeric(df))/2) 

#--results at certain depths
ames_cc <- 
  helper_readrawoutfile("00_soil-water-reset-Qs/sims/A_CC_203_daily.out") %>% 
  mutate(rot = "cc",
         site = "ames")

ames_cs <- helper_readrawoutfile("00_soil-water-reset-Qs/sims/A_CS_203_daily.out") %>% 
  mutate(rot = "cs",
         site = "ames")

ames_sc <- helper_readrawoutfile("00_soil-water-reset-Qs/sims/A_SC_203_daily.out") %>% 
  mutate(rot = "sc",
         site = "ames")

ames_sw <- 
  ames_cc %>% 
  bind_rows(ames_cs) %>% 
  bind_rows(ames_sc) %>% 
  select(rot, Date, day, contains("sw")) %>% 
  pivot_longer(4:ncol(.)) %>%  
  mutate(name = parse_number(name)) %>% 
  rename("depth_cm" = name)

#--reconcile depths

ames_depthcats <- 
  ames_sw %>% 
  select(depth_cm) %>% 
  distinct() %>% 
  arrange(depth_cm) %>% 
  mutate(
    n = 1:n(),
    dcat = paste0("d", n)) %>%
  select(-n) %>% 
  pivot_wider(names_from = dcat, values_from = depth_cm)


ames_dul <- 
  expand_grid(ames_soi, ames_depthcats) %>% 
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



#--combine

ames_dat <- 
  ames_sw %>% 
  left_join(ames_dul) %>% 
  mutate(year = lubridate::year(Date), 
         depth_cm = paste(depth_cm, "cm depth"),
         depth_cm = fct_inorder(depth_cm)) %>% 
  filter(year %in% c(ilia_yields %>% filter(site == "ames") %>% pull(year) %>% unique())) 

ames_dat %>% 
  mutate(site = "ames") %>% 
  write_csv("00_soil-water-reset-Qs/sw_ames.csv")



# deka --------------------------------------------------------------------

#--simulation soil profile
deka_soi <- 
  readxl::read_excel("00_soil-water-reset-Qs/deka-soil-prof.xlsx") %>% 
  as_tibble() %>% 
  select(depth_cm, dul_mm) %>% 
  separate(depth_cm, into = c("di", "df"), sep = "-") %>% 
  mutate(davg = (as.numeric(di) + as.numeric(df))/2) 

#--results at certain depths
deka_cc <- 
  helper_readrawoutfile("00_soil-water-reset-Qs/sims/Dekalb_CC_202.out") %>% 
  mutate(rot = "cc",
         site = "deka")

deka_cs <- helper_readrawoutfile("00_soil-water-reset-Qs/sims/Dekalb_CS_202.out") %>% 
  mutate(rot = "cs",
         site = "deka")

deka_sc <- helper_readrawoutfile("00_soil-water-reset-Qs/sims/Dekalb_SC_202.out") %>% 
  mutate(rot = "sc",
         site = "deka")

deka_sw <- 
  deka_cc %>% 
  bind_rows(deka_cs) %>% 
  bind_rows(deka_sc) %>% 
  select(rot, Date, day, contains("sw")) %>% 
  pivot_longer(4:ncol(.)) %>% 
  mutate(name = parse_number(name)) %>% 
  rename("depth_cm" = name)

#--reconcile depths

deka_depthcats <- 
  deka_sw %>% 
  select(depth_cm) %>% 
  distinct() %>% 
  arrange(depth_cm) %>% 
  mutate(
    n = 1:n(),
    dcat = paste0("d", n)) %>%
  select(-n) %>% 
  pivot_wider(names_from = dcat, values_from = depth_cm)


deka_dul <- 
  expand_grid(deka_soi, deka_depthcats) %>% 
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



#--combine

deka_dat <- 
  deka_sw %>% 
  left_join(deka_dul) %>% 
  mutate(year = lubridate::year(Date), 
         depth_cm = paste(depth_cm, "cm depth"),
         depth_cm = fct_inorder(depth_cm)) %>% 
  filter(year %in% c(ilia_yields %>% filter(site == "deka") %>% pull(year) %>% unique())) 

deka_dat %>% 
  mutate(site = "deka") %>% 
  write_csv("00_soil-water-reset-Qs/sw_deka.csv")



# monm --------------------------------------------------------------------

#--simulation soil profile
monm_soi <- 
  readxl::read_excel("00_soil-water-reset-Qs/monm-soil-prof.xlsx") %>% 
  as_tibble() %>% 
  select(depth_cm, dul_mm) %>% 
  separate(depth_cm, into = c("di", "df"), sep = "-") %>% 
  mutate(davg = (as.numeric(di) + as.numeric(df))/2) 

#--results at certain depths
monm_cc <- 
  helper_readrawoutfile("00_soil-water-reset-Qs/sims/Mon_CC_202.out") %>% 
  mutate(rot = "cc",
         site = "monm")

monm_cs <- helper_readrawoutfile("00_soil-water-reset-Qs/sims/Mon_CS_202.out") %>% 
  mutate(rot = "cs",
         site = "monm")

monm_sc <- helper_readrawoutfile("00_soil-water-reset-Qs/sims/Mon_SC_202.out") %>% 
  mutate(rot = "sc",
         site = "monm")

monm_sw <- 
  monm_cc %>% 
  bind_rows(monm_cs) %>% 
  bind_rows(monm_sc) %>% 
  select(rot, Date, day, contains("sw")) %>% 
  pivot_longer(4:ncol(.)) %>% 
  mutate(name = parse_number(name)) %>% 
  rename("depth_cm" = name)

#--reconcile depths

monm_depthcats <- 
  monm_sw %>% 
  select(depth_cm) %>% 
  distinct() %>% 
  arrange(depth_cm) %>% 
  mutate(
    n = 1:n(),
    dcat = paste0("d", n)) %>%
  select(-n) %>% 
  pivot_wider(names_from = dcat, values_from = depth_cm)


monm_dul <- 
  expand_grid(monm_soi, monm_depthcats) %>% 
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



#--combine

monm_dat <- 
  monm_sw %>% 
  left_join(monm_dul) %>% 
  mutate(year = lubridate::year(Date), 
         depth_cm = paste(depth_cm, "cm depth"),
         depth_cm = fct_inorder(depth_cm)) %>% 
  filter(year %in% c(ilia_yields %>% filter(site == "monm") %>% pull(year) %>% unique())) 

monm_dat %>% 
  mutate(site = "monm") %>% 
  write_csv("00_soil-water-reset-Qs/sw_monm.csv")

# lewi --------------------------------------------------------------------

#--simulation soil profile
lewi_soi <- 
  readxl::read_excel("00_soil-water-reset-Qs/lewi-soil-prof.xlsx") %>% 
  as_tibble() %>% 
  select(depth_cm, dul_mm) %>% 
  separate(depth_cm, into = c("di", "df"), sep = "-") %>% 
  mutate(davg = (as.numeric(di) + as.numeric(df))/2) 

#--results at certain depths
lewi_cc <- 
  helper_readrawoutfile("00_soil-water-reset-Qs/sims/L_CC_200.out") %>% 
  mutate(rot = "cc",
         site = "lewi")

lewi_cs <- helper_readrawoutfile("00_soil-water-reset-Qs/sims/L_CS_200.out") %>% 
  mutate(rot = "cs",
         site = "lewi")

lewi_sc <- helper_readrawoutfile("00_soil-water-reset-Qs/sims/L_SC_200.out") %>% 
  mutate(rot = "sc",
         site = "lewi")

lewi_sw <- 
  lewi_cc %>% 
  bind_rows(lewi_cs) %>% 
  bind_rows(lewi_sc) %>% 
  select(rot, Date, day, contains("sw")) %>% 
  pivot_longer(4:ncol(.)) %>% 
  mutate(name = parse_number(name)) %>% 
  rename("depth_cm" = name)

#--reconcile depths

lewi_depthcats <- 
  lewi_sw %>% 
  select(depth_cm) %>% 
  distinct() %>% 
  arrange(depth_cm) %>% 
  mutate(
    n = 1:n(),
    dcat = paste0("d", n)) %>%
  select(-n) %>% 
  pivot_wider(names_from = dcat, values_from = depth_cm)


lewi_dul <- 
  expand_grid(lewi_soi, lewi_depthcats) %>% 
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



#--combine

lewi_dat <- 
  lewi_sw %>% 
  left_join(lewi_dul) %>% 
  mutate(year = lubridate::year(Date), 
         depth_cm = paste(depth_cm, "cm depth"),
         depth_cm = fct_inorder(depth_cm)) %>% 
  filter(year %in% c(ilia_yields %>% filter(site == "lewi") %>% pull(year) %>% unique())) 

lewi_dat %>% 
  mutate(site = "lewi") %>% 
  write_csv("00_soil-water-reset-Qs/sw_lewi.csv")


