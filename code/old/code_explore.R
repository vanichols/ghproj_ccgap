# jan 14 2020
# gina
# explore apsim sim results
# met file is 2008-2016


# libs --------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(lubridate)
library(janitor)


# functions ---------------------------------------------------------------

find_r2_fun <- function(myfit) {
  myl <- length(fitted(myfit)) -1
  sqt <- var( fitted(myfit) + resid(myfit) ) * myl
  r1 <- (sqt - deviance(myfit)) / sqt
  r1 <- round(r1, 4)
  return(r1)
}

source("code/gina_read-apsimout-fun.R")


# mitch data --------------------------------------------------------------

mitchraw <- 
  read_csv("../../1_Gina_Projects/proj_RotEffect/_data/tidy/td_ltn-data.csv") %>% 
  filter(site == "ames",
         year > 2007,
         year < 2017) %>% 
  spread(rotation, yield_kgha) %>% 
  filter(!is.na(sc)) %>% 
  filter(!is.na(cc)) %>% 
  rename("ccyield_kgha" = cc,
         "scyield_kgha" = sc) %>% 
  mutate(nrate_kgha = round(nrate_kgha, 0))

mitchraw

# read in out files created by apsim --------------------------------------

# keep only the files I want to look at
iwant <- c()

simraw <- ReadInApsimRes(doifilter = "N")   %>% 
  mutate(nrate_kgha = as.numeric(str_extract(file, "[[:digit:]]+")),
         simyield_buac = crop_yield,
         simyield_kgha = simyield_buac * 62.77 * (1-0.15)) %>% 
  select(year, simyield_kgha, nrate_kgha) %>% 
  arrange(year, nrate_kgha)

simraw %>% left_join(mitchraw) %>% 
  ggplot(aes(nrate_kgha, simyield_kgha)) + 
  geom_point() + 
  geom_point(aes(nrate_kgha, scyield_kgha), color = "red")


simfits <- 
  simraw %>% 
  mutate(site = "ames_sim",
         parm = "baseline") %>% 
  group_by(site, parm) %>%
  nest() %>%
  ## linear function
  mutate(linear = purrr::map(data, ~ try(lm(simyield_kgha ~  nrate_kgha,
                                            data =.))))%>%
  ## quadratic funtion
  mutate(quad = purrr::map(data, ~ try(lm(simyield_kgha ~  nrate_kgha + I(nrate_kgha^2),
                                          data =.))))%>%
  
  ## linear plateau function
  mutate(LP = purrr::map(data, ~ try(nls(simyield_kgha ~ a + b * (nrate_kgha - c) * (nrate_kgha <= c),
                                         start = list(a = 7000, b = 640, c = 200),
                                         data = .))))%>%
  ## quad plateau function
  mutate(QP = purrr::map(data, ~ try(nls(simyield_kgha ~ (a + b * nrate_kgha + c * I(nrate_kgha^2)) * 
                                           (nrate_kgha <= -0.5 * b/c) +
                                           (a + I(-b^2/(4 * c))) * (nrate_kgha > -0.5 * b/c),
                                         start = list(a =50, b = 1, c = -0.00024),
                                         data = .)))) %>%
  gather(linear:QP, key = model, value = fit) %>% 
  #--extract a parm (?intercept?)
  mutate(
    a = case_when(
      model == "linear" ~ purrr::map(fit, .f = ~ coef(.)[2]),
      model == "quad" ~ purrr::map(fit, .f = ~ coef(.)[1]),
      model == "LP" ~ purrr::map(fit, .f = ~ coef(.)[1]),
      model == "QP" ~ purrr::map(fit, .f = ~ coef(.)[1])
    )
  ) %>%
  #--extract b parm (slope, kind of?)
  mutate(
    b = case_when(
      model == "linear" ~ purrr::map(fit, .f = ~ coef(.)[1]),
      model == "quad" ~ purrr::map(fit, .f = ~ coef(.)[2]),
      model == "LP" ~ purrr::map(fit, .f = ~ coef(.)[2]),
      model == "QP" ~ purrr::map(fit, .f = ~ coef(.)[2])
    )
  ) %>%
  #--extract c parm (?)
  mutate(
    c = case_when(
      model == "linear" ~ purrr::map(fit, .f = ~ coef(.)[3]), #--this just produces an NA
      model == "quad" ~ purrr::map(fit, .f = ~ coef(.)[3]),
      model == "LP" ~ purrr::map(fit, .f = ~ coef(.)[3]),
      model == "QP" ~ purrr::map(fit, .f = ~ coef(.)[3])
      #TRUE ~ NA #--this doesn't work for some reason
    )
  ) %>%
  #--find R2
  mutate(r2 = fit %>% purrr::map(find_r2_fun)) %>%
  select(-data,-fit) %>%
  unnest(cols = c(a, b, c, r2))


