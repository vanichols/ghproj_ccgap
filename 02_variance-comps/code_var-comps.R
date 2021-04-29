# Created:       april 28 2021
#
# purpose: figure out variance components
#
# notes: 
# last edited:   


rm(list = ls())
library(tidysawyer2) 
library(tidyverse)
library(saapsim)
library(scales)
library(fancycut)
library(lme4)
library(lmerTest)
library(patchwork)
library(rptR)
library(specr)

theme_set(theme_bw())


# 9. site vs year variation --------------------------------------------------

dat_comps <- read_csv("00_empirical-n-cont/dat_gap-components.csv")
dat_nonn <- dat_comps %>% select(site, year, nonngap) %>% filter(!is.na(nonngap))
dat_n <- dat_comps %>% select(site, year, ngap) %>% filter(!is.na(ngap))



# use specr ---------------------------------------------------------------

#--nesting help
#https://biostatmatt.com/archives/2718
#wafer within lot
#lmer(Thickness ~ (1 | Lot/Wafer), data=Oxide)

#--normal anova, year is more important than site
m1 <- lm(nonngap ~ site + year, data = dat_nonn)
anova(m1)

#--jsut chekcing, order doesn't matter
lmer(nonngap ~ (1|site) + (1|year), data = dat_nonn)
lmer(nonngap ~ (1|year) + (1|site), data = dat_nonn)

#--depends what I think year is...
#--to me, 2009 doesn't mean the same thing in Ames as it does in Dixon Springs

# do with both year and site ----------------------------------------------

m1 <- lmer(nonngap ~ (1|year) + (1|site), data = dat_nonn)

nonn_varcomps <- 
  specr::icc_specs(m1) %>%
  mutate_if(is.numeric, round, 2) %>% 
  as_tibble() %>% 
  mutate(resp = "nonngap")

m2 <- lmer(ngap ~ (1|year) + (1|site), data = dat_n)

n_varcomps <- 
  specr::icc_specs(m2) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  as_tibble() %>% 
  mutate(resp = "ngap")


specr_comps <- bind_rows(nonn_varcomps, n_varcomps) %>% rename("specr_pct" = percent)


# use rptR ----------------------------------------------------------------

#https://cran.r-project.org/web/packages/rptR/vignettes/rptR.html

#--include both site and year, nonn
rep1 <- rpt(nonngap ~ (1 | site) + (1 | year), 
            grname = c("site", 
                       "year"), 
            data = dat_nonn, 
            datatype = "Gaussian", 
            #nboot = 1000, 
            nboot = 0,
            npermut = 0)

print(rep1)

nonn_var <- 
  tibble(resp = "nonngap",
         site = 0.143,
         year = 0.133)

#--include both site and year, n
rep2 <- rpt(ngap ~ (1 | site) + (1 | year), 
            grname = c("site", 
                       "year"), 
            data = dat_n, 
            datatype = "Gaussian", 
            nboot = 0, 
            #nboot = 1000, #--change if you want CIs
            npermut = 0)

print(rep2)

n_var <- 
  tibble(resp = "ngap",
         site = 0.111,
         year = 0.106)

rptr_comps <- 
  bind_rows(nonn_var, n_var) %>% 
  mutate(Residual = 1 - site - year) %>% 
  pivot_longer(site:Residual, names_to = "grp", values_to = "rptr_pct")



#  combine ----------------------------------------------------------------

specr_comps %>% 
  left_join(rptr_comps) %>% write_csv("02_variance-comps/dat_site-year-var-decomp.csv")


# they are identical, just use specr --------------------------------------



# do with just site ----------------------------------------------

m3 <- lmer(nonngap ~ (1|site), data = dat_nonn)

nonn_varcomps <- 
  specr::icc_specs(m3) %>%
  mutate_if(is.numeric, round, 2) %>% 
  as_tibble() %>% 
  mutate(resp = "nonngap")

m4 <- lmer(ngap ~ (1|site), data = dat_n)

n_varcomps <- 
  specr::icc_specs(m4) %>%
  mutate_if(is.numeric, round, 2)  %>% 
  as_tibble() %>% 
  mutate(resp = "ngap")


specr_comps2 <- bind_rows(nonn_varcomps, n_varcomps) %>% rename("specr_pct" = percent)

specr_comps2 %>% write_csv("02_variance-comps/dat_site-var-decomp.csv")
