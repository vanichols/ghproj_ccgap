# explore variability in difference estimate

library(tidyverse)
library(tidysawyer2)
library(nlraa)
library(mgcv)



# fixed effect model ------------------------------------------------------

###---example----

data(barley, package = "nlraa")

barley %>% as_tibble()

fm.L <- lm(yield ~ NF, data = barley)
fm.Q <- lm(yield ~ NF + I(NF^2), data = barley)
fm.A <- nls(yield ~ SSasymp(NF, Asym, R0, lrc), data = barley)
fm.LP <- nls(yield ~ SSlinp(NF, a, b, xs), data = barley)
fm.BL <- nls(yield ~ SSblin(NF, a, b, xs, c), data = barley)
fm.G <- gam(yield ~ NF + s(NF^2, k = 3), data = barley)

## Print the table with weights
IC_tab(fm.L, fm.Q, fm.A, fm.LP, fm.BL, fm.G)

## Each model prediction is weighted according to their AIC values
prd <- predict_nls(fm.L, fm.Q, fm.A, fm.LP, fm.BL, fm.G)

ggplot(data = barley, aes(x = NF, y = yield)) + 
  geom_point() + 
  geom_line(aes(y = fitted(fm.L), color = "Linear")) +
  geom_line(aes(y = fitted(fm.Q), color = "Quadratic")) +
  geom_line(aes(y = fitted(fm.A), color = "Asymptotic")) +  
  geom_line(aes(y = fitted(fm.LP), color = "Linear-plateau")) + 
  geom_line(aes(y = fitted(fm.BL), color = "Bi-linear")) + 
  geom_line(aes(y = fitted(fm.G), color = "GAM")) + 
  geom_line(aes(y = prd, color = "Avg. Model"), size = 1.2)


###---mydata----
d1 <- 
  ilia_yields %>% 
  filter(site == "brow", year == 1999)

do_nls <- function(fdat = d1){

fm.L <- lm(yield_kgha ~ nrate_kgha, data = fdat)
fm.Q <- lm(yield_kgha ~ nrate_kgha + I(nrate_kgha^2), data = fdat)
fm.A <- nls(yield_kgha ~ SSasymp(nrate_kgha, Asym, R0, lrc), data = fdat)
fm.LP <- nls(yield_kgha ~ SSlinp(nrate_kgha, a, b, xs), data = fdat)
fm.BL <- nls(yield_kgha ~ SSblin(nrate_kgha, a, b, xs, c), data = fdat)
fm.G <- gam(yield_kgha ~ nrate_kgha + s(nrate_kgha^2, k = 3), data = fdat)

## Print the table with weights
ftable <- 
  IC_tab(fm.L, fm.Q, fm.A, fm.LP, fm.BL, fm.G) %>% 
  as_tibble() 

return(ftable)
}

#m1 <- 
  ilia_yields %>% 
  filter(site == "ames") %>% 
  group_by(state, site, year, rotation) %>% 
  nest() %>% 
  mutate(thing1 = data %>% map(safely(~do_nls(fdat = .)))) %>% 
  unnest(thing1)

  
m1 <- 
  ilia_yields %>% 
    group_by(state, site, year, rotation) %>% 
    nest() %>% 
    mutate(thing1 = data %>% map(possibly(~donlraa(fdat = .), NULL))) %>% 
    unnest(thing1)
  
  #   map(possibly(aic_blin, NULL)),
  # filter(is_null == 0)

m1 %>% 
  filter(site == "ames") %>% 
  unite(site, year, col = "site_year") %>% 
  mutate(y = 1) %>% 
  filter(rotation == "cc") %>% 
  ggplot(aes(model, y)) +
  geom_tile(aes(fill = weight)) + 
  facet_wrap(~site_year, scales = "free") 


m2 <- 
  ilia_yields %>% 
  group_by(state, site, rotation) %>% 
  nest() %>% 
  mutate(thing1 = data %>% map(possibly(~do_nls(fdat = .), NULL))) %>% 
  unnest(thing1)

#   map(possibly(aic_blin, NULL)),
# filter(is_null == 0)

m2 %>% 
  mutate(y = "A") %>% 
  filter(rotation == "cc") %>% 
  ggplot(aes(model, y)) +
  geom_tile(aes(fill = weight)) + 
  facet_grid(site~., scales = "free") +
  scale_fill_viridis_c()


m2 %>% 
  mutate(y = "A") %>% 
  filter(rotation == "cc") %>% 
  ggplot(aes(y, model)) +
  geom_tile(aes(fill = weight)) + 
  facet_grid(.~site, scales = "free") +
  scale_fill_viridis_c()


# mixed models ------------------------------------------------------------
library(nlme)

###---example----

data(Orange)

## All models should be fitted using Maximum Likelihood
fm.L <- nlme(circumference ~ SSlogis(age, Asym, xmid, scal), 
             random = pdDiag(Asym + xmid + scal ~ 1), 
             method = "ML", data = Orange)
fm.G <- nlme(circumference ~ SSgompertz(age, Asym, b2, b3), 
             random = pdDiag(Asym + b2 + b3 ~ 1), 
             method = "ML", data = Orange)
fm.F <- nlme(circumference ~ SSfpl(age, A, B, xmid, scal), 
             random = pdDiag(A + B + xmid + scal ~ 1), 
             method = "ML", data = Orange)
fm.B <- nlme(circumference ~ SSbg4rp(age, w.max, lt.e, ldtm, ldtb), 
             random = pdDiag(w.max + lt.e + ldtm + ldtb ~ 1), 
             method = "ML", data = Orange)

## Print the table with weights
IC_tab(fm.L, fm.G, fm.F, fm.B)

## Each model prediction is weighted according to their AIC values
prd <- predict_nlme(fm.L, fm.G, fm.F, fm.B)

ggplot(data = Orange, aes(x = age, y = circumference)) + 
  geom_point() + 
  geom_line(aes(y = predict(fm.L, level = 0), color = "Logistic")) +
  geom_line(aes(y = predict(fm.G, level = 0), color = "Gompertz")) +
  geom_line(aes(y = predict(fm.F, level = 0), color = "4P-Logistic")) +  
  geom_line(aes(y = predict(fm.B, level = 0), color = "Beta")) +
  geom_line(aes(y = prd, color = "Avg. Model"), size = 1.2)

###---mydata----

dat <- 
  ilia_yields %>% 
  mutate(yearF = as.factor(year)) %>% 
  unite(site, year, col = "site_year")

datG <- groupedData(yield_kgha ~ nrate_kgha | site_year, data = dat)

fmm.L <- lmer(yield_kgha ~ nrate_kgha + (1|site_year), data = dat)
fmm.Q <- lmer(yield_kgha ~ nrate_kgha + I(nrate_kgha^2) + (1|site_year), data = dat)

#--this will need some work I think
fmm.LP <- nlme(yield_kgha ~ SSlinp(nrate_kgha, Asym, xmid, scal), 
             random = pdDiag(Asym + xmid + scal ~ 1),
             method = "ML", data = datG)

## Print the table with weights
IC_tab(fmm.L, fmm.Q, fmm.LP)

## Each model prediction is weighted according to their AIC values
prd <- predict_nlme(fm.L, fm.G, fm.F, fm.B)

# 
# fm.L <- nlme(yield_kgha ~ SSlinp(age, Asym, xmid, scal), 
#              random = pdDiag(Asym + xmid + scal ~ 1), 
#              method = "ML", data = Orange)
# fm.G <- nlme(circumference ~ SSgompertz(age, Asym, b2, b3), 
#              random = pdDiag(Asym + b2 + b3 ~ 1), 
#              method = "ML", data = Orange)
# fm.F <- nlme(circumference ~ SSfpl(age, A, B, xmid, scal), 
#              random = pdDiag(A + B + xmid + scal ~ 1), 
#              method = "ML", data = Orange)
# fm.B <- nlme(circumference ~ SSbg4rp(age, w.max, lt.e, ldtm, ldtb), 
#              random = pdDiag(w.max + lt.e + ldtm + ldtb ~ 1), 
#              method = "ML", data = Orange)
