
library(tidyverse)

dat %>%
  group_by(site, year, rotation) %>%
  nest() %>%
  mutate(qp = purrr::map(data, ~try(nls(yield ~ (a + b * rate + c * I(rate^2)) *
                                        (rate <= -0.5 * b/c) +
                                        (a + I(-b^2/(4 * c))) * (rate > -0.5 * b/c),
                                      start = list(a = as.data.frame(.)[1,2],
                                                   b = as.data.frame(.)[1,3],
                                                   c = as.data.frame(.)[1,4]),
                                      control = list(maxiter = 1000),
                                      data = .)))) %>%
  mutate(newfit = as.character(qp),
         newfit = str_sub(newfit, 1, 3)) %>%
  filter(newfit != "Err") %>%
  mutate(a_coef = sapply(qp, FUN = function(qp){a = coef(qp)[1]}),
         b_coef = sapply(qp, FUN = function(qp){b = coef(qp)[2]}),
         c_coef = sapply(qp, FUN = function(qp){c = coef(qp)[3]}),
         aonr = -0.5 * (b_coef/c_coef),
         yaonr = (a_coef + I(-b_coef^2/(4 * c_coef)))) qp_regress
  
  ## maybe a way to solve the SC aonr at CC yield? 
  

qp_regress %>%
  mutate(pred_yield = map(qp, .f = predict, newdata = data.frame(rate = seq(0,270)))) %>%
  unnest(pred_yield) %>%
  group_by(site, year, rotation) %>%
  mutate(rate = as.numeric(as.character(seq(0,270))))


