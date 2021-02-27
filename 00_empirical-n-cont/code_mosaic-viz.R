# created 2/18/21
# gina
# purpose: figure out how to visualize the fits
# updated: 2/25/21 - miranda helped! probs with facets...


rm(list = ls())
library(tidysawyer2)
library(tidyverse)
library(scales)
library(ggmosaic)



# data --------------------------------------------------------------------

dat <- 
  read_csv("00_empirical-n-cont/fits-npct.csv") %>% 
  filter(!is.na(ngap_frac)) %>% 
  select(site, year, gap_at_rotaonr_kgha, ngap_frac) %>%
  mutate(
    ngap_frac = round(ngap_frac, 2),
    ogap_frac = 1-ngap_frac,
    ngap_pct = round(ngap_frac * 100, 0),
    ogap_pct = round(ogap_frac * 100, 0),
    yieldgap = round(gap_at_rotaonr_kgha/100, 0)) %>% 
  filter(yieldgap > 0) %>% 
  select(-gap_at_rotaonr_kgha)


# geom_col ----------------------------------------------------------------

#--use geom_col
dat %>% 
  select(-ngap_pct, -ogap_pct) %>% 
  pivot_longer(ngap_frac:ogap_frac) %>% 
  ggplot(aes(as.factor(year), value)) + 
  geom_col(aes(fill = name)) + 
  facet_wrap(~site, scales = "free_x")

#--use geom_col
dat %>% 
  select(-ngap_pct, -ogap_pct) %>% 
  pivot_longer(ngap_frac:ogap_frac) %>% 
  unite(site, year, col = "siteyear", remove = F) %>% 
  ggplot(aes(siteyear, value)) + 
  geom_col(aes(fill = name, color = site)) 

#--use geom_col
dat %>% 
  select(-ngap_pct, -ogap_pct, -ogap_frac) %>% 
  unite(site, year, col = "siteyear", remove = F) %>% 
  ggplot(aes(siteyear, ngap_frac)) + 
  geom_col(aes(fill = site), color = "black") 

#--make widths proportional to yield
dat %>% 
  pivot_longer(nfrac:ofrac) %>% 
  ggplot(aes(year, value)) + 
  geom_col(aes(fill = name, width = yield/10))

dat %>% 
  select(-ngap_pct, -ogap_pct, -ogap_frac) %>% 
  unite(site, year, col = "siteyear", remove = F) %>% 
  ggplot(aes(siteyear, ngap_frac)) + 
  geom_col(aes(fill = site, width = yieldgap/100), color = "black")

#--make widths proportional to yield, year as factor?
dat %>% 
  pivot_longer(nfrac:ofrac) %>% 
  ggplot(aes(as.factor(year), value)) + 
  geom_col(aes(fill = name, width = yield/10000))



# test it on one site -----------------------------------------------------

tst <- 
  dat %>% 
  filter(site == "ames")

#--create dummy dataframe
tst_pdat <- data.frame(Year = rep(tst$year, times = tst$yieldgap))    

#--create a type column
Type <- c()
for(i in 1:nrow(tst)) {
  
  Type <- c(Type,
            rep("nfrac", times = round(tst$yieldgap[i] * tst$ngap_frac[i], 0) ),
            rep("ofrac", times = round(tst$yieldgap[i] * tst$ogap_frac[i], 0) )
  )
}


tst_pdat$Type <- as.factor(Type)

ggplot(tst_pdat) + 
  geom_mosaic(aes(x = product(Type, Year), fill = Type)) +
  labs(x="Year", y = "Type") +
  scale_fill_manual(values = c("nfrac" = "red",
                               "ofrac" = "gray80"))


# how to facet? -----------------------------------------------------------

tst <- 
  dat %>% 
  filter(site %in% c("craw")) 

#--make a function
mosaicFun <- function(f.dat = tst) {
  
  newdat <- f.dat
  
  #-number of rows is sum of yieldgaps
  plot_dat <- data.frame(year = rep(newdat$year, times = newdat$yieldgap*100)) %>% as_tibble()    
  
  fractype <- c()
  
  for(i in 1:nrow(f.dat)) {
      fractype <- c(fractype,
              rep("nfrac", newdat$yieldgap[i] * newdat$ngap_pct[i]),
              rep("ofrac", newdat$yieldgap[i] * newdat$ogap_pct[i]))
  }
  plot_dat$fractype <- as.factor(fractype)
  plot_dat$site <- rep(newdat$site, times = newdat$yieldgap*100)
  
  res <- plot_dat %>% as_tibble()
  return(res)   
}

pdat <- mosaicFun(tst)
pdat$fs <- as.factor(paste(pdat$fractype, pdat$site)) # crude factor for colors

ggplot(pdat) + geom_mosaic(aes(x = product(fractype, year), fill = fs))



# hmmm --------------------------------------------------------------------

dat %>% pull(site) %>% unique()

tst3 <- 
  dat %>% 
  filter(site %in% c("ames", "nash"))

pdat <- mosaicFun(tst3)
pdat$fs <- as.factor(paste(pdat$fractype, pdat$site)) # crude factor for colors

#--something is wrong?
ggplot(pdat) + geom_mosaic(aes(x = product(fractype, year), fill = fs)) 

