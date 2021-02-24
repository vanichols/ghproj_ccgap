library(tidyverse)
library(tibble)
library(ggplot2)
library(tidyr)
library(ggmosaic)

dat <- tibble(year = c(2003, 2004, 2005),
       yield = c(5574, 3477, 1064),
       nfrac = c(50, 40, 80),
       ofrac = c(50, 60, 20))

#--use geom_col
dat %>% 
  pivot_longer(nfrac:ofrac) %>% 
  ggplot(aes(year, value)) + 
  geom_col(aes(fill = name))

#--make widths proportional to yield
dat %>% 
  pivot_longer(nfrac:ofrac) %>% 
  ggplot(aes(year, value)) + 
  geom_col(aes(fill = name, width = yield/10))

#--make widths proportional to yield, year as factor?
dat %>% 
  pivot_longer(nfrac:ofrac) %>% 
  ggplot(aes(as.factor(year), value)) + 
  geom_col(aes(fill = name, width = yield/10000))



# miranda -----------------------------------------------------------------

dat2 <- dat
dat2$nfrac.p <- dat$nfrac / 100
dat2$ofrac.p <- dat$ofrac / 100
dat2$yield <- 100*dat$yield

plot_dat <- data.frame(Year = rep(dat2$year, times = dat2$yield))    

Type <- c()
for(i in 1:nrow(dat)) {
  Type <- c(Type,
            rep("nfrac", dat2$yield[i] * dat2$nfrac.p[i]),
            rep("ofrac", dat2$yield[i] * dat2$ofrac.p[i]))
}
plot_dat$Type <- as.factor(Type)
ggplot(plot_dat) + geom_mosaic(aes(x = product(Type, Year), fill = Type)) +
  labs(x="Year", y = "Type") 


tst <- 
  read_csv("00_empirical-n-cont/fits-npct.csv") %>% 
  filter(site == "ames") %>% 
  filter(!is.na(ngap_frac)) %>% 
  select(site, year, gap_at_rotaonr_kgha, ngap_frac) %>% 
  mutate(ogap_frac = 1-ngap_frac,
         yieldgap = round(1*gap_at_rotaonr_kgha, 0))

head(plot_dat)

plot_dat <- data.frame(Year = rep(tst$year, times = tst$yieldgap))    

Type <- c()
for(i in 1:nrow(tst)) {

  Type <- c(Type,
            rep("nfrac", times = round(tst$yieldgap[i] * tst$ngap_frac[i], 0) ),
            rep("ofrac", times = round(tst$yieldgap[i] * tst$ogap_frac[i], 0) )
            )
}


plot_dat$Type <- as.factor(Type)

fig_mos <- 
  ggplot(plot_dat) + geom_mosaic(aes(x = product(Type, Year), fill = Type)) +
  labs(x="Year", y = "Type") +
  scale_fill_manual(values = c("nfrac" = "red",
                               "ofrac" = "gray80"))

fig_mos

#--use geom_col
fig_norm <- 
  tst %>% 
  filter(!is.na(ngap_frac)) %>% 
  rename("agap_frac" = ogap_frac) %>% 
  pivot_longer(ngap_frac:agap_frac) %>% 
  ggplot(aes(as.factor(year), value)) + 
  geom_col(aes(fill = name)) + 
  scale_fill_manual(values = c("ngap_frac" = "red",
                               "agap_frac" = "gray80"))


fig_norm
library(patchwork)

fig_mos/fig_norm
