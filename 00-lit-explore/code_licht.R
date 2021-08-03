# goal: look at mark's data
# created: 12/8/2020
# updated: 1/13/2021
#          2/24/2021 (assess power?)
# notes:

rm(list = ls())

library(tidyverse)
library(readxl)

library(lme4)
library(lmerTest)
library(emmeans)

# data --------------------------------------------------------------------

datraw <- read_csv("00-lit-explore/td_licht.csv")

#--viz
datraw %>% 
  ggplot(aes(Tillage, PlDenAve)) + 
  stat_summary(aes(color = PrevCrop, group = PrevCrop, pch = PrevCrop), size = 2) + 
  scale_color_manual(values = c("gold3", "green4")) + 
  labs(y = "Corn stand density (pl/ac)",
       subtitle = "Unpublished data from Licht, 11-site years",
       title = "Rotation effect on stand count depends on tillage")

ggsave("00-lit-explore/fig_licht.png")
  
datraw %>% 
    pull(SiteYear) %>% 
    unique()

datraw %>% 
  select(SiteYear) %>% 
  distinct()

#--ave amount less
datraw %>% 
  filter(Tillage %in% c("NT", "ST")) %>% 
  select(SiteYear, PrevCrop, Tillage, Rep, PlDenAve) %>% 
  pivot_wider(names_from = PrevCrop, values_from = PlDenAve) %>% 
  mutate(pct_diff = (Corn - Soybean)/Soybean * 100) %>% 
  summarise(pct_diff = mean(pct_diff, na.rm = T))


# plant density -----------------------------------------------------------

pden <- 
  datraw %>% 
  select(-(Mult:Prec)) %>% 


pden %>% 
  ggplot(aes(SiteYear, PlDenAve)) + 
  geom_point() + 
  facet_grid(.~PrevCrop)

#--what happened with the <25000
pden %>% 
  filter(PlDenAve < 25000)

#--should prob eliminate them
pdenf <- 
  pden %>% 
  filter(PlDenAve > 25000)

#--what is variation betweeen plots w/in site-yera?
#--looks like I could safely take the mean variation?
pdenf %>% 
  group_by(SiteYear, PrevCrop, Tillage) %>% 
  summarise(mx = max(PlDenAve, na.rm= T),
            mn = min(PlDenAve, na.rm= T),
            avg = mean(PlDenAve, na.rm = T)) %>% 
  ggplot(aes(color = PrevCrop)) + 
  geom_point(aes(SiteYear, avg)) +
  geom_linerange(aes(SiteYear, ymin = mn, ymax = mx)) +
  facet_wrap(~Tillage)

#--this gives us cohen's D, which is the mean/sp
pwr::pwr.t.test(n = 4, sig.level = .05, power = .95)

#--3.087 is cohen's d
cohen <- 3.087

#--to get sp (pooled standard deviation):
# sp = sqrt( d1/d2 )
# d1 = (n1-1) * s1^2 + (n2-1)*s2^2
# d2 = n1 + n2 - 2

pdenf %>% 
  group_by(SiteYear, PrevCrop, Tillage) %>% 
  summarise(sd = sd(PlDenAve, na.rm = T)) %>% 
  pivot_wider(names_from = PrevCrop, 
              values_from = sd) %>% 
  mutate(d1 = 3*Corn^2 + 3*Soybean^2,
         d2 = 4 + 4 - 2,
         sp = sqrt(d1/d2),
         det_dif = cohen * sp) %>% 
  group_by(SiteYear) %>% 
  summarise(det_dif = mean(det_dif)) %>% 
  summarise(det_dif = mean(det_dif))

#--we can only detect a difference bigger than 4500 plants/ac? Within a site year.  

#--but across 11 site years...the same? argh. 
myn <- 11
pdenf %>% 
  group_by(SiteYear, PrevCrop, Tillage) %>% 
  summarise(sd = sd(PlDenAve, na.rm = T)) %>% 
  pivot_wider(names_from = PrevCrop, 
              values_from = sd) %>% 
  mutate(d1 = (myn - 1)*Corn^2 + (myn - 1)*Soybean^2,
         d2 = myn + myn - 2,
         sp = sqrt(d1/d2),
         det_dif = cohen * sp) %>% 
  group_by(SiteYear) %>% 
  summarise(det_dif = mean(det_dif)) %>% 
  summarise(det_dif = mean(det_dif))

#--however, numerically it seems there is no difference

pdenf %>% 
#  filter(PrevCrop == "Corn") %>% 
  mutate(tillF = factor(Tillage, levels = c("MP", "DR", "CP", "ST", "NT"))) %>% 
  ggplot(aes(Tillage, PlDenAve)) + 
  geom_jitter(width = 0.2) + 
  facet_grid(.~PrevCrop)


# stats -------------------------------------------------------------------

##--comparing prev crop effect by tillage

#--use no outliers one
m1 <- lmer(PlDenAve ~ PrevCrop*Tillage + (1|SiteYear), data = pdenf)
anova(m1) #--need to keep interactions

em1 <- emmeans(m1, ~ PrevCrop|Tillage)

broom::tidy(contrast(em1)) %>% 
  filter(contrast == "Corn effect")

#--do the outliers make a difference? Yeah in the estimates, but not the p-values. But...I don't know. 
m2 <- lmer(PlDenAve ~ PrevCrop*Tillage + (1|SiteYear), data = pden)
anova(m2)

em2 <- emmeans(m2, ~ PrevCrop|Tillage)

broom::tidy(contrast(em2)) %>% 
  filter(contrast == "Corn effect")

##--comparing tillage effect w/in a prev crop

em3 <- emmeans(m1, ~ Tillage|PrevCrop)

#--within a continuous corn system, tillage does not change the stand count
broom::tidy(contrast(em3)) %>% 
  filter(PrevCrop == "Corn") %>% 
  arrange(estimate)

#--within a rotated corn system, tillage DOES change the stand count. It's higher in NT and ST. 
broom::tidy(contrast(em3)) %>% 
  filter(PrevCrop == "Soybean") %>% 
  arrange(estimate)
