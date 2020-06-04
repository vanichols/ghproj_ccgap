# Created:       3/25/2020
# last edited:   3/25/2020
#                3/27/2020 (include planting dates)
#                3/30/2020 (added planting-date-referenced weather)
#                4/8/2020 (add Alison suggestions, play w/machine learning)
#                4/30/2020 (update file structure)
# 
# purpose: Create pred tibble for weather variables
#
# notes: 


# 
# The set of weather covariates for corn was as follows: aver-
#   age vapor pressure deficit (VPD) from 61 to 90 d after plant- ing, maximum air temperature 91 to 120 d after sowing, maximum air temperature in the 30 d leading up to planting, and precipitation 31 to 60 d after planting. For


rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2", force = T)
library(tidysawyer2) #--has saw_xx data
library(lubridate)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
library(tidyr)


# weather data, from tidysawyer2 package ----------------------------------------------

wea <- saw_wea %>% as_tibble() 

pwea <- 
  wea %>% 
  left_join(saw_planting) %>% 
  select(-plant_date, -crop) %>% 
  filter(!is.na(plant_doy))

#--check, all sites/years?
pwea %>% 
  select(site, year) %>% 
  distinct() 


# do a pca? ---------------------------------------------------------------

library("factoextra")
data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])
res.pca <- prcomp(decathlon2.active, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# #-bahhhhhh
# pwea %>% 
#   unite(site, year, col = "site_year") %>%
#   
#   column_to_rownames("site_year_day")
# 
# w.pca <- prcomp(pwea, scale = TRUE)

# weather in reference to planting ----------------------------------------

#--days w/max temp < 50 up to 30 days after planting
# wea1 <- 
#   pwea %>% 
#   group_by(site, year) %>% 
#   filter(day > plant_doy, 
#          day < plant_doy + 28) %>%
#   filter(tmax_c <= 10) %>% #--temp is in C
#   summarise(p4wk_less50_tmax_c = n())

# wea1 %>% 
#   ggplot(aes(p4wk_less50_tmax_c)) + 
#   geom_histogram()

#--I don't like this one, bad distribution

#--days w/avg temp < 50 up to 30 dap
# wea2 <- 
#   pwea %>% 
#   group_by(site, year) %>% 
#   filter(day > plant_doy,
#          day < plant_doy + 30) %>% 
#   mutate(tav = (tmax_c + tmin_c)/2) %>% 
#   filter(tav < 10) %>% 
#   summarise(wintcolddays_n = n())

#--precip_mmy days >1" from planting to 60 DAP
wea3 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 30) %>%
  filter(precip_mm >= 25.4) %>% #--precip_mm more than an inch
  summarise(p4wk_1inprecip_mm = n())

wea3 %>% 
  ggplot(aes(p4wk_1inprecip_mm)) + 
  geom_histogram()

# also bad distribution

#--days to reach 140 (or 120?)
wea4 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy) %>% 
  mutate(gdd = (tmax_c + tmin_c)/2 - 10,
         gdd = ifelse(gdd < 0, 0, gdd),
         gdd_cum = cumsum(gdd)) %>% 
  filter(gdd_cum <= 140) %>% 
  summarise(ndays_gdd140 = n())

wea4 %>% 
  ggplot(aes(ndays_gdd140)) + 
  geom_histogram()

#--precip_mm tot 2 weeks after planting
wea5 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 14) %>% 
  summarise(p2wk_precip_mm_tot = sum(precip_mm))

wea6 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 21) %>% 
  summarise(p3wk_precip_mm_tot = sum(precip_mm))

wea7 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 28) %>% 
  summarise(p4wk_precip_mm_tot = sum(precip_mm))

#--2 weeks before planting
wea8 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 14) %>% 
  summarise(prep2wk_precip_mm_tot = sum(precip_mm))

wea8 %>% 
  ggplot(aes(prep2wk_precip_mm_tot)) + 
  geom_histogram()


wea9 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 21) %>% 
  summarise(prep3wk_precip_mm_tot = sum(precip_mm))

wea10 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 28) %>% 
  summarise(prep4wk_precip_mm_tot = sum(precip_mm))

#--avg high temp in the month 2 months after planting
wea11 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy + 56, 
         day < plant_doy + 56 + 28) %>% 
  summarise(p2mo_tx_mean = mean(tmax_c))


#--avg low temp in the month surrounding planting
wea12 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy - 14, 
         day < plant_doy + 14) %>% 
  summarise(pre2wkp2wk_tl_mean = mean(tmin_c))

#--number of days < 4degF (-15C) before planting. Why 4 deg? Anna? Don't remember...
wea13 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy) %>% 
  filter(tmin_c < -15) %>% 
  summarise(wintcolddays_n = n())

wea13 %>% 
  ggplot(aes(wintcolddays_n)) + 
  geom_histogram()


#--gdd in 2 months after planting
wea14 <- 
  pwea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy,
         day < plant_doy + 60) %>%
  mutate(tmax_c = ifelse(tmax_c > 30, 30, tmax_c),
         gdd = (tmin_c + tmax_c)/2 - 10,
         gdd = ifelse(gdd < 0, 0, gdd)) %>% 
  summarise(p2mo_gdd = sum(gdd))

wea14 %>% 
  ggplot(aes(p2mo_gdd)) + 
  geom_histogram()



wea_hs <- 
  pwea %>% 
  filter(day < plant_doy + 120, day > plant_doy) %>%
  filter(tmax_c > 30) %>% 
  group_by(site, year) %>% 
  summarise(heatstress_n = n())

wea_hs %>% 
  ggplot(aes(heatstress_n)) + 
  geom_histogram()


# weather metrics ---------------------------------------------------------

#--rafa's scirep paper
#--teasdale and cavigelli 2017 paper (always in reference to planting....)
#--others?
#--sa uses precip_mmy days > 1inch, growing season GDD, heat days (Tmax > 34C), cold days (Tmin < 4), rad, avg T


#--mean july max temp
wea_july <- 
  wea %>% 
  filter(day < 212, day > 181) %>% 
  group_by(site, year) %>% 
  summarise(jul_tmin_c_mean = mean(tmin_c),
            jul_tmax_c_mean = mean(tmax_c),
            jul_precip_mm_tot = sum(precip_mm))

wea_may <- 
  wea %>% 
  filter(day < 151, day > 121) %>% 
  group_by(site, year) %>% 
  summarise(may_tmin_c_mean = mean(tmin_c),
            may_tmax_c_mean = mean(tmax_c),
            may_precip_mm_tot = sum(precip_mm))

wea_apr <- 
  wea %>% 
  filter(day < 120, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(apr_tmin_c_mean = mean(tmin_c),
            apr_tmax_c_mean = mean(tmax_c),
            apr_precip_mm_tot = sum(precip_mm))

wea_aprmay <- 
  wea %>% 
  filter(day < 151, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(aprmay_tmin_c_mean = mean(tmin_c),
            aprmay_tmax_c_mean = mean(tmax_c),
            aprmay_precip_mm_tot = sum(precip_mm))


wea_gs <- 
  wea %>% 
  filter(day < 244, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(gs_precip_mm_tot = sum(precip_mm),
            gs_tavg = mean((tmax_c + tmin_c)/2))



# combine wea metrics -------------------------------------------


wea_parms <- 
  wea_hs %>% 
  #left_join(wea1) %>% 
  #left_join(wea2) %>% #--not a lot of variation
  #left_join(wea3) %>% #--not much variation
  left_join(wea4) %>% 
  left_join(wea5) %>% 
  #left_join(wea6) %>% 
  #left_join(wea7) %>% 
  left_join(wea8) %>% 
  #left_join(wea9) %>%
  #left_join(wea10) %>% 
  #left_join(wea11) %>% #--correlated w/days to gdd140 for some reason
  left_join(wea12) %>% 
  left_join(wea13) %>% 
  left_join(wea14) %>% 
  #left_join(wea_july) %>% #--I kind of don't like the things not referenced to planting
  #left_join(wea_may) %>% 
  #left_join(wea_apr) %>% 
  #left_join(wea_aprmay) %>%
  #left_join(wea_gs) %>%
  ungroup() %>% 
  mutate_if(is.numeric, list(~replace_na(., 0)))

# what is correlated? -----------------------------------------------------
library(corrplot)

wea_cor <- 
  wea_parms %>%
  ungroup() %>% 
  select_if(is.numeric) 
corres <- cor(wea_cor, use="complete.obs")
corrplot::corrplot.mixed(corres)
corrplot::corrplot(corres)

ggsave("01_create-features/fig_wea-corrs.png")

# i like heatstress more than gs_tavg I think, comment out line 272

wea_pca <- 
  wea_parms %>% 
  unite(site, year, col = "site_year") %>% 
  column_to_rownames("site_year")

res_pca <- prcomp(wea_pca, scale = TRUE)
fviz_eig(res_pca)
fviz_pca_ind(res_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(res_pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)



# write what i like -------------------------------------------------------

wea_parms %>% write_csv("01_create-features/cf_pred-wea.csv")


# when did heatstres occur ------------------------------------------------


hs_sum <- 
  pwea %>% 
  filter(day < plant_doy + 120, day > plant_doy) %>%
  filter(tmax_c > 30) %>% 
  group_by(site, year) %>% 
  summarise(heatstress_n = n())

hs_sum %>% 
  ggplot(aes(reorder(site, heatstress_n, mean), heatstress_n)) + 
  geom_col(aes(fill = as.factor(year)), position = "dodge") + 
  guides(fill = F)

#--crawfordsville has seasons w/more than 60 days
pwea %>% 
  filter(site == "craw") %>% 
  mutate(hs = ifelse(tmax_c > 30, "hot", "none")) %>%
  ggplot(aes(day, tmax_c)) + 
  geom_point(aes(color = hs)) +
  geom_vline(aes(xintercept = plant_doy)) +
  geom_vline(aes(xintercept = plant_doy + 120)) +
  geom_label(data = filter(hs_sum, site == "craw"),
             x = 200, y = 0, aes(label = heatstress_n)) +
  facet_wrap(~year) + 
  labs(title = "Crawfordsville days w/Tmax > 30degC",
       subtitle = "Planting to 120 DAP")

ggsave("01_create-features/fig_craw-heatstress-days.png")


#--ames has less in general
pwea %>% 
  filter(site == "nash") %>% 
  mutate(hs = ifelse(tmax_c > 30, "hot", "none")) %>%
  ggplot(aes(day, tmax_c)) + 
  geom_point(aes(color = hs)) +
  geom_vline(aes(xintercept = plant_doy)) +
  geom_vline(aes(xintercept = plant_doy + 120)) +
  geom_label(data = filter(hs_sum, site == "nash"),
             x = 200, y = 0, aes(label = heatstress_n)) +
  facet_wrap(~year) + 
  labs(title = "Nashua days w/Tmax > 30degC",
       subtitle = "Planting to 120 DAP")

ggsave("01_create-features/fig_nash-heatstress-days.png")
