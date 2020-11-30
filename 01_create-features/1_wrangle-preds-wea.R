# Created:       3/25/2020
# last edited:   3/25/2020
#                3/27/2020 (include planting dates)
#                3/30/2020 (added planting-date-referenced weather)
#                4/8/2020 (add Alison suggestions, play w/machine learning)
#                4/30/2020 (update file structure)
#                11/30/2020 (revisting. added illinois)
# 
# purpose: Create pred tibble for weather variables
#
# notes: 


# 
# The set of weather covariates for corn was as follows: aver-
#   age vapor pressure deficit (VPD) from 61 to 90 d after plant- ing, maximum air temperature 91 to 120 d after sowing, maximum air temperature in the 30 d leading up to planting, and precipitation 31 to 60 d after planting. For


rm(list = ls())
#devtools::install_github("vanichols/tidysawyer2", force = T)
library(tidysawyer2) #--has wea data
library(lubridate)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)
library(tidyr)

data("ilia_wea")

# ia_weather data, from tidysawyer2 package ----------------------------------------------
library(saapsim)
saf_date_to_doy("2001-10-01")

plantdat <- bind_rows(ia_planting, il_planting)

wea <- 
  ilia_wea %>% 
  left_join(plantdat) %>% #--note the weather files start in 1980
  select(-plant_date, -crop) %>% 
  filter(!is.na(plant_doy))

#--check, all sites/years?
wea %>% 
  select(site, year) %>% 
  distinct() 

wea %>% 
  group_by(state, year, site) %>% 
  summarise(precip_mm = sum(precip_mm, na.rm = T)) %>% 
  ggplot(aes(year, precip_mm, color = state)) + 
  geom_point()

wea1 <- 
  wea %>% 
  select(state, site, year) %>% 
  distinct() 


#--water year precip (prev Oct to July?)
wea2 <- 
  wea %>% 
  mutate(newyeargroup = ifelse( (day > 182 & day < 274), NA,
                                ifelse(
                                  day > 182, year - 1, year))) %>% 
  group_by(newyeargroup, site) %>% 
  summarise(wyprecip_mm = sum(precip_mm, na.rm = T)) %>% 
  ungroup() %>%
  rename(year = newyeargroup)

wea2 %>% 
  ggplot(aes(wyprecip_mm)) + 
  geom_histogram()

#--precip_mmy days >1" from planting to 60 DAP
wea3 <- 
  wea %>% 
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
  wea %>% 
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
  wea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 14) %>% 
  summarise(p2wk_precip_mm_tot = sum(precip_mm))

#--3 weeks
wea6 <- 
  wea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 21) %>% 
  summarise(p3wk_precip_mm_tot = sum(precip_mm))

#--4 weeks
wea7 <- 
  wea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy, 
         day < plant_doy + 28) %>% 
  summarise(p4wk_precip_mm_tot = sum(precip_mm))

#--2 weeks before planting
wea8 <- 
  wea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 14) %>% 
  summarise(prep2wk_precip_mm_tot = sum(precip_mm))

wea8 %>% 
  ggplot(aes(prep2wk_precip_mm_tot)) + 
  geom_histogram()

#--3 weeks
wea9 <- 
  wea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 21) %>% 
  summarise(prep3wk_precip_mm_tot = sum(precip_mm))

#--4 weeks
wea10 <- 
  wea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy, 
         day > plant_doy - 28) %>% 
  summarise(prep4wk_precip_mm_tot = sum(precip_mm))

#--avg high temp in the month 2 months after planting
wea11 <- 
  wea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy + 56, 
         day < plant_doy + 56 + 28) %>% 
  summarise(p2mo_tx_mean = mean(tmax_c))


#--avg low temp in the month surrounding planting
wea12 <- 
  wea %>% 
  group_by(site, year) %>% 
  filter(day > plant_doy - 14, 
         day < plant_doy + 14) %>% 
  summarise(pre2wkp2wk_tl_mean = mean(tmin_c))

#--number of days < 4degF (-15C) before planting. Why 4 deg? Anna? Don't remember...
wea13 <- 
  wea %>% 
  group_by(site, year) %>% 
  filter(day < plant_doy) %>% 
  filter(tmin_c < -15) %>% 
  summarise(wintcolddays_n = n())

wea13 %>% 
  ggplot(aes(wintcolddays_n)) + 
  geom_histogram()


#--gdd in 2 months after planting
wea14 <- 
  wea %>% 
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


#--heat stress yes/no days
wea_hs <- 
  wea %>% 
  filter(day < plant_doy + 120, day > plant_doy) %>%
  filter(tmax_c > 30) %>% 
  group_by(site, year) %>% 
  summarise(heatstress_n = n())

wea_hs %>% 
  ggplot(aes(heatstress_n)) + 
  geom_histogram()


#--heat stress gradient
wea_hs2 <- 
  wea %>% 
  filter(day < plant_doy + 120, day > plant_doy) %>%
  filter(tmax_c > 30) %>%
  mutate(tstress = tmax_c - 30) %>% 
  group_by(site, year) %>% 
  summarise(heatstress_cum = sum(tstress))

wea_hs2 %>% 
  ggplot(aes(heatstress_cum)) + 
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


wea_gsT <- 
  wea %>% 
  filter(day < 244, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(gs_tavg = mean((tmax_c + tmin_c)/2))

wea_gsP <- 
  wea %>% 
  filter(day < 244, day > 91) %>% 
  group_by(site, year) %>% 
  summarise(gs_precip_mm_tot = sum(precip_mm))


# combine wea metrics -------------------------------------------


wea_parms_all <- 
  wea1 %>% 
  left_join(wea2) %>% #--water year precip
  #left_join(wea3) %>% #--precip_mmy days >1" from planting to 60 DAP, not much variation
  #left_join(wea4) %>% #--days to reach 140 (or 120?)
  left_join(wea5) %>% #--precip_mm tot 2 weeks after planting
  #left_join(wea6) %>% #--3 wks
  #left_join(wea7) %>% #--4 wks
  left_join(wea8) %>% #--2 wks before
  #left_join(wea9) %>% #--3 wks
  #left_join(wea10) %>% #--4wks
  #left_join(wea11) %>% #--mo avg tmax 2 mo ap, corr w/days to gdd140 for some reason
  left_join(wea12) %>% #--avg tmin in the month surrounding planting
  left_join(wea13) %>% #--# days < 4degF (-15C) before planting. Why 4 deg? Anna? Don't remember...
  #left_join(wea14) %>% #--gdd in 2 months after planting
  #left_join(wea_hs) %>% #-heat stress yes/no days
  left_join(wea_hs2) %>% #--heat stress gradient
  #left_join(wea_july) %>% #--I kind of don't like the things not referenced to planting
  #left_join(wea_may) %>% 
  #left_join(wea_apr) %>% 
  #left_join(wea_aprmay) %>%
  #left_join(wea_gsT) %>% #--gs_tavg
  left_join(wea_gsP) %>% #--gs_precip_mm_tot
  ungroup() %>% 
  mutate_if(is.numeric, list(~replace_na(., 0)))

# what is correlated? -----------------------------------------------------
library(corrplot)

wea_cor <- 
  wea_parms_all %>%
  ungroup() %>% 
  select_if(is.numeric) 
corres <- cor(wea_cor, use="complete.obs")
corrplot::corrplot.mixed(corres)
corrplot::corrplot(corres)

ggsave("01_create-features/1_fig_wea-corrs.png")

#---this is just for deciding
#--which have lots of correlations
corres %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  pivot_longer(2:ncol(.)) %>% 
  filter(abs(value) > 0.6, value != 1) %>% 
  group_by(rowname) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

corres %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  pivot_longer(2:ncol(.)) %>% 
  filter(abs(value) > 0.6, value != 1) 


corres %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  pivot_longer(year:gs_tavg) %>% 
  filter(abs(value) > 0.6, value != 1) %>% 
  group_by(rowname) %>% 
  mutate(n = n())


# do a pca? ---------------------------------------------------------------

wea_pca <- 
  wea_parms_all %>% 
  unite(site, year, col = "site_year") %>% 
  column_to_rownames("site_year")

# res_pca <- prcomp(wea_pca, scale = TRUE)
# fviz_eig(res_pca)
# fviz_pca_ind(res_pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# fviz_pca_var(res_pca,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# fviz_pca_biplot(res_pca, repel = TRUE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969"  # Individuals color
# )
# 


# write what i like -------------------------------------------------------

wea_parms_all %>% write_csv("01_create-features/1_preds-wea.csv")


# when did heatstres occur ------------------------------------------------


hs_sum <- 
  wea %>% 
  filter(day < plant_doy + 120, day > plant_doy) %>%
  filter(tmax_c > 30) %>% 
  group_by(site, year) %>% 
  summarise(heatstress_n = n())

hs_sum %>% 
  ggplot(aes(reorder(site, heatstress_n, mean), heatstress_n)) + 
  geom_col(aes(fill = as.factor(year)), position = "dodge") + 
  guides(fill = F)

#--crawfordsville, brow, and dsup are the worst, has seasons w/more than 60 days
wea %>% 
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

ggsave("01_create-features/1_fig_ex-craw-heatstress-days.png")


#--ames has less in general
wea %>% 
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

ggsave("01_create-features/1_fig_ex-nash-heatstress-days.png")


# final decisions w/ just iowa, for reference ---------------------------------------------------------


wea_parms <- 
  wea_hs %>% 
  #left_join(wea1) %>% 
  left_join(wea2) %>% 
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
  #left_join(wea_hs) %>% 
  left_join(wea_hs2) %>% 
  #left_join(wea_july) %>% #--I kind of don't like the things not referenced to planting
  #left_join(wea_may) %>% 
  #left_join(wea_apr) %>% 
  #left_join(wea_aprmay) %>%
  #left_join(wea_gs) %>%
  ungroup() %>% 
  mutate_if(is.numeric, list(~replace_na(., 0)))

