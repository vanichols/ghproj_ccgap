# Created:       3/30/2020
# last edited:   
# 
# purpose: Create pred tibble for soil variables
#
# notes: 

rm(list = ls())

#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has his data in it :P
library(tidyverse)
library(lubridate)


# soil data, from package ----------------------------------------------

sc <- sad_soilchar %>% as_tibble() %>% 
  select(-cropprodindex_maj, -cropprodindex_wgt)

sprof <- sad_soilprof %>% as_tibble()


# wrangle -----------------------------------------------------------------

# sot wants clay 0-60cm
sclay <- 
  sprof %>% 
  filter(profile_cm %in% c("0-30", "30-60")) %>% 
  group_by(site) %>% 
  summarise(clay_pct = mean(clay_pct))


# sot wants soc 0-30
ssoc <- 
  sprof %>% 
  filter(profile_cm %in% c("0-30", "30-60")) %>% 
  group_by(site) %>% 
  summarise(soc_pct = mean(soc_pct))

# sot wants 1.5 m paw sum
spaw <- 
  sprof %>% 
  filter(!profile_cm %in% c("150-180")) %>% 
  group_by(site) %>% 
  summarise(paw_mm = sum(paw_mm))


# combine -----------------------------------------------------------------

soil_dat <- 
  sc %>% 
  left_join(sclay) %>% 
  left_join(ssoc) %>% 
  left_join(spaw) %>% 
  select(site, wtdepth_cm,
         iacsr_wgt,
         bhz_wt,
         clay_pct,
         soc_pct,
         paw_mm) %>% 
  rename(iacsr = iacsr_wgt,
         bhzdepth_cm = bhz_wt,
         clay_60cm_pct = clay_pct,
         soc_30cm_pct = soc_pct,
         paw_150cm_mm = paw_mm) %>% 
  mutate(wtdepth_cm = as.numeric(wtdepth_cm))


soil_dat %>% 
  ggplot(aes(soc_30cm_pct)) + 
  geom_histogram()

# write -------------------------------------------------------------------

soil_dat %>% write_csv("data/tidy/td_pred-soil.csv")



# play with pca -----------------------------------------------------------

df <- soil_dat %>% as.data.frame()

df2 <- df[-1]
row.names(df2) <- df$site

##--corelations?
library(corrplot)

df2_cor <- df2 %>%
  select_if(is.numeric) 
corres <- cor(df2_cor, use="complete.obs")
corrplot::corrplot.mixed(corres)

#--keep only the values that are most directly measured, not calculated
df3 <- df2 %>% 
  select(-paw_150cm_mm) #--this is calculated based on texture....

res2 <- prcomp(df3, scale = TRUE)
fviz_eig(res2)

res1 <- prcomp(df2, scale = TRUE)
fviz_eig(res1)

fviz_pca_ind(res2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



fviz_pca_var(res2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res1,
                repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/


library("factoextra")
data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])
res.pca <- prcomp(decathlon2.active, scale = TRUE)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
