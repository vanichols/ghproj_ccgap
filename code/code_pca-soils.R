
soil_dat <- read_csv("data/tidy/td_pred-soil.csv")

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
