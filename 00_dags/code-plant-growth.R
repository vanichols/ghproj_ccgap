# make a dag for prev crop -> plant pop
# 11/11/20 looked at it


#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

library(dagitty)
library(ggdag)


nt_dag <- dagify(
  #-prev crop
  res_amt ~ prev_crop,
  res_cn ~ prev_crop,
  root_chans ~ prev_crop,
  soilN_stat ~ prev_crop,
  disease_press ~ prev_crop,
  disease_type ~ prev_crop,
  worm_pop ~ prev_crop,
  #-res amt
  phys_barrier ~ res_amt,
  soil_temp ~ res_amt,
  soil_mois ~ res_amt,
  disease_press ~ res_amt,
  #--soil phys bar
  soil_erosion ~ phys_soil_bar,
  rain_impact ~ phys_barrier,
  seedling_surv ~ phys_barrier,
  #--derivs
  seedling_surv ~ soil_erosion,
  seedling_surv ~ rain_impact,
  soil_erosion ~ phys_barrier,
  #--soil temp
  speed_germ ~ soil_temp,
  disease_press ~ soil_temp,
  seedling_surv ~ soil_temp,
  soil_mois ~ soil_temp,
  #--soil mois
  disease_press ~ soil_mois,
  seedling_surv ~ soil_mois,
  disease_press ~ speed_germ,
  plant_pop ~ seedling_surv,
  seedling_surv ~ disease_press,
  exposure = "prev_crop",
  outcome = "plant_pop")


nt_coords <- list(
  x = c(prev_crop = 1, 
        res_amt = 2, res_cn = 2, root_chans = 2, soilN_stat = 2,
        disease_press = 2, disease_type = 2, worm_pop = 2,
         soil_temp = 3, soil_mois = 3,
        phys_barrier = 3.5, 
        soil_erosion = 5, rain_impact = 5, speed_germ = 5, 
        seedling_surv = 6, 
        plant_pop = 7),
  y = c(
    #--first col
    prev_crop = 0, 
    #--2nd col
    soilN_stat = 3,
    worm_pop = 2,
    root_chans = 1, 
    disease_type = 0, 
    res_cn = -1, 
    res_amt = -2, 
    disease_press = -4,
    #--3rd col
    phys_barrier = 3, 
    soil_temp = 0, 
    soil_mois = -2,
    #--x = 4
    soil_erosion = 4, 
    rain_impact = 3,
    speed_germ = 0,
    #--x = 5
    seedling_surv = -3, 
    plant_pop = 0)
  )

coordinates(nt_dag) <- nt_coords
nt_fig <- ggdag(nt_dag, text_col = "red", node = F) + 
  theme_dag_blank() + 
  labs(title = "Effect of previous crop on plant pop: No-till systems")
  
nt_fig

till_dag <- dagify(
  #-prev crop
  res_amt ~ prev_crop,
  res_cn ~ prev_crop,
  root_chans ~ prev_crop,
  soilN_stat ~ prev_crop,
  disease_press ~ prev_crop,
  disease_type ~ prev_crop,
  worm_pop ~ prev_crop,
  #-res amt
  disease_press ~ res_amt,
  #--soil phys bar
  #--soil temp
  plant_pop ~ seedling_surv,
  seedling_surv ~ disease_press,
  exposure = "prev_crop",
  outcome = "plant_pop")


till_coords <- list(
  x = c(prev_crop = 1, 
        res_amt = 2, 
        res_cn = 2, 
        root_chans = 2, 
        soilN_stat = 2,
        disease_press = 2, 
        disease_type = 2,
        worm_pop = 2,
        seedling_surv = 4, 
        plant_pop = 6),
  y = c(prev_crop = 0, 
        soilN_stat = 3,
        worm_pop = 2,
        root_chans = 1, 
        disease_type = 0, 
        res_cn = -1, 
        res_amt = -2, 
        disease_press = -4,
        seedling_surv = 0, plant_pop = 0)
)

coordinates(till_dag) <- till_coords
till_fig <- ggdag(till_dag, text_col = "red", node = F) + 
  theme_dag_blank() + 
  labs(title = "Effect of previous crop on plant pop: Tilled systems")

till_fig

library(patchwork)
nt_fig / till_fig
ggsave("dag_prevcrop-plantpop-v1.png")
