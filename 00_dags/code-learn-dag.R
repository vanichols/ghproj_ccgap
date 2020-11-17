library(dagitty)
library(ggdag)


dag <- dagitty("dag{y <- z -> x}")
tidy_dagitty(dag)


dagified <- dagify(x ~ z,
                   y ~ z,
                   exposure = "x",
                   outcome = "y")
tidy_dagitty(dagified)

ggdag(dag, layout = "circle")
ggdag(dagified, layout = "circle")


# List all the paths connecting LAI (the potential cause of interest) and seed protein
# concentration (the outcome).

# List all paths connecting previous crop (potential cause of itnerest) and stand count

sm_dag1 <- dagify(
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


coords <- list(
  x = c(prev_crop = 1, res_amt = 2, res_cn = 2, root_chans = 2, soilN_stat = 2,
        disease_press = 2, disease_type = 2, worm_pop = 2,
        phys_barrier = 3, soil_temp = 3, soil_mois = 3,
        soil_erosion = 4, rain_impact = 4, speed_germ = 4, 
        seedling_surv = 5, plant_pop = 6),
  y = c(prev_crop = 0, res_amt = 0, res_cn = 1, root_chans = 2, soilN_stat = 3,
        disease_press = -4, disease_type = -2, worm_pop = -1,
        phys_barrier = 2, soil_temp = 0, soil_mois = -2,
        soil_erosion = 3, rain_impact = 2, speed_germ = 0, 
        seedling_surv = -3, plant_pop = 0)
)

coord_df <- coords2df(coords)
coordinates(sm_dag1) <- coords2list(coord_df)
ggdag(sm_dag1, text_col = "red", node = F) + 
  theme_dag_blank()

