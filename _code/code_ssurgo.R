# Created:       3/25/2020
# last edited:   3/25/2020
# 
# purpose: use feddata package to get ssurgo data?
# notes: 

rm(list = ls())
#devtools::install_github("vanichols/saapsim", force = T)
library(saapsim) #--my package, has his data in it :P
library(tidyverse)
library(janitor)
library(lubridate)
library(FedData)
library(sf)


#--get lat/long for site
lonlat <- c(-93,42)

shft <- 1e-3 ## This is 111 meters
lonlat.mat <- rbind(lonlat, ##root
                    lonlat + c(shft,0), ## x = 1, y = 0
                    lonlat + c(shft,shft), ## x = 1, y = 1
                    lonlat + c(0, shft), ## x = 0, y = 1
                    lonlat) ## back to root
rownames(lonlat.mat) <- NULL

#--the previous matrix is a rectangle, create a spatial polygon
pg <- Polygon(lonlat.mat)
spg <- SpatialPolygons(list(Polygons(list(pg), "s1")),
                       proj4string = CRS("+proj=longlat +datum=WGS84"))

#--does it look normal?
ggplot() + geom_sf(data = st_as_sf(spg))


# use get_ssurgo ----------------------------------------------------------

# only necessary once!
# label should be what to call that profile
# notice that our template is the 'SpatialPolygon' we created
# NOTE: This step may take awhile (minutes?)
# 
# cia.soil <- get_ssurgo(template = spg, label = "CIA")


# look at what we got -----------------------------------------------------

srgo_shp <- st_read("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_Mapunits.shp")
ggplot() + geom_sf(data = cia_shp, aes(fill = MUKEY))



# read in csv files -------------------------------------------------------

# 1. mapunit descriptions, has iacornsr
################ --- stopped  ----  ##############################
mu_dat <- read_csv("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_mapunit.csv") %>% 
  remove_empty("cols") #--removes empty columns


# 2. chorizon 
#  Horizon depths, sand, silt, clay, organic matter, water holding capacity, etc.
# .r means reference value, .l means low value, .h means high value
chor_dat <- read_csv("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_chorizon.csv") %>% 
  remove_empty("cols")

# 3. component, tells what each mukey is 'made up of' 
cmpnt_dat <- read_csv("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_component.csv") %>% 
  remove_empty("cols")


# Which data has info we want?
# NOTE: mu_dat doesn't have anything we want to visualize, so we won't use it. 


# ummm --------------------------------------------------------------------


## Step 4A: First we simplify the data so it is easier to work with

srgo_shp_simp <- 
  srgo_shp %>% 
  mutate(mukey = as.character(mukey)) #--we don't want it to be numeric

cmpnt_simp <- cmpnt_dat %>% 
  select(compname, comppct.r, mukey, cokey) %>% 
  mutate(mukey = as.character(mukey)) #--again, we don't want it to be numeric

chor_simp <- chor_dat %>% 
  select(hzname, om.r, cokey, chkey) %>% #--let's just do organic matter for now
  filter(str_detect("A", hzname)) #--let's only look at the A horizon (the top layer of soil)



########################STOP#################################
# NOTE: Why did we pick om.r? What does .r mean?
########################STOP#################################

chor_dat %>% 
  select(hzname, om.r, om.h, om.l, cokey, chkey) %>% #--let's just do organic matter for now
  filter(str_detect("A", hzname))



srgo_shp_simp
cmpnt_simp
chor_simp

## Step 4B: Get one organic matter value for each mukey

# Combine the components with the om% data

chor_cmpnt_simp <- 
  cmpnt_simp %>% #--has components of each mukey
  left_join(chor_simp) #--has organic matter for each component



# You'll notice the comppct.r doesn't add up to 100% for each mukey
# Let's just keep the component that is most dominant in the mukey

om_simp <- 
  chor_cmpnt_simp %>% 
  # remember how to filter out nas?
  filter(!is.na(chkey)) %>% 
  group_by(mukey) %>% 
  filter( comppct.r == max(comppct.r))


# Next we join the above data with our spatial data

new_srgo <- 
  srgo_shp_simp %>% 
  left_join(om_simp, by = "mukey")


# Step 5: Visualize soils data + yield data -------------------------------

# Now we can overlay the yield map data with the soils data

#--Yield
new_srgo %>% 
  ggplot() + 
  geom_sf(aes(fill = om.r)) +
  scale_fill_distiller(palette = rev("YlOrBr")) +
  geom_sf(data = yld_map, aes(color = yld_vol_dr)) + 
  scale_color_distiller(palette = "Greens")

#--Grain moisture
new_srgo %>% 
  ggplot() + 
  geom_sf(aes(fill = om.r)) +
  scale_fill_distiller(palette = rev("YlOrBr")) +
  geom_sf(data = yld_map, aes(color = moisture)) + 
  scale_color_distiller(palette = "Blues")


# Note: the yield nor the grain moisture seems to be affected by the soil type. 
# Do you remember the warning the SSURGO website gave us when we got the soils for Agronomy Hall?
# What scale are we supposed to interpret the information at?
# What scale are we operating at?
# Remember 0.001 degrees is roughly 100 m
# How many meters across is our field? 

