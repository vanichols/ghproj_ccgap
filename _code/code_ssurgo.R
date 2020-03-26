# Created:       3/25/2020
# last edited:   3/26/2020 (upgrade from practice to actual lat/lon)
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


# lat/lon each site (from saapsim pkg) ------------------------------------

sll <- sad_siteinfo %>% 
  #--this is fixed in newer pkg, delete once it's updated
  mutate(site = str_sub(str_to_lower(site_name), 1, 4)) %>% 
  filter(site != "kell") %>% 
  rename(lon = long) %>%
  #--keep this
  select(site, lat, lon)


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


#--two methods:
#1. take thing that has highest muacres (major mapunit), steal iacornsr

iasr_maj <- 
  mu_dat %>% 
  filter(muacres == max(muacres)) %>% 
  select(iacornsr) %>% 
  rename(iacsr_maj = iacornsr)


#. do a weighted average
iasr_wgt <- 
  mu_dat %>%
    mutate(ac_tot = sum(muacres),
           wgt = muacres/ac_tot,
           val = wgt * iacornsr) %>% 
    summarise(iacsr_wgt = sum(val))


# 2. component, tells what each mukey is 'made up of', has mukey and cokey
cmpnt_dat <- read_csv("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_component.csv") %>% 
  remove_empty("cols") %>% 
  select(mukey, cokey, everything())

cmpnt_major <- 
  cmpnt_dat %>% 
  filter(majcompflag == "Yes") %>% 
  select(mukey, cokey)


# 3. chorizon 
#  Horizon depths, sand, silt, clay, organic matter, water holding capacity, etc.
# .r means reference value, .l means low value, .h means high value
# we just want depth to b horizon
chor_dat <- read_csv("EXTRACTIONS/CIA/SSURGO/CIA_SSURGO_chorizon.csv") %>% 
  remove_empty("cols")

ahorz_dat <- 
  chor_dat %>% 
  select(cokey, chkey, hzname, desgnmaster, contains(".r") & contains("hz")) %>% 
  filter(desgnmaster == "A") %>% 
  #--average by cokey (?) 
  group_by(cokey) %>% 
  summarise(depth_to_Bhz_cm = mean(hzdepb.r, na.rm = T))

# 1. use major cmpnt, and major mapunit
hzb_maj <- 
  mu_dat %>% 
  filter(muacres == max(muacres)) %>% 
  left_join(cmpnt_major) %>% 
  left_join(ahorz_dat) %>% 
  select(depth_to_Bhz_cm) %>% 
  rename(bhz_maj = depth_to_Bhz_cm)

# 2. use weighted cmpnts and mapunits

hzb_wgt <- 
  cmpnt_dat %>% 
  select(mukey, cokey, comppct.r)  %>% 
  left_join(ahorz_dat) %>%
  group_by(mukey) %>% 
  mutate(comp_tot = sum(comppct.r),
         wgt = comppct.r / comp_tot,
         val = depth_to_Bhz_cm * wgt) %>% 
  summarise(bhz_wt = sum(val))

