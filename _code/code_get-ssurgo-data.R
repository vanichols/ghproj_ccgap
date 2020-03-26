# Created:       3/25/2020
# last edited:   3/26/2020 (upgrade from practice to actual lat/lon)
# 
# purpose: use feddata package to get ssurgo data?
# notes: only do this once to download data


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


# start data download (only do once) --------------------------------------
mysites <- sll %>% select(site) %>% pull()

mychoice <- mysites[8]
mychoice
# ames, 9:25am
# kana, 9:28am
# craw, 9:32
# lewi, 11:08
# mcna, 11:13
# nash, 11:18
# suth, 11:20

sad_tidysawyer %>% select(site) %>% pull() %>% unique()

dat <- 
  sll %>% 
  filter(site == mychoice)


#--get lat/long for site
lonlat <- c(dat$lon, dat$lat)

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

soil <- get_ssurgo(template = spg, label = mychoice)


