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
library(sf)
library(glue)


hm <- read_csv("EXTRACTIONS/ames/SSURGO/ames_SSURGO_component.csv") 
  
hm %>% select(drainagecl)


# a helper function -------------------------------------------------------

fun_summarize_ssurgo <- function(f_mychoice = "ames") {
  
  ########--for trouble
  #f_mychoice <- "ames"
  
  my_dir <- glue('EXTRACTIONS/{f_mychoice}/SSURGO/') %>% as.character()
  my_shp <-
    glue('{my_dir}{f_mychoice}_SSURGO_Mapunits.shp') %>% as.character()
  my_mu <-
    glue('{my_dir}{f_mychoice}_SSURGO_mapunit.csv') %>% as.character()
  my_cmpnt <-
    glue('{my_dir}{f_mychoice}_SSURGO_component.csv') %>% as.character()
  my_chor <-
    glue('{my_dir}{f_mychoice}_SSURGO_chorizon.csv') %>% as.character()
  
  # look at what we got -----------------------------------------------------
  
  srgo_shp <- st_read(my_shp)
  ggplot() + geom_sf(data = srgo_shp, aes(fill = MUKEY))
  
  
  # read in csv files -------------------------------------------------------
  
  # 1. mapunit descriptions, has iacornsr, I think that's it
  
  mu_dat <- read_csv(my_mu) %>%
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
    mutate(
      ac_tot = sum(muacres),
      wgt = muacres / ac_tot,
      val = wgt * iacornsr
    ) %>%
    summarise(iacsr_wgt = sum(val))
  
  
  # 2. component, tells what each mukey is 'made up of', has mukey and cokey
  cmpnt_dat <- read_csv(my_cmpnt) %>%
    remove_empty("cols") %>%
    select(mukey, cokey, everything())
  
  cmpnt_major <-
    cmpnt_dat %>%
    filter(majcompflag == "Yes") %>%
    select(mukey, cokey)
  
  
  draincl_major <- cmpnt_dat %>% 
    filter(majcompflag == "Yes") %>% 
    select(mukey, cokey, drainagecl) %>% 
    left_join(mu_dat) %>% 
    filter(muacres == max(muacres)) %>%
    select(drainagecl) %>%
    rename(draincl_maj = drainagecl)
  
  
  
  # 3. chorizon
  #  Horizon depths, sand, silt, clay, organic matter, water holding capacity, etc.
  # .r means reference value, .l means low value, .h means high value
  
  #--we just want depth to b horizon? can I get wt stuff here?
  chor_dat <- read_csv(my_chor) %>%
    remove_empty("cols") %>% 
    select(cokey,
           chkey,
           hzname,
           desgnmaster,
           contains(".r")) 
  
  ahorz_dat <-
    chor_dat %>%
    select(cokey,
           chkey,
           hzname,
           desgnmaster,
           contains(".r") & contains("hz")) %>%
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
    mutate(
      comp_tot = sum(comppct.r),
      wgt = comppct.r / comp_tot,
      val = depth_to_Bhz_cm * wgt
    ) %>%
    summarise(bhz_wt = sum(val)) %>%
    left_join(mu_dat) %>%
    filter(muacres == max(muacres)) %>%
    select(bhz_wt)
  
  
  dat <-
    bind_cols(iasr_maj, iasr_wgt,
              hzb_maj, hzb_wgt,
              draincl_major) %>%
    mutate(site = f_mychoice)
  
  return(dat)
}



# use function ------------------------------------------------------------


# use pre-downloaded ssurgo data ------------------------------------------

sll <- sad_siteinfo %>% 
  #--this is fixed in newer pkg, delete once it's updated
  mutate(site = str_sub(str_to_lower(site_name), 1, 4)) %>% 
  filter(site != "kell",
         site != "musc") %>% 
  rename(lon = long) %>%
  #--keep this
  select(site) %>% 
  pull()


mychoice <- sll[1]
mychoice

dat <- fun_summarize_ssurgo(f_mychoice = mychoice)


for (i in 2:length(sll) ) {
  
  mychoice <- sll[i]
  f_dat <- fun_summarize_ssurgo(f_mychoice = mychoice)
  
  dat <- bind_rows(dat, f_dat)
 
   i <- i + 1
}

dat %>% 
  select(site, everything()) %>% 
  write_csv("_data/td_ssurgo-vals.csv")
  

