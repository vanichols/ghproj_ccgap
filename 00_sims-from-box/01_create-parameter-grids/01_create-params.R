# goal: build grid that has all parameter values in it
# created: 12/7/2020
# notes: derived from apsim folder, now on apsim2
# updated: 12/8/2020

rm(list = ls())

# use fernando's package
library(apsimx)
library(tidyverse)
library(readxl)
library(xml2)

#--getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
curdir <- paste(getwd())

curdir


#--list of params and defaults
parms <- 
  read_excel("../../parameter-defaults.xlsx") %>%
  fill(group_id) %>% 
  rename(group_desc = group_id) %>%  
  mutate(group_id = case_when(
    grepl("Kill", group_desc) ~ "kill",
    grepl("RUE", group_desc) ~ "rue",
    grepl("XF", group_desc) ~ "xf")) %>% 
  select(-group_desc, -notes) %>% 
  rename(value = default) 


parm_template <- 
  parms %>% 
  select(-param_id) %>% 
  unite(group_id, param_apsim, col = "parm") %>% 
  mutate(set = "defaults") %>% 
  pivot_wider(names_from = parm, values_from = value)


parm_template %>% 
  write_csv("R_template.csv")


# function to help --------------------------------------------------------

fun_SpecParm <-
  function(mypdf = parms,
           myp = 7,
           myv = "0, 0, 0, 0, 0") {
    
    tmp.parms <-
      mypdf %>%
      mutate(value = case_when(
        param_id == myp ~ myv,
        TRUE ~ value))
    
    return(tmp.parms)
  }


# create noscript set -------------------------------------------------------------

#--don't need params for script elimination - have to do that manually so just deleted them. 
#--this is just for reference/place-hodling
#--eliminate all scripts, set 1
### set 1 ----
set1 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 1,
         set_desc = "no scripts", 
         set_desc2 = "no scripts", 
         do_manually = "Y")




# create kill sets -------------------------------------------------------------

### set 1 ----
#--default values for kill, manually deleted other scripts
set2 <- 
  parms %>% 
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 2,
         set_desc = "kill", 
         set_desc2 = "kill defaults",
         do_manually = "N")

### set 3 ----
#--eliminate dry killing of plants, kill LOTS for mois
set3 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0.3") %>% #--no mois kill at dry end, a ton at wet end
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 3,
         set_desc = "kill", 
         set_desc2 = "no dry kill, 30% wet 1% cold",
         do_manually = "N")

### set 4----
#--eliminate dry killing of plants
set4 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0.03") %>% #--no mois kill at dry end
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 4,
         set_desc = "kill", 
         set_desc2 = "no dry kill, 3% wet 1% cold",
         do_manually = "N")

#--set5, exaggerate wet killing of plants, no dry killing
set5 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0.1") %>% #--no dry kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 5,
         set_desc = "kill", 
         set_desc2 = "no dry kill, 10% wet 1% cold",
         do_manually = "N")

#--set6, eliminate temperature killing
set6 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--no temp kill (only mois kill)
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 6,
         set_desc = "kill", 
         set_desc2 = "only mois kill (dry+wet)",
         do_manually = "N")

#--set7, eliminate temperature killing and dry kill
set7 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0.03") %>% #--no dry kill
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill 
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 7,
         set_desc = "kill", 
         set_desc2 = "only wet kill, 3%",
         do_manually = "N")

#--set8, no mois kill
set8 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 8,
         set_desc = "kill", 
         set_desc2 = "only cold kill, 10%",
         do_manually = "N")


#--set9, big cold kill
set9 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill
  fun_SpecParm(., 9, "0.3, 0") %>% #--big cold kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 9,
         set_desc = "kill", 
         set_desc2 = "only cold kill, 30%",
         do_manually = "N")

#--set10, only dry kill
set10 <- 
  fun_SpecParm(parms, 7, "0.01, 0, 0, 0, 0") %>% #--no mois kill
  fun_SpecParm(., 9, "0, 0") %>% #--no cold kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 10,
         set_desc = "kill", 
         set_desc2 = "only dry kill, 1%",
         do_manually = "N")

#--set11, only dry kill big
set11 <- 
  fun_SpecParm(parms, 7, "0.1, 0, 0, 0, 0") %>% #--no mois kill, 10% dry kill
  fun_SpecParm(., 9, "0, 0") %>% #--no cold kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 11,
         set_desc = "kill", 
         set_desc2 = "only dry kill, 10%",
         do_manually = "N")

## combine kill sets ----
#--start add it to the list

kill_parms <- 
  set1 %>% 
  bind_rows(set2) %>% 
  bind_rows(set3) %>% 
  bind_rows(set4) %>% 
  bind_rows(set5) %>%
  bind_rows(set6) %>% 
  bind_rows(set7) %>% 
  bind_rows(set8) %>% 
  bind_rows(set9) %>% 
  bind_rows(set10) %>% 
  bind_rows(set11) 


# create rue sets -------------------------------------------------------------
parms %>% filter(group_id == "rue")

#--default values for rue, manually deleted all other scripts
set100 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 100,
         set_desc = "rue", 
         set_desc2 = "rue defaults", 
         do_manually = "N")

### set 101
#--only res pen
set101 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 101,
         set_desc = "rue", 
         set_desc2 = "res pen only, 4%", 
         do_manually = "N")

### set 102 ----
#--only mois pen
set102 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "0.97, 1, 0.94") %>% #--def mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 102,
         set_desc = "rue", 
         set_desc2 = "mois pen only, 3(dry)% 6(wet)%", 
         do_manually = "N")

### set 103 ----
#--only cold pen
set103 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 0.96, 0.92") %>% #--def temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 103,
         set_desc = "rue", 
         set_desc2 = "cold pen only, 4(mod)% 8(sev)%", 
         do_manually = "N")


### set 104 ----
#--only res, start at 1000
set104 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 15, "1, 1000, 4000") %>% #--res rue
  fun_SpecParm(., 16, "1, 1, 0.96") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 104,
         set_desc = "rue", 
         set_desc2 = "res pen only, 1000-4000, 4%max", 
         do_manually = "N")

### set 105 ----
#--only res, start at 1000, 10% max
set105 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 15, "1, 1000, 4000") %>% #--res rue
  fun_SpecParm(., 16, "1, 1, 0.9") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 105,
         set_desc = "rue", 
         set_desc2 = "res pen only, 1000-4000, 10%max", 
         do_manually = "N")


### set 106 ----
#--only cold, set base temp to 10
set106 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 14, "10") %>% #--coldTT rue
  fun_SpecParm(., 15, "1, 1000, 4000") %>% #--res rue
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 0.96, 0.92") %>% #--def temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 106,
         set_desc = "rue", 
         set_desc2 = "inc coldTT base from 4 to 10", 
         do_manually = "N")



rue_parms <- 
  set100 %>% 
  bind_rows(set101) %>% 
  bind_rows(set102) %>% 
  bind_rows(set103) %>% 
  bind_rows(set104) %>% 
  bind_rows(set105) %>% 
  bind_rows(set106)


# create xf sets -------------------------------------------------------------
parms %>% filter(group_id == "xf")

### set 200 ----
#--default values for xf, manually deleted all other scripts
set200 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  mutate(set_id = 200,
         set_desc = "xf", 
         set_desc2 = "xf defaults", 
         do_manually = "N")

### set 201 ----
#--only res pen
set201 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 26, "0, 1000, 4000") %>% #--def res xf
  fun_SpecParm(., 27, "1, 1, 0.60") %>% #--def res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 201,
         set_desc = "xf", 
         set_desc2 = "res pen only, 1000-4000 40%", 
         do_manually = "N")

### set 202 ----
#--only wet mois pen
set202 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 0.60") %>% #--def mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 202,
         set_desc = "xf", 
         set_desc2 = "wet mois pen only, 40%", 
         do_manually = "N")

### set 203 ----
#--only dry mois pen
set203 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "0.6, 1, 1") %>% #--def mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 203,
         set_desc = "xf", 
         set_desc2 = "dry mois pen only, 40%", 
         do_manually = "N")


### set 204 ----
#--only mois
set204 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "0.60, 1, 0.60") %>% #--big mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no temp xf
  mutate(set_id = 204,
         set_desc = "xf", 
         set_desc2 = "wet+dry mois pen only, 40%", 
         do_manually = "N")

### set 205 ----
#--cold only
set205 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 31, "1, 0.60, 0.50") %>% #--def cold xf
  mutate(set_id = 205,
         set_desc = "xf", 
         set_desc2 = "cold pen only (40-50%)", 
         do_manually = "N")

### set 206 ----
#--cold only, new coldTT hi pen
set206 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--no mois xf
  fun_SpecParm(., 25, "10") %>% #--new coldTT
  fun_SpecParm(., 31, "1, 0.40, 0.50") %>% #--strong cold xf
  mutate(set_id = 206,
         set_desc = "xf", 
         set_desc2 = "cold pen only, 10 coldTT 40-50%", 
         do_manually = "N")

### set 207 ----
#--mois only, start before dul
set207 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 28, "0, 0.9, 2") %>% #--new mois xf
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--new mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no cold xf
  mutate(set_id = 207,
         set_desc = "xf", 
         set_desc2 = "mois pen start <dul, 50%", 
         do_manually = "N")


### set 208 ----
#--mois only, start before dul
set208 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 28, "0, 0.75, 2") %>% #--new mois xf
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--new mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no cold xf
  mutate(set_id = 208,
         set_desc = "xf", 
         set_desc2 = "mois pen start <<dul, 50%", 
         do_manually = "N")


### set 209 ----
#--just dec xf to 0.5 all the time
set209 <- 
  fun_SpecParm(parms, 7, "0, 0, 0, 0, 0") %>% #--no mois kill 
  fun_SpecParm(., 9, "0, 0") %>% #--no temp kill
  fun_SpecParm(., 16, "1, 1, 1") %>% #--no res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--no mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--no temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--no res xf
  fun_SpecParm(., 28, "0, 0.75, 2") %>% #--new mois xf
  fun_SpecParm(., 29, "0.5, 0.5, 0.5") %>% #--new mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--no cold xf
  mutate(set_id = 209,
         set_desc = "xf", 
         set_desc2 = "xf always dec 50%", 
         do_manually = "N")



xf_parms <- 
  set200 %>% 
  bind_rows(set201) %>% 
  bind_rows(set202) %>% 
  bind_rows(set203) %>% 
  bind_rows(set204) %>% 
  bind_rows(set205) %>% 
  bind_rows(set206) %>% 
  bind_rows(set207) %>% 
  bind_rows(set208) %>% 
  bind_rows(set209)


# combine all sets and write--------------------------------------------------------

all_parms <- 
  kill_parms %>% 
  bind_rows(rue_parms) %>% 
  bind_rows(xf_parms)

all_parms %>% write_csv("01_set-params.csv")

all_parms %>% 
  filter(set_id == 106)
