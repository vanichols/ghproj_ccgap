# goal: build grid that has all parameter values in it
# created: 12/15/2020
# notes: more targeted changes compared to round1
# updated:12/16/2020 changed set_desc and set_desc2 etc. to more intuitive things

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
         set_script = "no scripts", 
         set_pdes = "no scripts", 
         set_pcat = "no scripts",
         do_manually = "Y")




# create kill sets -------------------------------------------------------------

### set 2 ----
#--only wet kill
set2 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.5, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0, 0, 0, 0.03") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 2,
         set_script = "kill", 
         set_pdes = "1.5-2 wet kill, 3%",
         set_pcat = "wet",
         do_manually = "N")

### set 3 ----
#--only wet kill
set3 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.25, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0, 0, 0, 0.03") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 3,
         set_script = "kill", 
         set_pdes = "1.25-2 wet kill, 3%",
         set_pcat = "wet",
         do_manually = "N")

### set4 ----
#--only wet kill
set4 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.5, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0, 0, 0, 0.10") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 4,
         set_script = "kill", 
         set_pdes = "1.5-2 wet kill, 10%",
         set_pcat = "wet",
         do_manually = "N")

### set5 ----
#--only dry kill
set5 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.5, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0.01, 0, 0, 0") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 5,
         set_script = "kill", 
         set_pdes = "0-0.5 dry kill, 1%",
         set_pcat = "dry",
         do_manually = "N")


### set6 ----
#--only dry kill
set6 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.5, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0.1, 0, 0, 0") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 6,
         set_script = "kill", 
         set_pdes = "0-0.5 dry kill, 10%",
         set_pcat = "dry",
         do_manually = "N")

### set7 ----
#--only cold kill
set7 <- 
  fun_SpecParm(parms, 8, "-10, -3.5, 0") %>% #--temp kill soil temperature
  fun_SpecParm(., 9, "0.1, 0.1, 0") %>% #--temp kill pct plants
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 7,
         set_script = "kill", 
         set_pdes = "-3.5-0 cold kill, 10%",
         set_pcat = "cold",
         do_manually = "N")

### set8 ---- (THIS BREAKS NASHUA)
#--only cold kill
set8 <- 
  fun_SpecParm(parms, 8, "-10, -3.5, 0") %>% #--temp kill soil temperature
  fun_SpecParm(., 9, "0.2, 0.2, 0") %>% #--temp kill pct plants
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 8,
         set_script = "kill", 
         set_pdes = "-3.5-0 cold kill, 20%",
         set_pcat = "cold",
         do_manually = "N")

### set9 ----
#--only wet kill
set9 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.5, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0, 0, 0, 0.05") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 9,
         set_script = "kill", 
         set_pdes = "1.5-2 wet kill, 5%",
         set_pcat = "wet",
         do_manually = "N")

### set10 ----
#--flat kill rate
set10 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.5, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0.05, 0.05, 0.05, 0.05") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 10,
         set_script = "kill", 
         set_pdes = "5% kill all the time",
         set_pcat = "flat",
         do_manually = "Y")

### set11 ----
#--flat kill rate
set11 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.5, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0.01, 0.01, 0.01, 0.01") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 11,
         set_script = "kill", 
         set_pdes = "1% kill all the time",
         set_pcat = "flat",
         do_manually = "Y")

### set12 ----
#--flat kill rate
set12 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1.5, 2") %>% #--mois kill soil moist
  fun_SpecParm(., 7, "0.02, 0.02, 0.02, 0.02") %>% #--mois kill pct plants
  fun_SpecParm(., 9, "0, 0") %>% #--cold kill pct plants
  fun_SpecParm(., 16, "1, 1, 1") %>% #--res rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp rue
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  mutate(set_id = 12,
         set_script = "kill", 
         set_pdes = "2% kill all the time",
         set_pcat = "flat",
         do_manually = "Y")

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
  bind_rows(set11) %>% 
  bind_rows(set12)

# create rue sets -------------------------------------------------------------
parms %>% filter(group_id == "rue")

### set 101----
#--only res pen
set101 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  #--rue
  fun_SpecParm(., 15, "0, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.96") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  
  mutate(set_id = 101,
         set_script = "rue", 
         set_pdes = "2.5-10 res pen, 4%", 
         set_pcat = "res",
         do_manually = "N")

### set 102----
#--only res pen
set102 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  #--rue
  fun_SpecParm(., 15, "0, 500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.96") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  
  mutate(set_id = 102,
         set_script = "rue", 
         set_pdes = "0.5-10 res pen, 4%", 
         set_pcat = "res",
         do_manually = "N")

### set 103----
#--only res pen
set103 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  #--rue
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.96, 0.96") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  
  mutate(set_id = 103,
         set_script = "rue", 
         set_pdes = "0.5-2.5, 10 res pen, 4%", 
         set_pcat = "res",
         do_manually = "N")

### set 104----
#--only res pen
set104 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  #--rue
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  mutate(set_id = 104,
         set_script = "rue", 
         set_pdes = "0.5-2.5, 10 res pen, 10%", 
         set_pcat = "res",
         do_manually = "N")


### set 105----
#--only res pen
set105 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  #--rue
  fun_SpecParm(., 15, "0, 500, 5000, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  mutate(set_id = 105,
         set_script = "rue", 
         set_pdes = "0.5-5, 10 res pen, 10%", 
         set_pcat = "res",
         do_manually = "N")


### set 106----
#--flat rue pen, 10%
set106 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  #--rue
  fun_SpecParm(., 15, "0, 500, 5000, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "0.9, 0.9, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  mutate(set_id = 106,
         set_script = "rue", 
         set_pdes = "10% rue red all the time", 
         set_pcat = "flat",
         do_manually = "Y")

### set 107----
#--flat rue pen, 5%
set107 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf
  #--rue
  fun_SpecParm(., 15, "0, 500, 5000, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "0.95, 0.95, 0.95, 0.95") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  mutate(set_id = 107,
         set_script = "rue", 
         set_pdes = "5% rue red all the time", 
         set_pcat = "flat",
         do_manually = "Y")

## combine rue sets ----
#--start add it to the list

rue_parms <- 
  set1 %>% 
  bind_rows(set101) %>% 
  bind_rows(set102) %>% 
  bind_rows(set103) %>% 
  bind_rows(set104) %>% 
  bind_rows(set105) %>% 
  bind_rows(set106) %>% 
  bind_rows(set107)


# xf ----------------------------------------------------------------------

parms %>% filter(group_id == "xf")

### set 201----
#--only wet pen
set201 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.95") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 15, 100") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf, y
  mutate(set_id = 201,
         set_script = "xf", 
         set_pdes = "1-2, wet pen 5%", 
         set_pcat = "wet",
         do_manually = "N")


### set 202----
#--only wet pen
set202 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.90") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 15, 100") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf, y
  mutate(set_id = 202,
         set_script = "xf", 
         set_pdes = "1-2, wet pen 10%", 
         set_pcat = "wet",
         do_manually = "N")

### set 203----
#--only wet pen
set203 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.50") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 15, 100") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf, y
  mutate(set_id = 203,
         set_script = "xf", 
         set_pdes = "1-2, wet pen 50%", 
         set_pcat = "wet",
         do_manually = "N")



### set 204----
#--only cold pen
set204 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf, y
  fun_SpecParm(., 30, "0, 5, 15") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.9, 0.9") %>% #--temp xf, y
  mutate(set_id = 204,
         set_script = "xf", 
         set_pdes = "0-5, 15, cold pen 10%", 
         set_pcat = "cold",
         do_manually = "N")


### set 205----
#--only cold pen
set205 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf, y
  fun_SpecParm(., 30, "0, 5, 15") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.8, 0.8") %>% #--temp xf, y
  mutate(set_id = 205,
         set_script = "xf", 
         set_pdes = "0-5, 15, cold pen 20%", 
         set_pcat = "cold",
         do_manually = "N")


### set 206----
#--only cold pen
set206 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf, y
  fun_SpecParm(., 30, "0, 10, 15") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.8, 0.8") %>% #--temp xf, y
  mutate(set_id = 206,
         set_script = "xf", 
         set_pdes = "0-10, 15, cold pen 20%", 
         set_pcat = "cold",
         do_manually = "N")

### set 207----
#--only cold pen
set207 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf, y
  fun_SpecParm(., 30, "0, 5, 15") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 207,
         set_script = "xf", 
         set_pdes = "0-5, 15, cold pen 50%", 
         set_pcat = "cold",
         do_manually = "N")


### set 208----
#--flat xf pen 50%
set208 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "0.5, 0.5, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "0, 5, 15") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf, y
  mutate(set_id = 208,
         set_script = "xf", 
         set_pdes = "constant xf pen 50%", 
         set_pcat = "flat",
         do_manually = "N")


### set 209----
#--flat xf pen 20%
set209 <- 
  fun_SpecParm(parms, 9, "0, 0") %>% #--temp kill
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois kill 
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  #--xf
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "0.8, 0.8, 0.8") %>% #--mois xf, y
  fun_SpecParm(., 30, "0, 5, 15") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf, y
  mutate(set_id = 209,
         set_script = "xf", 
         set_pdes = "constant xf pen 20%", 
         set_pcat = "flat",
         do_manually = "N")


## combine xf sets ----
#--start add it to the list

xf_parms <- 
  set201 %>% 
  bind_rows(set202) %>% 
  bind_rows(set203) %>% 
  bind_rows(set204) %>% 
  bind_rows(set205) %>% 
  bind_rows(set206) %>% 
  bind_rows(set207) %>% 
  bind_rows(set208) %>% 
  bind_rows(set209)

# combos ----------------------------------------------------------------------

parms %>% filter(group_id == "rue")

### set 2001----
#--all
set2001 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2001,
         set_script = "combo", 
         set_pdes = "all dyn (resrue10%)", 
         set_pcat = "combo",
         do_manually = "Y")


### set 2002----
#--all
set2002 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.95, 0.95") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2002,
         set_script = "combo", 
         set_pdes = "all dyn (resrue5%)", 
         set_pcat = "combo",
         do_manually = "Y")


### set 2003----
#--all, not rue
set2003 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 1, 1") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2003,
         set_script = "combo", 
         set_pdes = "dyn kill + dyn xf", 
         set_pcat = "combo",
         do_manually = "Y")

### set 2004----
#--all
set2004 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.95, 0.95") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2004,
         set_script = "combo", 
         set_pdes = "dyn kill, dyn 5%rue, dyn50%xf", 
         set_pcat = "combo",
         do_manually = "Y")


### set 2005----
#--all
set2005 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.95, 0.95") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2005,
         set_script = "combo", 
         set_pdes = "no cold kill, 5rue%", 
         set_pcat = "combo",
         do_manually = "Y")


### set 2006----
#--all
set2006 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.95, 0.95") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2006,
         set_script = "combo", 
         set_pdes = "no wet xf, 5%rue", 
         set_pcat = "combo",
         do_manually = "Y")


### set 2007----
#--all
set2007 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.95, 0.95") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2007,
         set_script = "combo", 
         set_pdes = "no cold xf, 5%rue", 
         set_pcat = "combo",
         do_manually = "Y")

### set 2008----
#--all
set2008 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2008,
         set_script = "combo", 
         set_pdes = "no wet kill, 10%rue", 
         set_pcat = "combo",
         do_manually = "Y")


### set 2009----
#--all
set2009 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2009,
         set_script = "combo", 
         set_pdes = "no cold kill, 10rue%", 
         set_pcat = "combo",
         do_manually = "Y")


### set 2010----
#--all
set2010 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 1") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2010,
         set_script = "combo", 
         set_pdes = "no wet xf, 10%rue", 
         set_pcat = "combo",
         do_manually = "Y")


### set 2011----
#--all
set2011 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "1, 1, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 0.5, 0.5") %>% #--temp xf, y
  mutate(set_id = 2011,
         set_script = "combo", 
         set_pdes = "no cold xf, 10%rue", 
         set_pcat = "combo",
         do_manually = "Y")

### set 2012----
#--all
set2012 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.95, 0.95") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "0.5, 0.5, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf, y
  mutate(set_id = 2012,
         set_script = "combo", 
         set_pdes = "dyn kill, dyn 5%rue, dyn50%xf", 
         set_pcat = "combo",
         do_manually = "Y")

### set 2013----
#--all
set2013 <- 
  fun_SpecParm(parms, 6, "0, 0.5, 1, 1.5, 2") %>% #--mois x kill 
  fun_SpecParm(., 7, "0, 0, 0, 0, 0.05") %>% #--mois y kill 
  fun_SpecParm(., 8, "-3.5, 0") %>% #--temp x kill 
  fun_SpecParm(., 9, "0.1, 0") %>% #--temp y kill
  fun_SpecParm(., 15, "0, 500, 2500, 10000") %>% #--res x rue
  fun_SpecParm(., 16, "1, 1, 0.9, 0.9") %>% #--res y rue
  fun_SpecParm(., 17, "0, 1, 2") %>% #--mois x rue
  fun_SpecParm(., 18, "1, 1, 1") %>% #--mois y rue
  fun_SpecParm(., 19, "0, 15, 100") %>% #--temp x rue
  fun_SpecParm(., 20, "1, 1, 1") %>% #--temp y rue
  fun_SpecParm(., 26, "0, 2500, 10000") %>% #--res xf
  fun_SpecParm(., 27, "1, 1, 1") %>% #--res xf
  fun_SpecParm(., 28, "0, 1, 2") %>% #--mois xf, x
  fun_SpecParm(., 29, "0.5, 0.5, 0.5") %>% #--mois xf, y
  fun_SpecParm(., 30, "1, 5, 20") %>% #--temp xf, x
  fun_SpecParm(., 31, "1, 1, 1") %>% #--temp xf, y
  mutate(set_id = 2013,
         set_script = "combo", 
         set_pdes = "all, resrue10%, xf50%", 
         set_pcat = "combo",
         do_manually = "Y")

combo_parms <- 
  set2001 %>% 
  bind_rows(set2002) %>% 
  bind_rows(set2003) %>% 
  bind_rows(set2004) %>% 
  bind_rows(set2005) %>% 
  bind_rows(set2006) %>% 
  bind_rows(set2007) %>% 
  bind_rows(set2008) %>% 
  bind_rows(set2009) %>% 
  bind_rows(set2010) %>% 
  bind_rows(set2011) %>% 
  bind_rows(set2012) %>% 
  bind_rows(set2013)

# combine all sets and write--------------------------------------------------------

all_parms <- 
  kill_parms %>% 
  bind_rows(rue_parms) %>%
  bind_rows(xf_parms) %>% 
  bind_rows(combo_parms) %>% 
  mutate(set_id = ifelse(set_id > 1 & set_id < 2000, set_id + 1000, set_id))

all_parms %>% write_csv("01_set-params-round2.csv")
