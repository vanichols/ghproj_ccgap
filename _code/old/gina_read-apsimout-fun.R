library(tidyverse)
library(lubridate)
library(janitor)

ReadInApsimRes <- function(doifilter = "Y", iwant = iwant) {
  # read in out files created by apsim --------------------------------------
  
  # list the files w/out extensions (as a tibble that can be filtered)
  myrawouts <- list.files() %>%
    as_tibble() %>%
    filter(grepl("\\.out", value))
  
  
  if (doifilter == "Y"){
  myraws <-
    myrawouts %>%
    filter(value %in% iwant) %>%
    rename(file = value)} else {
      myraws <-
        myrawouts %>%
        rename(file = value)
    }
  
  
  ReadRawOutFile <- function(path) {
    suppressMessages({
      #path <- myraws[1,1] %>% pull()
      
      # Get names
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      myrawdatnames <- names(read_table(path, skip = 2))
      
      # Read the actual data
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      myrawdat <-
        read_table(path,
                   skip = 4,
                   na = "?",
                   col_names = myrawdatnames) %>%
        mutate(Date = dmy(Date),
               doy = yday(Date))
    })
  }
  
  dat <- myraws %>%
    mutate(path = paste0(file)) %>% 
    mutate(res = path %>% map(ReadRawOutFile)) %>%
    unnest(cols = c(res)) %>%
    clean_names()
  
  return(dat)
}
