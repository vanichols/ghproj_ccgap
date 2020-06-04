# Created:       4/1/2020
# 
# purpose: stats on pct gap (rather than raw gap)
#
# notes: 
#
# last edited:   4/23/2020
#                5/20/2020 moved to 02_fit-models folder
#               5/26/2020 use loo imp variables instead
#               5/29/2020 do it on raw gaps instad of %
#               6/4/2020 do a simple lm

rm(list = ls())
library(tidyverse)


# data --------------------------------------------------------------------

datraw <- read_csv("01_create-features/cf_preds-all.csv")


 