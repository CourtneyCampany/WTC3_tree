source("functions and packages/gmes_functions.R")

###required packakges
library(data.table)
library(plyr)
library(doBy)

####read licor data and run licor formatting functions------------------------------------------------------------------
licor_master <- read.csv("raw data/gm_licor.csv")

####format master licor file, will have two dataframes one with all data, and one with times for matching
licor_gmes <- chooseidfunc(licor_master, c("campaign" , "chamber",  "leaf",  "light"))
licor_gmes <- licorformat_func(licor_gmes)

###now run time range function if get samples id and time range
licor_times <- timerange_func(licor_gmes)
