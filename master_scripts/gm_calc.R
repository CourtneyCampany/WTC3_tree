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


####read all tdl files and run tdl formating and xsi functions on each list element-------------------------------------
names<- list.files(path="tdl_files/",pattern="csv",full.names=TRUE)

tdl_files <- llply(list.files(path="tdl_files/",pattern="csv",full.names=TRUE),function(filename) {
  dat=read.csv(filename)
})


###start here



####format tdl data (will be csv covering a day and a ref/sample line with samples within)
tdl_formatted <- llply(tdl_files, tdlformat_func)

####calculatre xsi/Delta with times by licor id
xsi_face <- llply(tdl_formatted, function(x)  xsicalc_func(x))
xsi_dfr <- llply(xsi_face, function(x) data.frame(x))

####name each list by filename and export to global environment as dfr
names2 <- gsub("tdl_files/", "", names)
names2 <- gsub(".csv", "", names2)

xsi_dfr2 <- setNames(xsi_dfr, names2)
list2env(lapply(xsi_dfr2, as.data.frame), .GlobalEnv