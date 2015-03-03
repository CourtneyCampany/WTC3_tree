

source("functions and packages/gmes_functions.R")
source("functions and packages/functions.R")

###required packakges
library(data.table)
library(plyr)
library(doBy)

##read in treatments
treatments <- read.csv("raw data/temp_trt.csv")

####read licor data and run licor formatting functions------------------------------------------------------------------
licor_master <- read.csv("tdl_files/april/licor_april.csv")
licor_master <- chlab_func(licor_master)

###remove pair ids for now (?) bind them back after formatting (should beed id, chamber)
pairs <- licor_master[, 2:7]
licor2 <- licor_master[, -7]

####format master licor file, will have two dataframes one with all data, and one with times for matching
licor_gmes <- chooseidfunc(licor2, c("campaign" , "chamber",  "leaf",  "light"))

licor_gmes <- licorformat_func(licor_gmes)

###now run time range function if get samples id and time range
licor_times <- timerange_func(licor_gmes)



####april
apr_names<- list.files(path="tdl_files/april/",pattern="tdl",full.names=TRUE)
apr_names2 <- gsub("tdl_files/april/", "", apr_names)
apr_names2 <- gsub(".csv", "", apr_names2)

apr_files <- llply(list.files(path="tdl_files/april/",pattern="tdl",full.names=TRUE),function(filename) {
  dat=read.csv(filename)})

apr_formatted <- llply(apr_files, tdlformat_func)

# test <- read.csv("tdl_files/april/tdl_apr_ch8.csv")
# test2 <-tdlformat_func(test)
# test3 <- xsicalc_func(test2)
# test4<- data.frame(test3)
# test5 <- gmesdata_func(test4, licor_gmes, licor_times, licorrows=5, whichlicor="H4")

# test <- read.csv("tdl_files/april/tdl_apr_ch11.csv")
# test2 <-tdlformat_func(test)
# test3 <- xsicalc_func(test2)
# test4<- data.frame(test3)
# test5 <- gmesdata_func(test4, licor_gmes, licor_times, licorrows=5, whichlicor="H4")
# test6 <- gmcalc_func(test5)

xsi_apr <- llply(apr_formatted, function(x)  xsicalc_func(x))
xsi_apr_dfr <- llply(xsi_apr, function(x) data.frame(x))

#actual file names
xsi_apr_dfr2 <- setNames(xsi_apr_dfr, apr_names2)

##for now run gmes functions for each licor #
xsi_apr_h1 <- xsi_apr_dfr2[c(5,8,12)] 
xsi_apr_h3 <- xsi_apr_dfr2[c(1,2,4,6,7,10)] 
xsi_apr_h4 <- xsi_apr_dfr2[c(3,9,11)]


apr_h1 <- lapply(xsi_apr_h1, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H1"))
apr_h3 <- lapply(xsi_apr_h3, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H3"))
apr_h4 <- lapply(xsi_apr_h4, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H4"))

apr_gm_h1 <- lapply(apr_h1, gmcalc_func)
apr_gm_h3 <- lapply(apr_h3, gmcalc_func)
apr_gm_h4 <- lapply(apr_h4, gmcalc_func)

apr1 <- rbind.fill(apr_gm_h1)
apr2 <- rbind.fill(apr_gm_h3)
apr3 <- rbind.fill(apr_gm_h4)





