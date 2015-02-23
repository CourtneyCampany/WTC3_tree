source("functions and packages/gmes_functions.R")

###required packakges
library(data.table)
library(plyr)
library(doBy)

####read licor data and run licor formatting functions------------------------------------------------------------------
licor_master <- read.csv("raw data/gm_licor.csv")

###remove pair ids for now (?) bind them back after formatting (should beed id, chamber)
pairs <- licor_master[, 2:7]
licor2 <- licor_master[, -7]

####format master licor file, will have two dataframes one with all data, and one with times for matching
licor_gmes <- chooseidfunc(licor2, c("campaign" , "chamber",  "leaf",  "light"))

licor_gmes <- licorformat_func(licor_gmes)

###now run time range function if get samples id and time range
licor_times <- timerange_func(licor_gmes)


####read all tdl files and run tdl formating and xsi functions on each list element-------------------------------------

  ####october
  oct_names<- list.files(path="tdl_files/october/",pattern="csv",full.names=TRUE)
  oct_names2 <- gsub("tdl_files/october/", "", oct_names)
  oct_names2 <- gsub(".csv", "", oct_names2)
  
  oct_files <- llply(list.files(path="tdl_files/october/",pattern="csv",full.names=TRUE),function(filename) {
   dat=read.csv(filename, header=TRUE)
  })

  oct_formatted <- llply(oct_files, tdlformat_func)

  xsi_oct <- llply(oct_formatted, function(x)  xsicalc_func(x))
  xsi_oct_dfr <- llply(xsi_oct, function(x) data.frame(x))
  #actual file names
  xsi_oct_dfr2 <- setNames(xsi_oct_dfr, oct_names2)

  ##for now run gmes functions for each licor #
  oct_H4 <- lapply(xsi_oct_dfr2, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=1, whichlicor="H4"))
  oct_gm <- lapply(oct_H4, gmcalc_func)


  ####december
  dec_names<- list.files(path="tdl_files/december",pattern="csv",full.names=TRUE)
  dec_names2 <- gsub("tdl_files/december/", "", dec_names)
  dec_names2 <- gsub(".csv", "", dec_names2)

  dec_files <- llply(list.files(path="tdl_files/december/",pattern="csv",full.names=TRUE),function(filename) {
    dat=read.csv(filename, header=TRUE)
  })

  dec_formatted <- llply(dec_files, tdlformat_func)

  xsi_dec <- llply(dec_formatted, function(x)  xsicalc_func(x))
  xsi_dec_dfr <- llply(xsi_dec, function(x) data.frame(x))
  #actual file names
  xsi_dec_dfr2 <- setNames(xsi_dec_dfr, dec_names2)

  ##for now run gmes functions for each licor #
  xsi_dec_h4 <- xsi_dec_dfr2[1:11] 
  xsi_dec_h2 <- xsi_dec_dfr2[11:12] 
  
  dec_h4 <- lapply(xsi_dec_h4, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=1, whichlicor="H4"))
  dec_h2 <- lapply(xsi_dec_h2, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=1, whichlicor="H2"))

  dec_gm_h4 <- lapply(dec_h4, gmcalc_func)
  dec_gm_h2 <- lapply(dec_h2, gmcalc_func)


  ####january
  jan_names<- list.files(path="tdl_files/january/",pattern="csv",full.names=TRUE)
  jan_names2 <- gsub("tdl_files/january/", "", jan_names)
  jan_names2 <- gsub(".csv", "", jan_names2)

  jan_files <- llply(list.files(path="tdl_files/january/",pattern="csv",full.names=TRUE),function(filename) {
    dat=read.csv(filename)})

  jan_formatted <- llply(jan_files, tdlformat_func)
  
  xsi_jan <- llply(jan_formatted, function(x)  xsicalc_func(x))
  xsi_jan_dfr <- llply(xsi_jan, function(x) data.frame(x))
  #actual file names
  xsi_jan_dfr2 <- setNames(xsi_jan_dfr, jan_names2)

  ##for now run gmes functions for each licor #
  xsi_jan_h2 <- xsi_jan_dfr[c(1, 5:12)] 
  xsi_jan_h3 <- xsi_jan_dfr[2:4] 

  jan_h2<- lapply(xsi_jan_h2, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=1, whichlicor="H2"))
  jan_h3 <- lapply(xsi_jan_h3, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=1, whichlicor="H3"))

  jan_gm_h2 <- lapply(jan_h2, gmcalc_func)
  jan_gm_h3 <- lapply(jan_h3, gmcalc_func)



  ####february (#####TIMES with ID are wrong  ch1 mathces ch6)

  feb_names<- list.files(path="tdl_files/february/",pattern="csv",full.names=TRUE)
  feb_names2 <- gsub("tdl_files/february/", "", feb_names)
  feb_names2 <- gsub(".csv", "", feb_names2)
  
  feb_files <- llply(list.files(path="tdl_files/february/",pattern="csv",full.names=TRUE),function(filename) {
    dat=read.csv(filename)})

  feb_formatted <- llply(feb_files, tdlformat_func)

  xsi_feb <- llply(feb_formatted, function(x)  xsicalc_func(x))
  xsi_feb_dfr <- llply(xsi_feb, function(x) data.frame(x))
  #actual file names
  xsi_feb_dfr2 <- setNames(xsi_feb_dfr, feb_names2)

  ##for now run gmes functions for each licor #
  xsi_feb_r1 <- xsi_feb_dfr2[c(1,4,7,8,11,12, 10)] 
  xsi_feb_h4 <- xsi_feb_dfr2[c(2,3,5,6,9,10)] 

  feb_r1<- lapply(xsi_feb_r1, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=3, whichlicor="R1"))
  feb_h4 <- lapply(xsi_feb_h4, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H4"))

  feb_gm_h2 <- lapply(feb_r1, gmcalc_func)
  feb_gm_h3 <- lapply(feb_h4, gmcalc_func)


zzz <- list()
for(i in 1:length(xsi_feb_r1)){
  zzz[[i]] <- gmesdata_func(xsi_feb_r1[[i]], licor_gmes, licor_times, licorrows=1,whichlicor="H4")
  message(i)
}


x <-  data.frame(feb_files[10])
x2 <-  data.frame(feb_formatted[1])
x3 <- data.frame(xsicalc_func(x2))
x4 <- gmesdata_func(x3, licor_gmes, licor_times, licorrows=5, whichlicor="H4")
x5 <- gmcalc_func(x4)

  ####march
  mar_names<- list.files(path="tdl_files/march/",pattern="csv",full.names=TRUE)
  mar_names2 <- gsub("tdl_files/march/", "", mar_names)
  mar_names2 <- gsub(".csv", "", mar_names2)
  
  mar_files <- llply(list.files(path="tdl_files/march/",pattern="csv",full.names=TRUE),function(filename) {
   dat=read.csv(filename)})

  mar_formatted <- llply(mar_files, tdlformat_func)

  xsi_mar <- llply(mar_formatted, function(x)  xsicalc_func(x))
  xsi_mar_dfr <- llply(xsi_mar, function(x) data.frame(x))

  #actual file names
  xsi_mar_dfr2 <- setNames(xsi_mar_dfr, mar_names2)

  ##for now run gmes functions for each licor #
  xsi_mar_h3 <- xsi_mar_dfr2[c(1,5,9)] 
  xsi_mar_h2 <- xsi_mar_dfr2[2] 
  xsi_mar_h1 <- xsi_mar_dfr2[c(7,8,12)] 
  xsi_mar_h4 <- xsi_mar_dfr2[c(3,4,6,10:11)] 

  mar_h3 <- lapply(xsi_mar_h3, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H3"))
  mar_h2 <- lapply(xsi_mar_h2, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H2")) 
  mar_h1 <- lapply(xsi_mar_h1, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H1"))
  mar_h4 <- lapply(xsi_mar_h4, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H4"))
  
  mar_gm_h3 <- lapply(mar_h3, gmcalc_func)
  mar_gm_h2 <- lapply(mar_h2, gmcalc_func)
  mar_gm_h1 <- lapply(mar_h1, gmcalc_func)
  mar_gm_h4 <- lapply(mar_h4, gmcalc_func)



  ####april
  apr_names<- list.files(path="tdl_files/april/",pattern="csv",full.names=TRUE)
  apr_names2 <- gsub("tdl_files/april/", "", apr_names)
  apr_names2 <- gsub(".csv", "", apr_names2)

  apr_files <- llply(list.files(path="tdl_files/april/",pattern="csv",full.names=TRUE),function(filename) {
   dat=read.csv(filename)})

  apr_formatted <- llply(apr_files, tdlformat_func)

  xsi_apr <- llply(apr_formatted, function(x)  xsicalc_func(x))
  xsi_apr_dfr <- llply(xsi_apr, function(x) data.frame(x))

  #actual file names
  xsi_apr_dfr2 <- setNames(xsi_apr_dfr, apr_names2)

  ##for now run gmes functions for each licor #
  xsi_apr_h1 <- xsi_apr_dfr2[c(5,8,12)] 
  xsi_apr_h3 <- xsi_apr_dfr2[c(1,2,4,10)] 
  xsi_apr_h4 <- xsi_apr_dfr2[c(3,6,7,9,11)] 

  apr_h1 <- lapply(xsi_apr_h1, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H1"))
  apr_h3 <- lapply(xsi_apr_h3, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H3"))
  apr_h4 <- lapply(xsi_apr_h4, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H4"))

  apr_gm_h1 <- lapply(apr_h1, gmcalc_func)
  apr_gm_h3 <- lapply(apr_h3, gmcalc_func)
  apr_gm_h4 <- lapply(apr_h4, gmcalc_func)








# xsi_dfr2 <- setNames(xsi_dfr, names2)
# list2env(lapply(xsi_dfr2, as.data.frame), .GlobalEnv)













