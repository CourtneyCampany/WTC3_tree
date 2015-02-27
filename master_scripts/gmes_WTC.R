source("functions and packages/gmes_functions.R")
source("functions and packages/functions.R")

###required packakges
library(data.table)
library(plyr)
library(doBy)

##read in treatments
treatments <- read.csv("raw data/temp_trt.csv")

####read licor data and run licor formatting functions------------------------------------------------------------------
licor_master <- read.csv("raw data/gm_licor.csv")
licor_master <- chlab_func(licor_master)

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



  ####february
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
  xsi_feb_r1 <- xsi_feb_dfr2[c(1,4,7,8,11,12)] 
  xsi_feb_h4 <- xsi_feb_dfr2[c(2,3,5,6,9,10)] 

  feb_r1<- lapply(xsi_feb_r1, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=3, whichlicor="R1"))
  feb_h4 <- lapply(xsi_feb_h4, function(x) gmesdata_func(x, licor_gmes, licor_times, licorrows=5, whichlicor="H4"))

  feb_gm_r1 <- lapply(feb_r1, gmcalc_func)
  feb_gm_h4 <- lapply(feb_h4, gmcalc_func)

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



#### combine all gm runs into one dfr
oct <- rbind.fill(oct_gm)
dec1 <- rbind.fill(dec_gm_h2)
dec2 <- rbind.fill(dec_gm_h4)
jan1 <- rbind.fill(jan_gm_h2)
jan2 <- rbind.fill(jan_gm_h3)
feb1 <- rbind.fill(feb_gm_r1)
feb2 <- rbind.fill(feb_gm_h4)
mar1 <- rbind.fill(mar_gm_h1)
mar2 <- rbind.fill(mar_gm_h2)
mar3 <- rbind.fill(mar_gm_h3)
mar4 <- rbind.fill(mar_gm_h4)
apr1 <- rbind.fill(apr_gm_h1)
apr2 <- rbind.fill(apr_gm_h3)
apr3 <- rbind.fill(apr_gm_h4)

##testing

gm_WTC <- rbind.fill(oct, dec1)
gm_WTC <- rbind.fill(gm_WTC, dec2)
gm_WTC <- rbind.fill(gm_WTC, jan1)
gm_WTC <- rbind.fill(gm_WTC, jan2)
gm_WTC <- rbind.fill(gm_WTC, feb1)
gm_WTC <- rbind.fill(gm_WTC, feb2)
gm_WTC <- rbind.fill(gm_WTC, mar1)
gm_WTC <- rbind.fill(gm_WTC, mar2)
gm_WTC <- rbind.fill(gm_WTC, mar3)
gm_WTC <- rbind.fill(gm_WTC, mar4)
gm_WTC <- rbind.fill(gm_WTC, apr1)
gm_WTC <- rbind.fill(gm_WTC, apr2)
gm_WTC <- rbind.fill(gm_WTC, apr3)

###add back pair ids
pairs2<- chooseidfunc(pairs, c("campaign" , "chamber",  "leaf",  "light"))

gm_wtc_pair <- merge(gm_WTC, pairs2[,c(1:4, 6:7)], by="id")
##add treatments
gm_wtc_pair <- merge(gm_wtc_pair, treatments)

#write.csv(gm_wtc_pair, "calculated_data/gmes_wtc.csv", row.names=FALSE)

#summary
gm_agg <- summaryBy(gm~ temp+leaf+light, data=gm_wtc_pair,FUN=mean, keep.names=TRUE)




####useful code to save
# xsi_dfr2 <- setNames(xsi_dfr, names2)
# list2env(lapply(xsi_dfr2, as.data.frame), .GlobalEnv)


# zzz <- list()
# for(i in 1:length(xsi_feb_h4)){
#   zzz[[i]] <- gmesdata_func(xsi_feb_h4[[i]], licor_gmes, licor_times, licorrows=5,whichlicor="H4")
#   message(i)
# }











