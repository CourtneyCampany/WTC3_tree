source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
library(doBy)
library(scales)

###read data--------------------------------------------------------------------------------------------

#gas exchange data
photo <- read.csv("raw data/gm_licor.csv")
#plot summary
treatments <- read.csv("raw data/temp_trt.csv")
#read in leaf data
leaf_chem <- read.csv("raw data/leaf_chem.csv")


###format gas exchange data and get mean by id-----------------------------------------------------------

##remove shade high light treatment
photo$leaflight <- as.factor(paste(photo$leaf,photo$light, sep="-"))

ci <- photo[photo$leaflight == "sun-high" | photo$leaflight == "shade-low", c(2:5,13, 24,63) ]
  
#add months and treatments
  ci$campaign <- as.factor(ci$campaign)
  ci<- chlab_func(ci)
  ci<- add_Month(ci)
  ci<- addtrt_func(ci)
  ci<- droplevels(ci)

##make unique id for merge with delta data
  ci<- chooseidfunc(ci, c("campaign", "chamber",  "leaf"))

###this now has mean of values of sun-high and shade-low
ci_agg <- summaryBy(Ci+ CO2R ~ id, data=ci, FUN=mean, keep.names=TRUE)


###format delta c12 data----------------------------------------------------------------------------------
deltaC <- leaf_chem[,4:8]
deltaC <- add_campaign(deltaC)
deltaC<- chooseidfunc(deltaC, c("campaign", "chamber",  "leaf"))

ci_bar <- merge(ci_agg, deltaC[,c(1,7)], all=TRUE)

ci_bar_calc <- function (x, a=4.4, b=29, c13_source=-8){
  x$D <- (c13_source-x$c13)/(1+(c13_source/1000))
  x$ci_bar <- (x$CO2R *((x$D-a) / (b-a)))
  x$leaf <- as.factor(substring(x$id, 8))
  x$campaign <- as.factor(substring(x$id, 1, 1))
  x$chamber <- as.factor(substring(x$id, 3, 6))
  return(x)
}

ci_bar2 <- ci_bar_calc(ci_bar)




