source("functions and packages/functions.R")
library(doBy)

gasexchange  <- read.csv("calculated_data/gmes_wellwatered.csv")

###leaf data
ge_agg <- summaryBy(Photo+Cond+Ci+Trmmol+gm+VpdL ~ chamber+id+leaf +light+temp+leaflight+Month+drydown, 
                    data=gasexchange, FUN=mean, keep.names=TRUE)
ge_agg$tukeyid <- as.factor(paste(ge_agg$leaflight, ge_agg$temp, sep="-"))

###linear models of gas exchange by treatments
library(visreg)
library(multcomp)
library(nlme)

#Photosynthesis
photo_agg <- summaryBy(Photo+Cond+gm ~ leaf, data=ge_agg[ge_agg$leaflight == "shade-high",] , FUN=c(mean,se))
