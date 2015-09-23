##run energy balance model on E and gs

source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+Cond+VpdL+Trmmol+PARi+gm+CO2R+Tair+Ci+CTleaf ~ 
                      chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
gm_agg$leaf <- gsub("s", "S", gm_agg$leaf)
gm_agg$light <- gsub("high", "Sun-light", gm_agg$light)
gm_agg$light <- gsub("low", "Shade-light", gm_agg$light)

names(gm_agg)[10] <- "VPD"

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
gm_sunsha <- droplevels(gm_sunsha)

##sun-shade dataframes
sundat <- gm_sunsha[gm_sunsha$leaflight =="sun-high",]
shadat <- gm_sunsha[gm_sunsha$leaflight =="shade-low",]
##dfr with lights on
fleckdat <- gm_agg[gm_agg$leaflight == "shade-high",]
fleckdat <- droplevels(fleckdat)


shade_agg <- summaryBy(. ~ leaf, data=shadat, keep.names = TRUE)
####run plantecophys on leaf-type dataframes
shade_EB <- PhotosynEB(GS=shade_agg$Cond, Ca = shade_agg$CO2R,Tair=shade_agg$Tair)
                     
shade_EB <- PhotosynEB(GS=shadat$Cond, Ca = shadat$CO2R,Tair=shadat$Tair)                     

