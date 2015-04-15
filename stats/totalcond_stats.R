###total conductance to leaf N

source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+Cond+gm+CTleaf ~ chamber+campaign+leaf +light+temp+leaflight+drydown, 
                    data=gmes, FUN=mean, keep.names=TRUE)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
gm_sunsha <- droplevels(gm_sunsha)

#add total conductance to CO2
gm_sunsha$gmgs <- with(gm_sunsha, gm+Cond)


### get leaf N by chamber
leafN <- read.csv("calculated_data/leaf_chemistry.csv")

#no drought
leafN2 <- leafN[leafN$drydown != "drought",]


##no drought
Ncond <- merge(gm_sunsha, leafN2)

plot(leafN_area~gmgs, data=Ncond, col=leaf, pch=16, xlim=c(0,.5), ylim=c(0,6))
plot(gmgs~leafN, data=Ncond, col=leaf, pch=16, xlim=c(0,.02), ylim=c(0,.6))
plot(gmgs~CTleaf, data=Ncond, col=leaf, pch=16, xlim=c(15,40), ylim=c(0,.6))

