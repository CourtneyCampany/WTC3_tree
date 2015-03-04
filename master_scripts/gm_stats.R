source("master_scripts/plot_objects.R")

gmes <- read.csv("calculated_data/gmes_WTC.csv")

###for analysis first subset well watered and drought treatments
gm_drought <- gmes[gmes$drydown == "drought",]

gm_water <- gmes[gmes$drydown == "control",]
write.csv(gm_water, "calculated_data/gmes_wellwatered.csv", row.names=FALSE)

palette(c("blue", "red"))
###drought----------------------------------------------------------------------------------------------------

##plots
plot(Photo~gm, data=gm_drought, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0, .5))
  points(Photo~gm, data=gm_drought, subset=leaflight=="shade-low",pch=1, col=temp)
  legend("topright", templab, pch=16,inset = 0.03, col=palette()) 



###well watered------------------------------------------------------------------------------------------------

##sun-shade
plot(Photo~gm, data=gm_water, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0,.5), xlab=gmlab)
points(Photo~gm, data=gm_water, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)

##shade low-high
plot(Photo~gm, data=gm_water, subset=leaflight=="shade-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0,.5), xlab=gmlab)
points(Photo~gm, data=gm_water, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)


#leaf types
sun_gm <- gm_water[gm_water$leaf =="sun",]
shade_gm <- gm_water[gm_water$leaflight == "shade-low",]
lightson <- gm_water[gm_water$leaflight == "shade-high",]

#simple plots through time
plot(gm~campaign, data=sun_gm, pch=16, col=temp, ylim=c(0, .6), ylab="sun")
plot(gm~campaign, data=shade_gm, pch=16, col=temp, ylim=c(0, .6), ylab="shade")
plot(gm~campaign, data=lightson, pch=16, col=temp, ylim=c(0, .6), ylab="shade-high")


#means
gm_agg <- summaryBy(gm~ temp+leaf+light, data=gm_water,FUN=mean, keep.names=TRUE)
gm_drought_agg <- summaryBy(gm~ temp+leaf+light, data=gm_drought,FUN=mean, keep.names=TRUE)





