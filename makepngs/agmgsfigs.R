source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ Cond + gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
gm_agg$leaf <- gsub("s", "S", gm_agg$leaf)
gm_agg$light <- gsub("high", "Sun-light", gm_agg$light)
gm_agg$light <- gsub("low", "Shade-light", gm_agg$light)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
gm_sunsha <- droplevels(gm_sunsha)

##sun-shade dataframes
sundat <- gm_sunsha[gm_sunsha$leaflight =="sun-high",]
shadat <- gm_sunsha[gm_sunsha$leaflight =="shade-low",]
##dfr with lights on
fleckdat <- gm_agg[gm_agg$leaflight == "shade-high",]
fleckdat <- droplevels(fleckdat)

#### GS vs A data: use gam for CI of non-linear relationship between A and gs----------------------------------------
library(mgcv)
#SUN leaves
sunmod <- gam(Photo ~ s(Cond, k=5), data=gm_sunsha, subset=leaflight=="sun-high")

#predict
#get apprpriate vector of gs from sun leaves
gsdat <- gm_sunsha[gm_sunsha$leaflight=="sun-high", "Cond"]

#generate sequence and then predict
gssun_seq <- seq(min(gsdat), max(gsdat), length=101)
gssun_pred <- predict(sunmod, newdata=data.frame(Cond=gssun_seq), se.fit=TRUE)

#ci and model fit
sunupr <- gssun_pred$fit + (2*gssun_pred$se.fit)
sunlwr <- gssun_pred$fit - (2*gssun_pred$se.fit)

#SHADE leaves
shamod <- gam(Photo ~ s(Cond, k=5), data=gm_sunsha, subset=leaflight=="shade-low")

#get apprpriate vector cond from sun leaves
gsdat2 <- gm_sunsha[gm_sunsha$leaflight=="shade-low", "Cond"]
#generate sequence and then predict
gssha_seq <- seq(min(gsdat2), max(gsdat2), length=101)
gssha_pred <- predict(shamod, newdata=data.frame(Cond=gssha_seq), type="link", se.fit=TRUE)

shaupr <- gssha_pred$fit + (2*gssha_pred$se.fit)
shalwr <- gssha_pred$fit - (2*gssha_pred$se.fit)

#SUNFLECK leaves
fleckmod <- gam(Photo ~ s(Cond, k=5), data=fleckdat)

#get apprpriate vector cond from sun leaves
gsfleck <- fleckdat[, "Cond"]
#generate sequence and then predict
gsfleck_seq <- seq(min(gsfleck), max(gsfleck), length=101)
gsfleck_pred <- predict(fleckmod, newdata=data.frame(Cond=gsfleck_seq), type="link", se.fit=TRUE)

fleckupr <- gsfleck_pred$fit + (2*gsfleck_pred$se.fit)
flecklwr <- gsfleck_pred$fit - (2*gsfleck_pred$se.fit)

#### Gm vs A data: use gam for CI of non-linear relationship between A and gs----------------------------------------------

##read bootstrapped data previosuly ran from sunshade phys script
agm_sun <-   read.csv( "master_scripts/bootstrap_results/agm_sun.csv")
agm_sha <-  read.csv( "master_scripts/bootstrap_results/agm_sha.csv") 
agm_fleck <-  read.csv( "master_scripts/bootstrap_results/agm_fleck.csv") 

gmA_sun_mod2 <- lm(Photo ~ gm , data=sundat)

gmA_sha_mod2 <- lm(Photo~ gm  ,data=shadat)

gmA_fleck_mod2 <- lm(Photo~ gm  ,data=fleckdat)



###with smoothplot
palette(c(lightscol, newshacol, suncol))


###needs A gs with and without lights-----------------------------------------------------------------------------
png(filename = "makepngs/ags1.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,6,1,1), las=1, cex.axis=1.5, cex.lab=2, mgp=c(3.5,1,0))
plot(Photo~Cond, data=gm_agg,  col=leaflight, ylim=c(5,25), xlim=c(0,.5), xlab="", ylab="",pch="")
par(new=TRUE)
smoothplot(Cond, Photo, leaflight,data=gm_agg, kgam=5, R="chamber",ylim=c(5,25), xlim=c(0,.5),
           linecol=c(lightscol2, newshade,suncol2),pch="", ylab=satlab, xlab=condlab)
points(Photo~Cond, data=gm_agg,  col=leaflight, pch=c(16, 17)[gm_sunsha$temp], cex=2.5)

legend("topleft", alllab, pch=c(16,16,16,16,17), col=allcols2,inset = 0.01, bty='n',cex=1.75)

dev.off()


###no lightson

png(filename = "makepngs/ags2.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,6,1,1), las=1, cex.axis=1.5, cex.lab=2, mgp=c(3.5,1,0))
plot(Photo~Cond, data=gm_agg[gm_agg$leaflight!="shade-high",],  col=leaflight, ylim=c(5,25), xlim=c(0,.4), xlab="", ylab="",pch="")
par(new=TRUE)
smoothplot(Cond, Photo, leaflight,data=gm_agg[gm_agg$leaflight!="shade-high",], kgam=5, R="chamber",ylim=c(5,25), xlim=c(0,.4),
           linecol=c(newshade,suncol2),pch="", ylab=satlab, xlab=condlab)
points(Photo~Cond, data=gm_agg[gm_agg$leaflight!="shade-high",],  col=leaflight, pch=c(16, 17)[gm_sunsha$temp], cex=2.5)

legend("topleft", c("Sun", "Shade", "AT", "ET"), pch=c(16,16,16,17), col=c(suncol2, newshade, "black", "black"),
       inset = 0.01, bty='n',cex=1.75)

dev.off()

####agm all lights

png(filename = "makepngs/agm.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,6,1,1), las=1, cex.axis=1.5, cex.lab=2, mgp=c(3.5,1,0))
plot(Photo~gm, data=gm_agg,  col=leaflight, ylim=c(5,25), xlim=c(0,.4), xlab=gmlab, ylab=satlab, pch="")
predline(gmA_sun_mod2, col=suncol2,lwd=2)
predline(gmA_sha_mod2, col=newshade,lwd=2)
predline(gmA_fleck_mod2, col=lightscol2,lwd=2)
points(Photo~gm, data=gm_agg,  col=leaflight, pch=c(16, 17)[gm_sunsha$temp], cex=2.5)

legend("topleft", alllab, pch=c(16,16,16,16,17), col=allcols2,inset = 0.01, bty='n',cex=1.75)

dev.off()

