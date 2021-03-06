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


####PLOTTING: 2 panel figure with gm, gs, A---------------------------------------------------------------------------------

#1: panel Photosynthesis vs gs (gam plots)
windows(10, 12)

par(mfrow=c(2,1))

par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(Photo~Cond, data=gm_sunsha, subset=leaflight=="sun-high",  col=suncol, ylim=c(5,25), 
     xlim=c(0,.5), xlab=condlab, ylab=satlab,  pch=c(16, 17)[pch=gm_sunsha$temp])

lines(gssun_seq, sunupr, lty=2, lwd=2,col=suncol)
lines(gssun_seq, sunlwr, lty=2, lwd=2,col=suncol)
lines(gssun_seq, gssun_pred$fit, lty=1, lwd=2,col=suncol)

#shade
points(Photo~Cond, data=gm_sunsha, subset=leaflight=="shade-low",col=shacol,pch=c(16, 17)[pch=gm_sunsha$temp])
lines(gssha_seq, shaupr, lty=2, lwd=2,col=shacol)
lines(gssha_seq, shalwr, lty=2, lwd=2,col=shacol)
lines(gssha_seq, gssha_pred$fit, lty=1, lwd=2,col=shacol)

#sunfleck  
points(Photo~Cond, data=fleckdat, col=lightscol,pch=c(16, 17)[pch=fleckdat$temp])

lines(gsfleck_seq, fleckupr, lty=2, lwd=2,col=lightscol)
lines(gsfleck_seq, flecklwr, lty=2, lwd=2,col=lightscol)
lines(gsfleck_seq, gsfleck_pred$fit, lty=1, lwd=2,col=lightscol)
legend("topleft", alllab, pch=c(16,16,16,16,17), col=allcols,inset = 0.01, bty='n',cex=.8)


text(x=.5, y=24.5, "(a)", cex=1)


####panel2: gm vs A

par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(Photo~gm, data=sundat,  col=suncol, ylim=c(5,25), xlim=c(0,.5), xlab=gmlab, ylab=satlab, 
     pch=c(16, 17)[pch=gm_sunsha$temp])
points(Photo~gm, data=shadat, col=shacol, pch=c(16, 17)[pch=gm_sunsha$temp])
points(Photo~gm, data=fleckdat, col=lightscol, pch=c(16, 17)[pch=fleckdat$temp])

with(agm_sun, {
  lines(gm, lcl, lty=2, lwd=2,col=suncol2)
  lines(gm, ucl, lty=2, lwd=2,col=suncol2)
  lines(gm, pred, lty=1, lwd=2,col=suncol2)
})
with(agm_sha, {
  lines(gm, lcl, lty=2, lwd=2,col=shacol2)
  lines(gm, ucl, lty=2, lwd=2,col=shacol2)
  lines(gm, pred, lty=1, lwd=2,col=shacol2)
})

with(agm_fleck, {
  lines(gm, lcl, lty=2, lwd=2,col=lightscol2)
  lines(gm, ucl, lty=2, lwd=2,col=lightscol2)
  lines(gm, pred, lty=1, lwd=2,col=lightscol2)
})

text(x=.5, y=24.5, "(b)", cex=1)

dev.copy2pdf(file="master_scripts/paper_figures/gmgs.pdf")
dev.off()