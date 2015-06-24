source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")
library(mgcv)
library(lme4)

####panel a = ITE and VPD

ite <- read.csv("calculated_data/gmes_wellwatered.csv")
g1_leaf <- read.csv("calculated_data/g1_leaf.csv")

###get average by id
ite_agg <- summaryBy(Photo+Cond+ Trmmol+VpdL ~ chamber+id+leaf +light+temp+leaflight+Month, 
                     data=ite, FUN=mean, keep.names=TRUE)

#add iWUE
ite_agg$ite <- with(ite_agg, Photo/Trmmol)

##remove shade-high
ite_sunsha <- ite_agg[ite_agg$leaflight != "shade-high",]
ite_sunsha <- droplevels(ite_sunsha)

###add model equation for ITE
k <- 0.5
Ca <- 400
Pa <- 102.3


vpdrange <- seq(.1, 4, length= 101)

ite_sunat <- ((Ca * Pa) / ((g1_leaf[5,1]* sqrt(vpdrange)) + vpdrange))/1000
ite_sunet <- ((Ca * Pa) / ((g1_leaf[6,1]* sqrt(vpdrange)) + vpdrange))/1000
ite_shaat <- ((Ca * Pa) / ((g1_leaf[3,1]* sqrt(vpdrange)) + vpdrange))/1000
ite_shaet <- ((Ca * Pa) / ((g1_leaf[4,1]* sqrt(vpdrange)) + vpdrange))/1000

###use gam for CI of non-linear relationship between A and gs------------------------------------------------------

#SUN leaves
sunmod <- gam(Photo ~ s(Cond, k=5), data=ite_sunsha, subset=leaflight=="sun-high")

#predict
#get apprpriate vector of gs from sun leaves
gsdat <- ite_sunsha[ite_sunsha$leaflight=="sun-high", "Cond"]

#generate sequence and then predict
gssun_seq <- seq(min(gsdat), max(gsdat), length=101)
gssun_pred <- predict(sunmod, newdata=data.frame(Cond=gssun_seq), se.fit=TRUE)

#ci and model fit
sunupr <- gssun_pred$fit + (2*gssun_pred$se.fit)
sunlwr <- gssun_pred$fit - (2*gssun_pred$se.fit)

#SHADE leaves
shamod <- gam(Photo ~ s(Cond, k=5), data=ite_sunsha, subset=leaflight=="shade-low")

#get apprpriate vector CC from sun leaves
gsdat2 <- ite_sunsha[ite_sunsha$leaflight=="shade-low", "Cond"]
#generate sequence and then predict
gssha_seq <- seq(min(gsdat2), max(gsdat2), length=101)
gssha_pred <- predict(shamod, newdata=data.frame(Cond=gssha_seq), type="link", se.fit=TRUE)

shaupr <- gssha_pred$fit + (2*gssha_pred$se.fit)
shalwr <- gssha_pred$fit - (2*gssha_pred$se.fit)


#### Multi panel plot of WUE --------------------------------------------------------------------------------------------

windows(8,8)
#png(filename = "markdown/wateruse.png", width = 11, height = 8.5, units = "in", res= 400)

par(mfrow=c(2,1))
 
par(mar=c(5,5,1,1), cex.axis=.75, cex.lab=.9, las=1, cex=1.25)
plot(ite~VpdL, data=ite_sunsha, subset=leaflight=="sun-high",  col=suncol, xlab=vpdlab, ylab=itelab,
     xlim=c(0,4), ylim=c(0,20),  pch=c(16, 17)[pch=ite_sunsha$temp])
  points(ite~VpdL, data=ite_sunsha, subset=leaflight=="shade-low", col=shacol, pch=c(16, 17)[pch=ite_sunsha$temp]) 
  
  points(ite_sunat ~ vpdrange, type="l", col=suncol, lwd=2, lty=1)
  points(ite_sunet ~ vpdrange, type="l", col=suncol, lwd=2, lty=2)
  points(ite_shaat ~ vpdrange, type="l", col=shacol, lwd=2, lty=1)
  points(ite_shaet ~ vpdrange, type="l", col=shacol, lwd=2, lty=2)
  
  text(x=0, y=19.8 ,"(a)", cex=.9)
  
  legend("topright", leglab2, pch=c(16,17,16,17), col=colaci,inset = 0.01, bty='n',cex=.9)


#2: panel GAM PLOTS (photosynthesis vs gs or transpiration)-----------------------------------------------------------
par(mar=c(5,5,0,1),cex.axis=.75, cex.lab=.9, cex=1.25, las=1)
plot(Photo~Cond, data=ite_sunsha, subset=leaflight=="sun-high",  col=suncol, ylim=c(5,20), 
     xlim=c(0,.35), xlab=condlab, ylab=satlab,  pch=c(16, 17)[pch=ite_sunsha$temp])

  lines(gssun_seq, sunupr, lty=2, lwd=2,col=suncol)
  lines(gssun_seq, sunlwr, lty=2, lwd=2,col=suncol)
  lines(gssun_seq, gssun_pred$fit, lty=1, lwd=2,col=suncol)

#shade
points(Photo~Cond, data=ite_sunsha, subset=leaflight=="shade-low",col=shacol,pch=c(16, 17)[pch=ite_sunsha$temp])
  lines(gssha_seq, shaupr, lty=2, lwd=2,col=shacol)
  lines(gssha_seq, shalwr, lty=2, lwd=2,col=shacol)
  lines(gssha_seq, gssha_pred$fit, lty=1, lwd=2,col=shacol)
  text(x=0, y=19.8, "(b)", cex=.9)

dev.copy2pdf(file="master_scripts/paper_figures/wateruse.pdf")
dev.off()
  

