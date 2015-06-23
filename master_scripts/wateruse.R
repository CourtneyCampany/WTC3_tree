source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")
library(mgcv)
library(lme4)

####pane a = ITE and VPD
ite <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
ite_agg <- summaryBy(Photo+Trmmol+VpdL ~ chamber+id+leaf +light+temp+leaflight+Month, 
                     data=ite, FUN=mean, keep.names=TRUE)

#add iWUE
ite_agg$ite <- with(ite_agg, Photo/Trmmol)

##remove shade-high
ite_sunsha <- ite_agg[ite_agg$leaflight != "shade-high",]
ite_sunsha <- droplevels(ite_sunsha)

###add model equation for ITE
k <- 0.5

(ite$CO2r * 102.3)/(1.6*(g1*(ite$VPDl^k)+ ite$VPDl))


windows(8,8)
#png(filename = "makepngs/ite_vpd.png", width = 11, height = 8.5, units = "in", res= 400)
par(mar=c(5,5,2,2), cex.lab=1.25, las=1, cex=1.5, cex.axis=1.25)
plot(ite~VpdL, data=ite_sunsha, subset=leaflight=="sun-high",  col=suncol, xlab=vpdlab, ylab=itelab,
     xlim=c(0,4), ylim=c(0,15),  pch=c(16, 17)[pch=ite_sunsha$temp])
points(ite~VpdL, data=ite_sunsha, subset=leaflight=="shade-low", col=shacol, pch=c(16, 17)[pch=ite_sunsha$temp]) 

legend("topright", leglab2, pch=c(16,17,16,17), col=colaci,inset = 0.01, bty='n')


#2: panel B = A-gs------------------------------------------------------

#########can I remove CIbardata for more points

#read in gm data set (no drought) and Cibar(discrimination)-

Ci_bar <- read.csv("calculated_data/Ci_bar.csv")

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
##calculate CC
gmes$Cc<- with(gmes, Ci-Photo/gm)
gmes$cc_ci<- with(gmes, Ci/Cc)

###get average by id
gm_agg <- summaryBy(Photo+Cond+Ci+Trmmol+Cc+gm+VpdL+xsi+DELTA+cc_ci ~ chamber+id+leaf +light+temp+leaflight+Month, 
                    data=gmes, FUN=mean, keep.names=TRUE)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
gm_sunsha <- droplevels(gm_sunsha)
gm_sunsha$id <- gsub("-high", "", gm_sunsha$id)
gm_sunsha$id <- gsub("-low", "", gm_sunsha$id)

##merge
gm_c13 <- merge(gm_sunsha, Ci_bar[, c(2,8)], by="id")
gm_c13$Cc_bar <- with(gm_c13, ci_bar-Photo/gm)
#add total conductance to CO2
gm_c13$gmgs <- with(gm_c13, gm+Cond)
#add iWUE
gm_c13$iWUE <- with(gm_c13, Photo/Trmmol)

####GAM PLOTS (photosynthesis vs gs or transpiration)-----------------------------------------------------------

##palette for sun shade
palette(c(shacol, suncol))

## Photosynthesis vs gs 

#SUN leaves
sunmod <- gam(Photo ~ s(Cond, k=5), data=gm_c13, subset=leaflight=="sun-high")

#predict
#get apprpriate vector of gs from sun leaves
gsdat <- gm_c13[gm_c13$leaflight=="sun-high", "Cond"]

#generate sequence and then predict
gssun_seq <- seq(min(gsdat), max(gsdat), length=101)
gssun_pred <- predict(sunmod, newdata=data.frame(Cond=gssun_seq), se.fit=TRUE)

#ci and model fit
sunupr <- gssun_pred$fit + (2*gssun_pred$se.fit)
sunlwr <- gssun_pred$fit - (2*gssun_pred$se.fit)

#SHADE leaves
shamod <- gam(Photo ~ s(Cond, k=5), data=gm_c13, subset=leaflight=="shade-low")

#get apprpriate vector CC from sun leaves
gsdat2 <- gm_c13[gm_c13$leaflight=="shade-low", "Cond"]
#generate sequence and then predict
gssha_seq <- seq(min(gsdat2), max(gsdat2), length=101)
gssha_pred <- predict(shamod, newdata=data.frame(Cond=gssha_seq), type="link", se.fit=TRUE)

shaupr <- gssha_pred$fit + (2*gssha_pred$se.fit)
shalwr <- gssha_pred$fit - (2*gssha_pred$se.fit)

###plot
windows(10,8)
par(mar=c(5,5,2,2), cex.axis=1.25, cex.lab=1.5)
plot(Photo~Cond, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(5,20), 
     xlim=c(0,.35), xlab=condlab, ylab=satlab, cex=1.25)

lines(gssun_seq, sunupr, lty=2, lwd=2,col=suncol)
lines(gssun_seq, sunlwr, lty=2, lwd=2,col=suncol)
lines(gssun_seq, gssun_pred$fit, lty=1, lwd=2,col=suncol)

#shade
points(Photo~Cond, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
lines(gssha_seq, shaupr, lty=2, lwd=2,col=shacol)
lines(gssha_seq, shalwr, lty=2, lwd=2,col=shacol)
lines(gssha_seq, gssha_pred$fit, lty=1, lwd=2,col=shacol)

legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol, bty='n', cex=1.25) 


