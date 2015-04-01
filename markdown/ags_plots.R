# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
# source("functions and packages/packages.R")
library(mgcv)
library(lme4)

library(plantecophys)

#read in gm data set (no drought) and Cibar(discrimination)

Ci_bar <- read.csv("calculated_data/Ci_bar.csv")

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
##calculate CC
gmes$Cc<- with(gmes, Ci-Photo/gm)
gmes$cc_ci<- with(gmes, Ci/Cc)

###get average by id
gm_agg <- summaryBy(Photo+Cond+CTleaf+Ci+Cc+gm+VpdL+xsi+DELTA+cc_ci ~ chamber+id+leaf +light+temp+leaflight, 
                    data=gmes, FUN=mean, keep.names=TRUE)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
gm_sunsha <- droplevels(gm_sunsha)
gm_sunsha$id <- gsub("-high", "", gm_sunsha$id)
gm_sunsha$id <- gsub("-low", "", gm_sunsha$id)

##merge
gm_c13 <- merge(gm_sunsha, Ci_bar[, c(2,8)], by="id")
gm_c13$Cc_bar <- with(gm_c13, ci_bar-Photo/gm)


###Photosynthesis vs gs (need to fit sun with something else)------------------------------------------------
###simple plot use smoothplot from RD and get CI polygon
palette(c(shacol, suncol))

###try to add 95%ci as dotted line and then turn shading of addpoly lighter

####run gam models and then predict
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
# windows(10,8)
# plot(Photo~Cond, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
#      xlim=c(0,.4), xlab=condlab, ylab="", cex=1.25)
# 
# lines(gssun_seq, sunupr, lty=2, lwd=2,col=suncol)
# lines(gssun_seq, sunlwr, lty=2, lwd=2,col=suncol)
# lines(gssun_seq, gssun_pred$fit, lty=1, lwd=2,col=suncol)
# 
# #shade
# points(Photo~Cond, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
# lines(gssha_seq, shaupr, lty=2, lwd=2,col=shacol)
# lines(gssha_seq, shalwr, lty=2, lwd=2,col=shacol)
# lines(gssha_seq, gssha_pred$fit, lty=1, lwd=2,col=shacol)
# 
# 
# title(ylab=satlab, mgp=ypos, cex=1.2)
# legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol) 
# dev.copy2pdf(file="master_scripts/figures/photo_gs.pdf")
# dev.off()

#####a gs plots---------------------------------------------------------------------------------------------
aci_leaf <- read.csv("calculated_data/tdl_aci.csv")

gsT_sun <- data.frame(GS= gm_c13[gm_c13$leaflight=="sun-high", "Cond"], 
                       Tleaf=gm_c13[gm_c13$leaflight=="sun-high", "CTleaf"])

gsT_sha <- data.frame(GS= gm_c13[gm_c13$leaflight=="shade-low", "Cond"], 
                      Tleaf=gm_c13[gm_c13$leaflight=="shade-low", "CTleaf"])

gsT_sun <- arrange(gsT_sun, GS)
gsT_sha <- arrange(gsT_sha, GS)
gssun_seq2 <- seq(min(gsT_sun[1]), max(gsT_sun[1]), length=101)
gssha_seq2 <- seq(min(gsT_sha[1]), max(gsT_sha[1]), length=101)

sunT <- mean(gsT_sun[[2]])
shaT <- mean(gsT_sha[[2]])

##ags curves with photosyn
# sunAT_sim <- mapply(Photosyn,GS=gsT_sun[1], Tleaf=gsT_sun[2],Vcmax=aci_leaf[3,3], Jmax=aci_leaf[3,4],PPFD=1408)
# sunET_sim <- mapply(Photosyn,GS=gsT_sun[1], Tleaf=gsT_sun[2],Vcmax=aci_leaf[4,3], Jmax=aci_leaf[4,4], PPFD=1408)
# shaAT_sim <- mapply(Photosyn,GS=gsT_sha[1], Tleaf=gsT_sha[2],Vcmax=aci_leaf[1,3], Jmax=aci_leaf[1,4], PPFD=375)
# shaET_sim <- mapply(Photosyn,GS=gsT_sha[1], Tleaf=gsT_sha[2],Vcmax=aci_leaf[2,3], Jmax=aci_leaf[2,4],PPFD=375)

sunAT_sim2 <- Photosyn(GS=gssun_seq2,Vcmax=aci_leaf[3,3], Jmax=aci_leaf[3,4],PPFD=1408, Tleaf=sunT)
sunET_sim2 <- Photosyn(GS=gssun_seq2 ,Vcmax=aci_leaf[4,3], Jmax=aci_leaf[4,4], PPFD=1408, Tleaf = sunT)
shaAT_sim2 <- Photosyn(GS=gssha_seq2,Vcmax=aci_leaf[1,3], Jmax=aci_leaf[1,4], PPFD=350,Tleaf = shaT)
shaET_sim2 <- Photosyn(GS=gssha_seq2 ,Vcmax=aci_leaf[2,3], Jmax=aci_leaf[2,4],PPFD=350,Tleaf = shaT)

# windows(10,10)
# plot(sunAT_sim[[3]], sunAT_sim[[2]], col=suncol, pch=21,  cex=1.1,ylim=c(5,25), 
#      xlim=c(0,.35), xlab=condlab, ylab="", type="l", lwd=2)
# points(sunET_sim[[3]], sunET_sim[[2]], col=suncol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
# points(shaAT_sim[[3]], shaAT_sim[[2]], col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
# points(shaET_sim[[3]], shaET_sim[[2]], col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
# 
# points(Photo~Cond, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
# points(Photo~Cond, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25)
# title(ylab=satlab, mgp=ypos, cex=1.2)
# legend("topleft", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)
# dev.copy2pdf(file="master_scripts/figures/photo_gs_pred.pdf")
# dev.off()


###panel plot
par(cex.axis=.96, cex.lab=1.2,mfrow=c(1,2),oma=c(0.1,0.1,0.1,0.1), las=1) 

par(mar=c(5,4,2,0))
plot(Photo~Cond, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,30), 
     xlim=c(0,.4), xlab=condlab, ylab="", cex=1.25)

lines(gssun_seq, sunupr, lty=2, lwd=2,col=suncol)
lines(gssun_seq, sunlwr, lty=2, lwd=2,col=suncol)
lines(gssun_seq, gssun_pred$fit, lty=1, lwd=2,col=suncol)

#shade
points(Photo~Cond, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
lines(gssha_seq, shaupr, lty=2, lwd=2,col=shacol)
lines(gssha_seq, shalwr, lty=2, lwd=2,col=shacol)
lines(gssha_seq, gssha_pred$fit, lty=1, lwd=2,col=shacol)
text(.01, 24.5, "(a)", cex=1.2)

title(ylab=satlab, mgp=ypos, cex=1.2)
legend("topright", leaflab2, pch=16,inset = 0.03, col=leafcol) 

par(mar=c(5,0,2,2))
plot(sunAT_sim2$GS, sunAT_sim2$ALEAF, col=suncol, pch=21,  cex=1.1,ylim=c(0,30), 
     xlim=c(0,.4), xlab=condlab, ylab="", type="l", lwd=2, yaxt='n')
points(sunET_sim2$GS, sunET_sim2$ALEAF, col=suncol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
points(shaAT_sim2$GS, shaAT_sim2$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
points(shaET_sim2$GS, shaET_sim2$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)

points(Photo~Cond, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
points(Photo~Cond, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25)
legend("topright", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)
text(x=.01, 24.5, "(b)", cex=1.2)
