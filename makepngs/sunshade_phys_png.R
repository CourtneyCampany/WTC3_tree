source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")
library(mgcv)
library(lme4)

#read in gm data set (no drought) and Cibar(discrimination)

Ci_bar <- read.csv("calculated_data/Ci_bar.csv")

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
  ##calculate CC
  gmes$Cc<- with(gmes, Ci-Photo/gm)
  gmes$cc_ci<- with(gmes, Ci/Cc)

###get average by id
gm_agg <- summaryBy(Photo+Cond+Ci+Cc+gm+VpdL+xsi+DELTA+cc_ci ~ chamber+id+leaf +light+temp+leaflight, 
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
  png(filename = "makepngs/photo_cond.png", width = 11, height = 8.5, units = "in", res= 400)
  
  par(mar=c(5,7,2,2), cex.lab=2)
  plot(Photo~Cond, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
       xlim=c(0,.4), xlab="", ylab=satlab, cex.axis=1.75, cex=1.5)
  
  lines(gssun_seq, sunupr, lty=2, lwd=3.5,col=suncol)
  lines(gssun_seq, sunlwr, lty=2, lwd=3.5,col=suncol)
  lines(gssun_seq, gssun_pred$fit, lty=1, lwd=3.5,col=suncol)
  
  #shade
  points(Photo~Cond, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.5)
  lines(gssha_seq, shaupr, lty=2, lwd=3.5,col=shacol)
  lines(gssha_seq, shalwr, lty=2, lwd=3.5,col=shacol)
  lines(gssha_seq, gssha_pred$fit, lty=1, lwd=3.5,col=shacol)
  
  title(xlab=condlab, line=4, cex=1.25)
  legend("topleft", leaflab2, pch=16,inset = 0.02, col=leafcol, bty='n', cex=1.75) 
  
  dev.off()

##bootstrap plots of confidence intervals with chamber as random effect---------------------------------
  library(lmerTest)
  library(plyr)
  
  palette(c(shacol, suncol))
  
  sundat <- gm_c13[gm_c13$leaflight =="sun-high",]
  shadat <- gm_c13[gm_c13$leaflight =="shade-low",]
  
  ##make dfrs for each comparison for simplicity with bootstrapping
  acc_sun <- sundat[, c("Photo", "Cc", "chamber")]
  acc_sha<- shadat[, c("Photo", "Cc", "chamber")]
  
  aci_sun <- sundat[, c("Photo", "Ci", "chamber")]
  aci_sha<- shadat[, c("Photo", "Ci", "chamber")]
  
  agm_sun <- sundat[, c("Photo", "gm", "chamber")]
  agm_sha<- shadat[, c("Photo", "gm", "chamber")]
  
  acib_sun <- sundat[, c("Photo", "ci_bar", "chamber")]
  acib_sha<- shadat[, c("Photo", "ci_bar", "chamber")]
  
  gmgs_sun <- sundat[, c("Photo", "gmgs", "chamber")]
  gmgs_sha<- shadat[, c("Photo", "gmgs", "chamber")]
  
  ###Photosynthesis vs Cc
  
  #SUN
  Acc_sun_lm <- lmer(Photo~ Cc + (1|chamber), data=acc_sun)
  
  boot_accsun <- bootMer(Acc_sun_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
  acc_sun$lcl <- apply(boot_accsun$t, 2, quantile, 0.025)
  acc_sun$ucl <- apply(boot_accsun$t, 2, quantile, 0.975)
  acc_sun$pred <- predict(Acc_sun_lm, re.form=NA)
  acc_sun <- arrange(acc_sun, Cc)
  
  Acc_sha_lm <- lmer(Photo~ Cc + (1|chamber), data=acc_sha)
  
  boot_accsha <- bootMer(Acc_sha_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
  acc_sha$lcl <- apply(boot_accsha$t, 2, quantile, 0.025)
  acc_sha$ucl <- apply(boot_accsha$t, 2, quantile, 0.975)
  acc_sha$pred <- predict(Acc_sha_lm, re.form=NA)
  acc_sha <- arrange(acc_sha, Cc)
  
  png(filename = "makepngs/photo_cc.png", width = 11, height = 8.5, units = "in", res= 400)
  
  par(mar=c(5,7,2,2), cex.lab=2)
  plot(Photo~Cc, data=sundat, pch=16, col=suncol, ylim=c(0,25), xlim=c(0,350), xlab="", ylab=satlab, 
       cex.axis=1.75, cex=1.5)
  points(Photo~Cc, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.5)
  with(acc_sun, {
    lines(Cc, lcl, lty=2, lwd=3.5,col="forestgreen")
    lines(Cc, ucl, lty=2, lwd=3.5,col="forestgreen")
    lines(Cc, pred, lty=1, lwd=3.5,col="forestgreen")
  })
  with(acc_sha, {
    lines(Cc, lcl, lty=2, lwd=3.5,col="yellow4")
    lines(Cc, ucl, lty=2, lwd=3.5,col="yellow4")
    lines(Cc, pred, lty=1, lwd=3.5,col="yellow4")
  })
  
  title(xlab=cclab, line=4, cex=1.25)
  legend("topleft", leaflab2, pch=16,inset = 0.02, col=leafcol, bty='n', cex=1.75) 

  dev.off()
  

###Photosynthesis vs Ci--------------------------------------------------------------------------------

#SUN
Aci_sun_lm <- lmer(Photo~ Ci + (1|chamber), data=aci_sun)

boot_acisun <- bootMer(Aci_sun_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
aci_sun$lcl <- apply(boot_acisun$t, 2, quantile, 0.025)
aci_sun$ucl <- apply(boot_acisun$t, 2, quantile, 0.975)
aci_sun$pred <- predict(Aci_sun_lm, re.form=NA)
aci_sun <- arrange(aci_sun, Ci)

Aci_sha_lm <- lmer(Photo~ Ci + (1|chamber), data=aci_sha)

boot_accsha <- bootMer(Aci_sha_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
aci_sha$lcl <- apply(boot_accsha$t, 2, quantile, 0.025)
aci_sha$ucl <- apply(boot_accsha$t, 2, quantile, 0.975)
aci_sha$pred <- predict(Aci_sha_lm, re.form=NA)
aci_sha <- arrange(aci_sha, Ci)

png(filename = "makepngs/photo_ci.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,7,2,2), cex.lab=2)
plot(Photo~Ci, data=sundat, pch=16, col=suncol, ylim=c(0,25), xlim=c(0,350), xlab="", ylab=satlab, 
     cex.axis=1.75,cex=1.5)
points(Photo~Ci, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.5)
with(aci_sun, {
  lines(Ci, lcl, lty=2, lwd=3.5,col="forestgreen")
  lines(Ci, ucl, lty=2, lwd=3.5,col="forestgreen")
  lines(Ci, pred, lty=1, lwd=3.5,col="forestgreen")
})
with(aci_sha, {
  lines(Ci, lcl, lty=2, lwd=3.5,col="yellow4")
  lines(Ci, ucl, lty=2, lwd=3.5,col="yellow4")
  lines(Ci, pred, lty=1, lwd=3.5,col="yellow4")
})

title(xlab=cilab, line=4, cex=1.25)
legend("topleft", leaflab2, pch=16,inset = 0.02, col=leafcol, bty='n', cex=1.75)

dev.off()


###Photosynthesis vs gm-------------------------------------------------------------------------------

#SUN
Agm_sun_lm <- lmer(Photo~ gm + (1|chamber), data=agm_sun)

  boot_agmsun <- bootMer(Agm_sun_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
  agm_sun$lcl <- apply(boot_agmsun$t, 2, quantile, 0.025)
  agm_sun$ucl <- apply(boot_agmsun$t, 2, quantile, 0.975)
  agm_sun$pred <- predict(Agm_sun_lm, re.form=NA)
  agm_sun <- arrange(agm_sun, gm)

Agm_sha_lm <- lmer(Photo~ gm + (1|chamber), data=agm_sha)

  boot_agmsha <- bootMer(Agm_sha_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
  agm_sha$lcl <- apply(boot_agmsha$t, 2, quantile, 0.025)
  agm_sha$ucl <- apply(boot_agmsha$t, 2, quantile, 0.975)
  agm_sha$pred <- predict(Agm_sha_lm, re.form=NA)
  agm_sha <- arrange(agm_sha, gm)

  
png(filename = "makepngs/photo_gm.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,7,2,2), cex.lab=2)
plot(Photo~gm, data=sundat, pch=16, col=suncol, ylim=c(0,25), xlim=c(0,.5), xlab="", ylab=satlab, 
     cex.axis=1.75, cex=1.5)
  points(Photo~gm, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.5)
with(agm_sun, {
  lines(gm, lcl, lty=2, lwd=3.5,col="forestgreen")
  lines(gm, ucl, lty=2, lwd=3.5,col="forestgreen")
  lines(gm, pred, lty=1, lwd=3.5,col="forestgreen")
})
with(agm_sha, {
  lines(gm, lcl, lty=2, lwd=3.5,col="yellow4")
  lines(gm, ucl, lty=2, lwd=3.5,col="yellow4")
  lines(gm, pred, lty=1, lwd=3.5,col="yellow4")
})

title(xlab=gmlab, line=4, cex=1.25)
legend("topleft", leaflab2, pch=16,inset = 0.02, col=leafcol, bty='n', cex=1.75) 

dev.off()


###Photosynthesis vs cibar

  #SUN
  Acib_sun_lm <- lmer(Photo~ ci_bar + (1|chamber), data=acib_sun)
  
  boot_acibsun <- bootMer(Acib_sun_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
  acib_sun$lcl <- apply(boot_acibsun$t, 2, quantile, 0.025)
  acib_sun$ucl <- apply(boot_acibsun$t, 2, quantile, 0.975)
  acib_sun$pred <- predict(Acib_sun_lm, re.form=NA)
  acib_sun <- arrange(acib_sun, ci_bar)
  
  Acib_sha_lm <- lmer(Photo~ ci_bar + (1|chamber), data=acib_sha)
  
  boot_acibsha <- bootMer(Acib_sha_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
  acib_sha$lcl <- apply(boot_acibsha$t, 2, quantile, 0.025)
  acib_sha$ucl <- apply(boot_acibsha$t, 2, quantile, 0.975)
  acib_sha$pred <- predict(Acib_sha_lm, re.form=NA)
  acib_sha <- arrange(acib_sha, ci_bar)
  

png(filename = "makepngs/photo_cibar.png", width = 11, height = 8.5, units = "in", res= 400)
par(mar=c(5,7,2,2), cex.lab=2)

plot(Photo~ci_bar, data=sundat, pch=16, col=suncol, ylim=c(0,25), 
     xlim=c(150,350), xlab="", ylab=satlab ,cex.axis=1.75, cex=1.5)
#shade
points(Photo~ci_bar, data=shadat, pch=16, col=shacol, cex=1.5)
  with(acib_sun, {
    lines(ci_bar, lcl, lty=2, lwd=3.5,col="forestgreen")
    lines(ci_bar, ucl, lty=2, lwd=3.5,col="forestgreen")
    lines(ci_bar, pred, lty=1, lwd=3.5,col="forestgreen")
  })
  with(acib_sha, {
    lines(ci_bar, lcl, lty=2, lwd=3.5,col="yellow4")
    lines(ci_bar, ucl, lty=2, lwd=3.5,col="yellow4")
    lines(ci_bar, pred, lty=1, lwd=3.5,col="yellow4")
  })

title(xlab=cibarlab2, line=4, cex=1.25)
legend("topleft", leaflab2, pch=16,inset = 0.02, col=leafcol, bty='n', cex=1.75) 

dev.off()
  
###Photosynthesis vs total conductance to CO2-------------------------------------------------------------
  
###try with gam
  
  #SUN leaves
  gmgs_sun_mod <- gam(Photo ~ s(gmgs, k=10), data=gm_c13, subset=leaflight=="sun-high")
  
  #predict
  #get apprpriate vector of gs from sun leaves
  gmgs_dat <- gm_c13[gm_c13$leaflight=="sun-high", "gmgs"]
  
  #generate sequence and then predict
  gmgs_sun_seq <- seq(min(gmgs_dat), max(gmgs_dat), length=101)
  gmgs_sun_pred <- predict(gmgs_sun_mod, newdata=data.frame(gmgs=gmgs_sun_seq), se.fit=TRUE)
  
  #ci and model fit
  gmgs_sunupr <- gmgs_sun_pred$fit + (2*gmgs_sun_pred$se.fit)
  gmgs_sunlwr <- gmgs_sun_pred$fit - (2*gmgs_sun_pred$se.fit)
  
  #SHADE leaves
  gmgs_shamod <- gam(Photo ~ s(gmgs, k=10), data=gm_c13, subset=leaflight=="shade-low")
  
  #get apprpriate vector CC from sun leaves
  gmgs_dat2 <- gm_c13[gm_c13$leaflight=="shade-low", "gmgs"]
  #generate sequence and then predict
  gmgs_sha_seq <- seq(min(gmgs_dat2), max(gmgs_dat2), length=101)
  gmgs_sha_pred <- predict(gmgs_shamod, newdata=data.frame(gmgs=gmgs_sha_seq), type="link", se.fit=TRUE)
  
  gmgs_shaupr <- gmgs_sha_pred$fit + (2*gmgs_sha_pred$se.fit)
  gmgs_shalwr <- gmgs_sha_pred$fit - (2*gmgs_sha_pred$se.fit)
  
  ###plot
  png(filename = "makepngs/photo_gmgs.png", width = 11, height = 8.5, units = "in", res= 400)
  par(mar=c(5,7,2,2), cex.lab=2)
  
  plot(Photo~gmgs, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
       xlim=c(0,.6), xlab="", ylab=satlab, cex.axis=1.75, cex=1.5)
  
  lines(gmgs_sun_seq, gmgs_sunupr, lty=3.5, lwd=2,col=suncol)
  lines(gmgs_sun_seq, gmgs_sunlwr, lty=3.5, lwd=2,col=suncol)
  lines(gmgs_sun_seq, gmgs_sun_pred$fit, lty=1, lwd=3.5,col=suncol)
  
  #shade
  points(Photo~gmgs, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.5)
  lines(gmgs_sha_seq, gmgs_shaupr, lty=2, lwd=3.5,col=shacol)
  lines(gmgs_sha_seq, gmgs_shalwr, lty=2, lwd=3.5,col=shacol)
  lines(gmgs_sha_seq, gmgs_sha_pred$fit, lty=1, lwd=3.5,col=shacol)
  
  title(xlab=totcondlab, line=4, cex=1.25)
  legend("topleft", leaflab2, pch=16,inset = 0.02, col=leafcol, bty='n', cex=1.75)  

dev.off()
