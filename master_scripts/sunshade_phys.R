source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")
library(mgcv)
library(lme4)

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------

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

  
####simple plots first to look at difference between water use efficiency btw sun shade---------------------  
  
  ##palette for temp treatment
  palette(c("blue", "red"))
  
  ###VPD between leaf types
  shadevpd <- gm_c13[gm_c13$leaflight=="shade-low", c("VpdL", "iWUE","id", "chamber", "Month", "temp")]
  names(shadevpd)[1:2] <- c("vpd_shade", "ITE_shade")
  sunvpd <- gm_c13[gm_c13$leaflight=="sun-high",  c("VpdL", "iWUE", "id", "chamber", "Month", "temp")]
  names(sunvpd)[1:2] <- c("vpd_sun", "ITE_sun")
  vpd_tree <- merge(shadevpd, sunvpd, by=c("chamber", "Month", "temp"))

  plot(vpd_shade~ vpd_sun, data=vpd_tree, ylab="vpd_sun", xlab="vpd_shade", ylim=c(0, 4), xlim=c(0,4),
       col=as.factor(temp), pch=16, cex=1.5)
  abline(0,1)
  
  plot(ITE_shade~ ITE_sun, data=vpd_tree, ylab="ITE_sun", xlab="ITE_shade", ylim=c(0, 15), xlim=c(0,15),
       col=as.factor(temp), pch=16, cex=1.5)
  abline(0,1)
  
  ##palette for sun shade
  palette(c(shacol, suncol))
  
  plot(Cond~Trmmol, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25, xlim=c(0,6), ylim=c(0,.4))
    points(Cond~Trmmol, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
    
  plot(Cond~VpdL, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25, xlim=c(0,5), ylim=c(0,.4))
    points(Cond~VpdL, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25) 
    
  bar(iWUE, c(leaf, temp), gm_c13, col=c("yellowgreen", "green4"),half.errbar=FALSE, mar=c(5,7,2,2), 
      legend = FALSE, ylim=c(0,10))
  legend("topright", c("Shade", "Sun"), pch=22,inset = 0.01, pt.bg=c("yellowgreen", "green4"),
           bty='n', cex=1.25)  
  
  bar(iWUE,leaf, gm_c13, col=c("yellowgreen", "green4"),half.errbar=FALSE, mar=c(5,7,2,2), 
      legend = FALSE, ylim=c(0,10), xlab="")
  legend("topright", c("Shade", "Sun"), pch=22,inset = 0.01, pt.bg=c("yellowgreen", "green4"),
         bty='n', cex=1.25) 
  
  plot(iWUE~VpdL, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25, xlim=c(0,4), ylim=c(0,20))
  points(iWUE~VpdL, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25) 
  legend("topright", c("Shade", "Sun"), pch=22,inset = 0.01, pt.bg=c("yellowgreen", "green4"),
         bty='n', cex=1.25) 
  
  
####GAM PLOTS (photosynthesis vs gs or transpiration)-------------------------------------------------------------
  
##1: Photosynthesis vs gs 
  #run gam models and then predict
  
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
  plot(Photo~Cond, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
       xlim=c(0,.4), xlab=condlab, ylab=satlab, cex=1.25)
  
  lines(gssun_seq, sunupr, lty=2, lwd=2,col=suncol)
  lines(gssun_seq, sunlwr, lty=2, lwd=2,col=suncol)
  lines(gssun_seq, gssun_pred$fit, lty=1, lwd=2,col=suncol)
  
  #shade
  points(Photo~Cond, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  lines(gssha_seq, shaupr, lty=2, lwd=2,col=shacol)
  lines(gssha_seq, shalwr, lty=2, lwd=2,col=shacol)
  lines(gssha_seq, gssha_pred$fit, lty=1, lwd=2,col=shacol)
  
  legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol, bty='n', cex=1.25) 
  dev.copy2pdf(file="master_scripts/figures/photo_gs.pdf")
  dev.off()
 
   
##2: Photosynthesis vs tranpiration

  #SUN leaves
  atrans_sun_mod <- gam(Photo ~ s(Trmmol, k=4), data=gm_c13, subset=leaflight=="sun-high")
  
  #predict
  #get apprpriate vector of transpiration from sun leaves
  atrans_dat <- gm_c13[gm_c13$leaflight=="sun-high", "Trmmol"]
  
  #generate sequence and then predict
  atrans_sun_seq <- seq(min(atrans_dat), max(atrans_dat), length=101)
  atrans_sun_pred <- predict(atrans_sun_mod, newdata=data.frame(Trmmol=atrans_sun_seq), se.fit=TRUE)
  
  #ci and model fit
  atrans_sunupr <- atrans_sun_pred$fit + (2*atrans_sun_pred$se.fit)
  atrans_sunlwr <- atrans_sun_pred$fit - (2*atrans_sun_pred$se.fit)
  
  #SHADE leaves
  atrans_shamod <- gam(Photo ~ s(Trmmol, k=4), data=gm_c13, subset=leaflight=="shade-low")
  
  #get apprpriate vector transpiration from sun leaves
  atrans_dat2 <- gm_c13[gm_c13$leaflight=="shade-low", "Trmmol"]
  #generate sequence and then predict
  atrans_sha_seq <- seq(min(atrans_dat2), max(atrans_dat2), length=101)
  atrans_sha_pred <- predict(atrans_shamod, newdata=data.frame(Trmmol=atrans_sha_seq), type="link", se.fit=TRUE)
  
  atrans_shaupr <- atrans_sha_pred$fit + (2*atrans_sha_pred$se.fit)
  atrans_shalwr <- atrans_sha_pred$fit - (2*atrans_sha_pred$se.fit)
  
  ###plot
  windows(10,8)
  par(mar=c(5,5,2,2), cex.axis=1.25, cex.lab=1.5)
  plot(Photo~Trmmol, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
       xlim=c(0,6), xlab=trmmollab, ylab=satlab, cex=1.25)
  
  lines(atrans_sun_seq, atrans_sunupr, lty=2, lwd=2,col=suncol)
  lines(atrans_sun_seq, atrans_sunlwr, lty=2, lwd=2,col=suncol)
  lines(atrans_sun_seq, atrans_sun_pred$fit, lty=1, lwd=2,col=suncol)
  
  #shade
  points(Photo~Trmmol, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  lines(atrans_sha_seq, atrans_shaupr, lty=2, lwd=2,col=shacol)
  lines(atrans_sha_seq, atrans_shalwr, lty=2, lwd=2,col=shacol)
  lines(atrans_sha_seq, atrans_sha_pred$fit, lty=1, lwd=2,col=shacol)
  
  legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol, bty='n', cex=1.25)  
  dev.copy2pdf(file="master_scripts/figures/phototransp.pdf")
  dev.off()
  

####BOOTSTRAPPING models with confidence intervals and chamber as random effect--------------------------------------
  library(lmerTest)
  library(plyr)
  
  ##sun-shade dataframes
  sundat <- gm_c13[gm_c13$leaflight =="sun-high",]
  shadat <- gm_c13[gm_c13$leaflight =="shade-low",]
  
  ##dfrs for each comparison for simplicity with bootstrapping
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
  
  
#1: Photosynthesis vs Cc
  
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
  
  windows(10,10)
  par(mar=c(5,5,2,2), cex.axis=1.25, cex.lab=1.5)
  plot(Photo~Cc, data=sundat, pch=16, col=suncol, ylim=c(0,25), xlim=c(0,350), xlab=cclab, ylab=satlab, cex=1.25)
  points(Photo~Cc, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  with(acc_sun, {
    lines(Cc, lcl, lty=2, lwd=2,col="forestgreen")
    lines(Cc, ucl, lty=2, lwd=2,col="forestgreen")
    lines(Cc, pred, lty=1, lwd=2,col="forestgreen")
  })
  with(acc_sha, {
    lines(Cc, lcl, lty=2, lwd=2,col="yellow4")
    lines(Cc, ucl, lty=2, lwd=2,col="yellow4")
    lines(Cc, pred, lty=1, lwd=2,col="yellow4")
  })
  
  legend("topright", leaflab2, pch=16,inset = 0.03, col=leafcol, bty='n', cex=1.25) 
  dev.copy2pdf(file="master_scripts/figures/photo_cc.pdf")
  dev.off()
  

#2: Photosynthesis vs Ci
  
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
  
  windows(10,10)
  par(mar=c(5,5,2,2), cex.axis=1.25, cex.lab=1.5)
  plot(Photo~Ci, data=sundat, pch=16, col=suncol, ylim=c(0,25), xlim=c(0,350), xlab=cclab, ylab=satlab, cex=1.25)
  points(Photo~Ci, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  with(aci_sun, {
    lines(Ci, lcl, lty=2, lwd=2,col="forestgreen")
    lines(Ci, ucl, lty=2, lwd=2,col="forestgreen")
    lines(Ci, pred, lty=1, lwd=2,col="forestgreen")
  })
  with(aci_sha, {
    lines(Ci, lcl, lty=2, lwd=2,col="yellow4")
    lines(Ci, ucl, lty=2, lwd=2,col="yellow4")
    lines(Ci, pred, lty=1, lwd=2,col="yellow4")
  })

  legend("topright", leaflab2, pch=16,inset = 0.03, col=leafcol, bty='n', cex=1.25) 
  dev.copy2pdf(file="master_scripts/figures/photo_ci.pdf")
  dev.off()


#3: Photosynthesis vs gm

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
  
  windows(10,10)
  par(mar=c(5,5,2,2), cex.axis=1.25, cex.lab=1.5)
  plot(Photo~gm, data=sundat, pch=16, col=suncol, ylim=c(0,25), xlim=c(0,.5), xlab=cclab, ylab=satlab, cex=1.25)
    points(Photo~gm, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  with(agm_sun, {
    lines(gm, lcl, lty=2, lwd=2,col="forestgreen")
    lines(gm, ucl, lty=2, lwd=2,col="forestgreen")
    lines(gm, pred, lty=1, lwd=2,col="forestgreen")
  })
  with(agm_sha, {
    lines(gm, lcl, lty=2, lwd=2,col="yellow4")
    lines(gm, ucl, lty=2, lwd=2,col="yellow4")
    lines(gm, pred, lty=1, lwd=2,col="yellow4")
  })

  legend("topright", leaflab2, pch=16,inset = 0.03, col=leafcol, bty='n', cex=1.25) 
  dev.copy2pdf(file="master_scripts/figures/photo_gm.pdf")
  dev.off()


#4: Photosynthesis vs cibar

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
  

  windows(10,8)
  par(mar=c(5,5,2,2), cex.axis=1.25, cex.lab=1.5)
  plot(Photo~ci_bar, data=sundat, pch=16, col=suncol, ylim=c(0,25), 
       xlim=c(150,350), xlab=cibarlab2, ylab=satlab, cex=1.25)
  #shade
  points(Photo~ci_bar, data=shadat, pch=16, col=shacol, cex=1.25)
    with(acib_sun, {
      lines(ci_bar, lcl, lty=2, lwd=2,col="forestgreen")
      lines(ci_bar, ucl, lty=2, lwd=2,col="forestgreen")
      lines(ci_bar, pred, lty=1, lwd=2,col="forestgreen")
    })
    with(acib_sha, {
      lines(ci_bar, lcl, lty=2, lwd=2,col="yellow4")
      lines(ci_bar, ucl, lty=2, lwd=2,col="yellow4")
      lines(ci_bar, pred, lty=1, lwd=2,col="yellow4")
    })

  legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol, bty='n', cex=1.25) 
  dev.copy2pdf(file="master_scripts/figures/photo_cibar.pdf")
  dev.off()
  
#5: Photosynthesis vs total conductance to CO2

  #SUN
  gmgs_sun_lm <- lmer(Photo~ gmgs + (1|chamber), data=gmgs_sun)
  
  boot_gmgsun <- bootMer(gmgs_sun_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
  gmgs_sun$lcl <- apply(boot_gmgsun$t, 2, quantile, 0.025)
  gmgs_sun$ucl <- apply(boot_gmgsun$t, 2, quantile, 0.975)
  gmgs_sun$pred <- predict(gmgs_sun_lm, re.form=NA)
  gmgs_sun <- arrange(gmgs_sun, gmgs)
  
  gmgs_sha_lm <- lmer(Photo~ gmgs + (1|chamber), data=gmgs_sha)
  
  boot_gmgssha <- bootMer(gmgs_sha_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
  gmgs_sha$lcl <- apply(boot_gmgssha$t, 2, quantile, 0.025)
  gmgs_sha$ucl <- apply(boot_gmgssha$t, 2, quantile, 0.975)
  gmgs_sha$pred <- predict(gmgs_sha_lm, re.form=NA)
  gmgs_sha <- arrange(gmgs_sha, gmgs)
  
  ##plot
  windows(10,8)
  par(mar=c(5,5,2,2), cex.axis=1.25, cex.lab=1.5)
  plot(Photo~gmgs, data=sundat, pch=16, col=suncol, ylim=c(0,25), 
       xlim=c(0,.8), xlab=totcondlab, ylab=satlab, cex=1.25)
  #shade
  points(Photo~gmgs, data=shadat, pch=16, col=shacol, cex=1.25)
  with(gmgs_sun, {
    lines(gmgs, lcl, lty=2, lwd=2,col="forestgreen")
    lines(gmgs, ucl, lty=2, lwd=2,col="forestgreen")
    lines(gmgs, pred, lty=1, lwd=2,col="forestgreen")
  })
  with(gmgs_sha, {
    lines(gmgs, lcl, lty=2, lwd=2,col="yellow4")
    lines(gmgs, ucl, lty=2, lwd=2,col="yellow4")
    lines(gmgs, pred, lty=1, lwd=2,col="yellow4")
  })

  legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol, bty='n', cex=1.25) 
  dev.copy2pdf(file="master_scripts/figures/totalcond.pdf")
  dev.off()
  
#####write calculated dfrs with bootstraop intervals for use later
write.csv(acc_sun, "calculated_data/bootstrap/acc_sun.csv", row.names=FALSE)
write.csv(acc_sha, "calculated_data/bootstrap/acc_sha.csv", row.names=FALSE)
write.csv(aci_sun, "calculated_data/bootstrap/aci_sun.csv", row.names=FALSE)
write.csv(aci_sha, "calculated_data/bootstrap/aci_sha.csv", row.names=FALSE) 
write.csv(agm_sun, "calculated_data/bootstrap/agm_sun.csv", row.names=FALSE)
write.csv(agm_sha, "calculated_data/bootstrap/agm_sha.csv", row.names=FALSE) 
write.csv(acib_sun, "calculated_data/bootstrap/acib_sun.csv", row.names=FALSE)
write.csv(acib_sha, "calculated_data/bootstrap/acib_sha.csv", row.names=FALSE)  

  write.csv(sundat, "calculated_data/bootstrap/sundat.csv", row.names=FALSE)
  write.csv(shadat, "calculated_data/bootstrap/shadat.csv", row.names=FALSE)  
