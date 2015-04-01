
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



###Photosynthesis vs Ci--------------------------------------------------------------------------------
library(lmerTest)

sundat <- gm_c13[gm_c13$leaflight =="sun-high",]
shadat <- gm_c13[gm_c13$leaflight =="shade-low",]

aci_sun <- sundat[, c("Photo", "Ci", "chamber")]
aci_sha<- shadat[, c("Photo", "Ci", "chamber")]

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


###aci graph and table---------------------------------------------------------------------------------
aci_leaf <- read.csv("calculated_data/tdl_aci.csv")

#simulated curves
library(plantecophys)

#simulate ACi curves for each leaf, ambient and elevated T
sunAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[3,3], Jmax=aci_leaf[3,4],PPFD=1408)
sunET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[4,3], Jmax=aci_leaf[4,4], PPFD=1408)
shaAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[1,3], Jmax=aci_leaf[1,4], PPFD=375)
shaET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[2,3], Jmax=aci_leaf[2,4],PPFD=375)


###panel plot
windows(14,8)
par(cex.axis=.96, cex.lab=1.2,mfrow=c(1,2),oma=c(0.1,0.1,0.1,0.1), las=1) 

par(mar=c(5,4,2,0))

plot(Photo~Ci, data=sundat, pch=16, col=suncol, ylim=c(0,25), xlim=c(0,350), xlab=cilab, ylab="", cex=1.25)
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
  title(ylab=satlab, mgp=ypos, cex=1.2)
  legend("topright", leaflab2, pch=16,inset = 0.03, col=leafcol) 

par(mar=c(5,0,2,2))
#panel2
plot(sunAT_sim$Ci, sunAT_sim$ALEAF, col="forestgreen", pch=21,  cex=1.1,xlab=cilab, ylab="", 
     type="l", lwd=2,yaxt='n')
  points(sunET_sim$Ci, sunET_sim$ALEAF, col="forestgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  points(shaAT_sim$Ci, shaAT_sim$ALEAF, col="yellowgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
  points(shaET_sim$Ci, shaET_sim$ALEAF, col="yellowgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  legend("topleft", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)


