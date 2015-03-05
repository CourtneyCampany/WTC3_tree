source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought)

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
Ci_bar <- read.csv("calculated_data/Ci_bar.csv")


##calculate CC
gmes$Cc<- with(gmes, Ci-Photo/gm)
gmes$cc_ci<- with(gmes, Ci/Cc)

##need to combine Ci_bar with CC and gm

###get average by id
gm_agg <- summaryBy(Photo+Cond+Ci+Cc+gm+VpdL+xsi+DELTA+cc_ci ~ id+leaf +light+temp+leaflight, 
                    data=gmes, FUN=mean, keep.names=TRUE)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
  gm_sunsha <- droplevels(gm_sunsha)
  gm_sunsha$id <- gsub("-high", "", gm_sunsha$id)
  gm_sunsha$id <- gsub("-low", "", gm_sunsha$id)

##merge
gm_c13 <- merge(gm_sunsha, Ci_bar[, c(2,8)], by="id")
  gm_c13$Cc_bar <- with(gm_c13, ci_bar-Photo/gm)

###plot objects
#PLot  
suncol <- alpha("forestgreen", alpha=.75)
shacol <- alpha("yellow4", alpha=.75)
leafcol <- c(suncol, shacol)
leaflab2 <- c("Sun", "Shade")

###Photosynthesis vs Cc--------------------------------------------------------------------------------

#SUN leaves
  ##photo not different by temp treatment, so not used
Acc_sun_lm <- lm(Photo~ Cc, data=gm_c13,subset=leaflight=="sun-high")
  summary(Acc_sun_lm)
  confint(Acc_sun_lm)
  visreg(Acc_sun_lm)
  #can get CI from here but limited by number of data points
  #sun_pred <-predict(Acc_sun_lm, interval="confidence")

  ##instead predict Photo over a sequence of CC using the model fit

  #get apprpriate vector CC from sun leaves
  ccdat <- gm_c13[gm_c13$leaflight=="sun-high", "Cc"]
  #generate sequence and then predict
  ccsun_seq <- seq(min(ccdat), max(ccdat), length=101)
  ccsun_pred <- predict.lm(Acc_sun_lm, newdata=data.frame(Cc=ccsun_seq), interval="confidence")

#SHADE leaves
Acc_shade_lm <- lm(Photo~ Cc, data=gm_c13,subset=leaflight=="shade-low")
  summary(Acc_sha_lm)
  confint(Acc_sha_lm)
  visreg(Acc_sha_lm)

  #get apprpriate vector CC from sun leaves
  ccdat2 <- gm_c13[gm_c13$leaflight=="shade-low", "Cc"]
  #generate sequence and then predict
  ccsha_seq <- seq(min(ccdat2), max(ccdat2), length=101)
  ccsha_pred <- predict.lm(Acc_shade_lm, newdata=data.frame(Cc=ccsha_seq), interval="confidence")


##plot
windows(10,8)
plot(Photo~Cc, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
     xlim=c(0,350), xlab=cclab, ylab="", cex=1.25)
  ablineclip(Acc_sun_lm, lty=1, x1=min(gm_c13[gm_c13$leaf=="sun","Cc"]), x2=max(gm_c13[gm_c13$leaf=="sun","Cc"]), 
           col="forestgreen", lwd=2)
  lines(ccsun_seq, ccsun_pred[,2], lty=2, lwd=2, col="forestgreen")
  lines(ccsun_seq, ccsun_pred[,3], lty=2, lwd=2, col="forestgreen")
  #shade
  points(Photo~Cc, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  ablineclip(Acc_sha_lm, lty=1, x1=min(gm_c13[gm_c13$leaflight=="shade-low","Cc"]), 
             x2=max(gm_c13[gm_c13$leaflight=="shade-low","Cc"]), col="yellow4", lwd=2)
  lines(ccsha_seq, ccsha_pred[,2], lty=2, lwd=2,col="yellow4")
  lines(ccsha_seq, ccsha_pred[,3], lty=2, lwd=2,col="yellow4")

  title(ylab=satlab, mgp=ypos, cex=1.2)
  legend("topright", leaflab2, pch=16,inset = 0.03, col=leafcol) 


###Photosynthesis vs Ci--------------------------------------------------------------------------------

#SUN leaves
Aci_sun_lm <- lm(Photo~ Ci, data=gm_c13,subset=leaflight=="sun-high")
  summary(Aci_sun_lm)
  confint(Aci_sun_lm)
  visreg(Aci_sun_lm)

  #predict
  #get apprpriate vector CC from sun leaves
  cidat <- gm_c13[gm_c13$leaflight=="sun-high", "Ci"]
  #generate sequence and then predict
  cisun_seq <- seq(min(cidat), max(cidat), length=101)
  cisun_pred <- predict.lm(Aci_sun_lm, newdata=data.frame(Ci=cisun_seq), interval="confidence")

#SHADE leaves
Aci_shade_lm <- lm(Photo~ Ci, data=gm_c13, subset=leaflight=="shade-low")
  summary(Aci_shade_lm)
  confint(Aci_shade_lm)
  visreg(Aci_shade_lm)
  
  #get apprpriate vector CC from sun leaves
  cidat2 <- gm_c13[gm_c13$leaflight=="shade-low", "Ci"]
  #generate sequence and then predict
  cisha_seq <- seq(min(cidat2), max(cidat2), length=101)
  cisha_pred <- predict.lm(Aci_shade_lm, newdata=data.frame(Ci=cisha_seq), interval="confidence")


windows(10,8)
plot(Photo~Ci, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
     xlim=c(0,350), xlab=cilab, ylab="", cex=1.25)
  ablineclip(Aci_sun_lm, lty=1, x1=min(gm_c13[gm_c13$leaf=="sun","Ci"]), x2=max(gm_c13[gm_c13$leaf=="sun","Ci"]), 
             col="forestgreen", lwd=2)
  lines(cisun_seq, cisun_pred[,2], lty=2, lwd=2, col="forestgreen")
  lines(cisun_seq, cisun_pred[,3], lty=2, lwd=2, col="forestgreen")
  #shade
  points(Photo~Ci, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)

  ablineclip(Aci_shade_lm, lty=1, x1=min(gm_c13[gm_c13$leaflight=="shade-low","Ci"]), 
             x2=max(gm_c13[gm_c13$leaflight=="shade-low","Ci"]), col="yellow4", lwd=2)
  lines(cisha_seq, cisha_pred[,2], lty=2, lwd=2,col="yellow4")
  lines(cisha_seq, cisha_pred[,3], lty=2, lwd=2,col="yellow4")
  
  title(ylab=satlab, mgp=ypos, cex=1.2)
  legend("topright", leaflab2, pch=16,inset = 0.03, col=leafcol) 



###Photosynthesis vs gm-------------------------------------------------------------------------------

#SUN leaves
Agm_sun_lm <- lm(Photo~ gm, data=gm_c13,subset=leaflight=="sun-high")
summary(Agm_sun_lm)
confint(Agm_sun_lm)
visreg(Agm_sun_lm)

#predict
#get apprpriate vector CC from sun leaves
gmdat <- gm_c13[gm_c13$leaflight=="sun-high", "gm"]
#generate sequence and then predict
gmsun_seq <- seq(min(gmdat), max(gmdat), length=101)
gmsun_pred <- predict.lm(Agm_sun_lm, newdata=data.frame(gm=gmsun_seq), interval="confidence")

#SHADE leaves
Agm_sha_lm <- lm(Photo~ gm, data=gm_c13, subset=leaflight=="shade-low")
summary(Agm_sha_lm)
confint(Agm_sha_lm)
visreg(Agm_sha_lm)

#get apprpriate vector CC from sun leaves
gmdat2 <- gm_c13[gm_c13$leaflight=="shade-low", "gm"]
#generate sequence and then predict
gmsha_seq <- seq(min(gmdat2), max(gmdat2), length=101)
gmsha_pred <- predict.lm(Agm_sha_lm, newdata=data.frame(gm=gmsha_seq), interval="confidence")


windows(10,8)
plot(Photo~gm, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
     xlim=c(0,.45), xlab=gmlab, ylab="", cex=1.25)
ablineclip(Agm_sun_lm, lty=1, x1=min(gm_c13[gm_c13$leaf=="sun","gm"]), x2=max(gm_c13[gm_c13$leaf=="sun","gm"]), 
           col="forestgreen", lwd=2)
lines(gmsun_seq, gmsun_pred[,2], lty=2, lwd=2, col="forestgreen")
lines(gmsun_seq, gmsun_pred[,3], lty=2, lwd=2, col="forestgreen")
#shade
points(Photo~gm, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)

ablineclip(Agm_sha_lm, lty=1, x1=min(gm_c13[gm_c13$leaflight=="shade-low","gm"]), 
           x2=max(gm_c13[gm_c13$leaflight=="shade-low","gm"]), col="yellow4", lwd=2)
lines(gmsha_seq, gmsha_pred[,2], lty=2, lwd=2,col="yellow4")
lines(gmsha_seq, gmsha_pred[,3], lty=2, lwd=2,col="yellow4")

title(ylab=satlab, mgp=ypos, cex=1.2)
legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol) 




###Photosynthesis vs cibar-------------------------------------------------------------------------------

#SUN leaves
Acib_sun_lm <- lm(Photo~ ci_bar, data=gm_c13,subset=leaflight=="sun-high")
summary(Acib_sun_lm)
confint(Acib_sun_lm)
visreg(Acib_sun_lm)

#predict
#get apprpriate vector CC from sun leaves
cibdat <- gm_c13[gm_c13$leaflight=="sun-high", "ci_bar"]
#generate sequence and then predict
cibsun_seq <- seq(min(cibdat), max(cibdat), length=101)
cibsun_pred <- predict.lm(Acib_sun_lm, newdata=data.frame(ci_bar=cibsun_seq), interval="confidence")

#SHADE leaves
Acib_sha_lm <- lm(Photo~ ci_bar, data=gm_c13, subset=leaflight=="shade-low")
summary(Acib_sha_lm)
confint(Acib_sha_lm)
visreg(Acib_sha_lm)

#get apprpriate vector CC from sun leaves
cibdat2 <- gm_c13[gm_c13$leaflight=="shade-low", "ci_bar"]
#generate sequence and then predict
cibsha_seq <- seq(min(cibdat2), max(cibdat2), length=101)
cibsha_pred <- predict.lm(Acib_sha_lm, newdata=data.frame(ci_bar=cibsha_seq), interval="confidence")


windows(10,8)
plot(Photo~ci_bar, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,25), 
     xlim=c(150,350), xlab=cibarlab2, ylab="", cex=1.25)
ablineclip(Acib_sun_lm, lty=1, x1=min(gm_c13[gm_c13$leaf=="sun","ci_bar"]), x2=max(gm_c13[gm_c13$leaf=="sun","ci_bar"]), 
           col="forestgreen", lwd=2)
lines(cibsun_seq, cibsun_pred[,2], lty=2, lwd=2, col="forestgreen")
lines(cibsun_seq, cibsun_pred[,3], lty=2, lwd=2, col="forestgreen")
#shade
points(Photo~ci_bar, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)

ablineclip(Acib_sha_lm, lty=1, x1=min(gm_c13[gm_c13$leaflight=="shade-low","ci_bar"]), 
           x2=max(gm_c13[gm_c13$leaflight=="shade-low","ci_bar"]), col="yellow4", lwd=2)
lines(cibsha_seq, cibsha_pred[,2], lty=2, lwd=2,col="yellow4")
lines(cibsha_seq, cibsha_pred[,3], lty=2, lwd=2,col="yellow4")

title(ylab=satlab, mgp=ypos, cex=1.2)
legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol) 





