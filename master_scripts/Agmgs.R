# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
# source("functions and packages/packages.R")

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

#### Gm vs A data: use gam for CI of non-linear relationship between A and gm----------------------------------------------

##read bootstrapped data previosuly ran from sunshade phys script
agm_sun <-   read.csv( "master_scripts/bootstrap_results/agm_sun.csv")
agm_sha <-  read.csv( "master_scripts/bootstrap_results/agm_sha.csv") 
agm_fleck <-  read.csv( "master_scripts/bootstrap_results/agm_fleck.csv") 
##testing stats for random model comparison
# library(nlme)
##linear model with chamber as random effect

# gmA_sun_mod <- lme(Photo ~ gm ,random=~1|chamber, data=sundat)
gmA_sun_mod2 <- lm(Photo ~ gm , data=sundat)

   # summary(gmA_sun_mod)
   # summary(gmA_sun_mod2)
   # anova(gmA_sun_mod,gmA_sun_mod2)

#   gmA_sha_mod <- lme(Photo~ gm  ,random=~1|chamber, data=shadat)
gmA_sha_mod2 <- lm(Photo~ gm  ,data=shadat)
   # summary(gmA_sha_mod)
   # summary(gmA_sha_mod2)
   # anova(gmA_sha_mod,gmA_sha_mod2)

#   gmA_fleck_mod <- lme(Photo~ gm  ,random=~1|chamber, data=fleckdat)
gmA_fleck_mod2 <- lm(Photo~ gm  ,data=fleckdat)
   # summary(gmA_fleck_mod)
   # summary(gmA_fleck_mod2)
   # anova(gmA_fleck_mod,gmA_fleck_mod2)

# ###need to test leaflight treatments against each other with A vs gm
# gmA_leaf <- lm(Photo ~ gm*leaflight, data=gm_agg)
# gmA_leaf2 <- lme(Photo ~ gm*leaflight ,random=~1|chamber, data=gm_agg)
# gmA_leaf3 <- aov(Photo ~ gm*leaflight, data=gm_agg)
# gmA_leaf4 <- aov(Photo ~ gm+leaflight, data=gm_agg)
# 
# summary(gmA_leaf)
# anova(gmA_leaf4)
# library(visreg)
# visreg(gmA_leaf)
# coef(gmA_leaf3)

###with smoothplot
palette(c(lightscol, shacol, suncol))
 
#windows(10, 12)
par(mfrow=c(2,1))
#gs
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(Photo~Cond, data=gm_agg,  col=leaflight, ylim=c(5,25), xlim=c(0,.5), xlab="", ylab="",pch="")
par(new=TRUE)
smoothplot(Cond, Photo, leaflight,data=gm_agg, kgam=5, R="chamber",ylim=c(5,25), xlim=c(0,.5),
             linecol=c(lightscol2, shacol2,suncol2),pch="", ylab=satlab, xlab=condlab)
points(Photo~Cond, data=gm_agg,  col=leaflight, pch=c(16, 17)[gm_sunsha$temp])
  
legend("topleft", alllab, pch=c(16,16,16,16,17), col=allcols,inset = 0.01, bty='n',cex=.8)
text(x=.5, y=24.5, "(a)", cex=1)

#gm
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(Photo~gm, data=gm_agg,  col=leaflight, ylim=c(5,25), xlim=c(0,.5), xlab=gmlab, ylab=satlab, pch="")
  predline(gmA_sun_mod2, col=suncol2,lwd=2)
  predline(gmA_sha_mod2, col=shacol2,lwd=2)
  predline(gmA_fleck_mod2, col=lightscol2,lwd=2)
  points(Photo~gm, data=gm_agg,  col=leaflight, pch=c(16, 17)[gm_sunsha$temp])

text(x=.5, y=24.5, "(b)", cex=1)
# dev.copy2pdf(file="master_scripts/paper_figures/Agmgs.pdf")
# dev.off()
   
   

  

  
   