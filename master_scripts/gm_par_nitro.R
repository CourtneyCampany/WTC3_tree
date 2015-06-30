# source("functions and packages/functions.R")
# source("functions and packages/packages.R")
# source("master_scripts/plot_objects.R")
# 
# treatments <- read.csv("raw data/temp_trt.csv")

####read and format PAR data----------------------------------------------------------------------------------------------
par <- read.csv("raw data/par.csv")

#format function
par<- parformat(par)
  names(par)[3] <- "leaf"
  names(par)[7] <- "leaflight"
  par$chamber<- as.factor(par$chamber)

par_leaf <- par[par$drydown != "drought",]
  par_leaf <- droplevels(par_leaf)

par_leaf2 <- par_leaf[, c(1:3, 5:7)]

par_leaf3 <- par_leaf2[par_leaf2$leaflight !="shade-high",]

####read and format leaf N data----------------------------------------------------------------------------------------------
leaf_chem <- read.csv("raw data/leaf_chem.csv")
leaf_mass <- read.csv("raw data/leaf_data.csv")

#calculate nitrogen content in leaves
canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
  #add treatments  
  canopy_chem <- addtrt_func(canopy_chem)
  canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
  canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
  canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
  canopy_chem<- add_campaign(canopy_chem)

nitro <- canopy_chem[canopy_chem$drydown == "control",]  


####read and gm data---------------------------------------------------------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
sunshade <- gmes[gmes$leaflight != "shade-high",]  
  ###get average by id
  gm_agg <- summaryBy(gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=sunshade, FUN=mean, keep.names=TRUE)
  

####Combine datasets----------------------------------------------------------------------------------------------------
leafdat <- merge(gm_agg, nitro)  
leafdat2 <- merge(leafdat, par_leaf3)

###dfr with lights gm and par
fleck <- gmes[gmes$leaflight == "shade-high",]
fleck_agg <- summaryBy(gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=fleck, FUN=mean, keep.names=TRUE)
fleckpar <- par_leaf2[par_leaf2$leaflight == "shade-high",]

fleck_dat <- merge(fleck_agg, fleckpar[, c(1:4)])


####model equations for gm/nitro and PPFD by leaflight
library(nlme)
library(visreg)

# library(lme4)
# library(lmerTest)

# gmpar_sha <- lme(gm~ par, random=~1|chamber, data=leafdat2, subset=leaflight == "shade-low")
#   summary(gmpar_sha)
#   anova(gmpar_sha)
#   visreg(gmpar_sha)  
# 
# gmpar_sun <- lme(gm~ par, random=~1|chamber, data=leafdat2, subset=leaflight == "sun-high")
#   summary(gmpar_sun)
#   anova(gmpar_sun)
#   visreg(gmpar_sun)
gmpar_sun2 <- lm(gm~ par, data=leafdat2, subset=leaflight == "sun-high")
# anova(gmpar_sun, gmpar_sun2)
###lower AIC with chamber as random but models not different
  
# gmpar_fleck <- lme(gm~ par, random=~1|chamber, data=fleck_dat)
#   summary(gmpar_fleck)
#   anova(gmpar_fleck)
#   visreg(gmpar_fleck)   
gmpar_fleck2 <- lm(gm~ par, data=fleck_dat)
# anova(gmpar_fleck, gmpar_fleck2)
###lower AIC with chamber as random but models not different


##nitropar
# nitropar_sha <- lme(leafN_area~ par, random=~1|chamber, data=leafdat2, subset=leaflight == "shade-low")
#   summary(nitropar_sha)
#   anova(nitropar_sha)
#   visreg(nitropar_sha)  
# 
# nitropar_sun <- lme(leafN_area~ par, random=~1|chamber, data=leafdat2, subset=leaflight == "sun-high")
#   summary(nitropar_sha)
#   anova(nitropar_sha)
#   visreg(nitropar_sha)

##gmnitro
# gmnitro_sha <- lme(gm~ leafN_area, random=~1|chamber, data=leafdat2, subset=leaflight == "shade-low")
#   summary(gmnitro_sha)
#   anova(gmnitro_sha)
#   visreg(gmnitro_sha)  
# 
# gmnitro_sun <- lme(gm~ leafN_area, random=~1|chamber, data=leafdat2, subset=leaflight == "sun-high")
#   summary(gmnitro_sun)
#   anova(gmnitro_sun)
#   visreg(gmnitro_sun)


####PLOT relationships between gm~Narea, gm~PAR and Narea and Par---------------------------------------------------------------

# windows(8, 12)
par(mfrow=c(3,1), las=1, mgp=c(2, .5, 0), cex.lab=.8, cex.axis=.8, cex=1.25)

par(mar=c(3.5,5,1,2))
plot(gm~leafN_area, data=leafdat2, col=leaf, pch=c(16, 17)[pch=leafdat2$temp], ylim=c(0,.4), xlim=c(0,4.5), 
     ylab=gmlab, xlab=narealab)
ablineclip(h=mean(leafdat2[leafdat2$leaf=="shade", "gm"]), x1=min(leafdat2[leafdat2$leaf=="shade", "leafN_area"]),
           x2=max(leafdat2[leafdat2$leaf=="shade", "leafN_area"]), lty=5, lwd=2, col="yellow4")
ablineclip(h=mean(leafdat2[leafdat2$leaf=="sun", "gm"]), x1=min(leafdat2[leafdat2$leaf=="sun", "leafN_area"]),
           x2=max(leafdat2[leafdat2$leaf=="sun", "leafN_area"]), lty=5, lwd=2, col="forestgreen")
legend("topleft", leglab3, pch=rep(c(16,17),3), col=leafcols,inset = 0.01, bty='n',cex=.6)
##gm does not increase with N across leaf type yet there is still difference

par(mar=c(3.5,5,0,2))  
plot(gm~par, data=leafdat2, col=leaf, pch=c(16, 17)[pch=leafdat2$temp], ylim=c(0,.4), xlim=c(0,1800), 
     ylab=gmlab, xlab=parlab)
points(gm~par, data=fleck_dat, col=lightscol, pch=c(16, 17)[pch=fleck_dat$temp])

ablineclip(h=mean(leafdat2[leafdat2$leaf=="shade", "gm"]), x1=min(leafdat2[leafdat2$leaf=="shade", "par"]),
           x2=max(leafdat2[leafdat2$leaf=="shade", "par"]), lty=5, lwd=2, col="yellow4")

ablineclip(gmpar_fleck2, x1=min(fleck_dat[fleck_dat$leaf=="shade", "par"]),
           x2=max(fleck_dat[fleck_dat$leaf=="shade", "par"]), lty=5, lwd=2, col="darkorange2")

ablineclip(gmpar_sun2, x1=min(leafdat2[leafdat2$leaf=="sun", "par"]),
           x2=max(leafdat2[leafdat2$leaf=="sun", "par"]), lty=5, lwd=2, col="forestgreen")


##no effect on gm with slight changes to PAR at shade leaves, sunfleck and sun gm the same.  
##sun leaves decrease at high par but not sunfleck


par(mar=c(3.5,5,0,2))
plot(leafN_area~par, data=leafdat2, col=leaf, pch=c(16, 17)[pch=leafdat2$temp], ylim=c(0,4.5), xlim=c(0,1800), 
     ylab=narealab, xlab=parlab)
###no realtionship between PAR and leafN within leaf types
ablineclip(h=mean(leafdat2[leafdat2$leaf=="shade", "leafN_area"]), x1=min(leafdat2[leafdat2$leaf=="shade", "par"]),
           x2=max(leafdat2[leafdat2$leaf=="shade", "par"]), lty=5, col="yellow4", lwd=2)

ablineclip(h=mean(leafdat2[leafdat2$leaf=="sun", "leafN_area"]),  x1=min(leafdat2[leafdat2$leaf=="sun", "par"]),
       x2=max(leafdat2[leafdat2$leaf=="sun", "par"]), lty=5, col="forestgreen", lwd=2)

#  dev.copy2pdf(file="master_scripts/paper_figures/gm_nitro_par.pdf")
#  dev.off() 
  