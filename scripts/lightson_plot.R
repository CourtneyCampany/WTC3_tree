source("functions and packages/packages_md.R")
source("functions and packages/functions.R")
source("functions and packages/plot_objects_all.R")
treatments <- read.csv("raw data/temp_trt.csv")

gmes <- read.csv("calculated_data/gmes_WTC.csv")
gmpair<- read.csv("calculated_data/gmes_wtc_pair.csv")
gmwet <- read.csv("calculated_data/gmes_wellwatered.csv")
Ci_bar <- read.csv("calculated_data/Ci_bar.csv")
###for analysis first subset well watered and drought treatments
gm_drought <- gmes[gmes$drydown == "drought",]
gm_water <- gmes[gmes$drydown == "control",]

####  mixed effect model for lights on---------------------------------------------------------------------------------
library(lme4)
library(lmerTest)

gmwater_agg <- summaryBy(Photo +gm ~ id+Month+temp+drydown+chamber+leaflight, data=gm_water,FUN=mean, keep.names=TRUE)
gm_water_agg2 <- gmwater_agg[gmwater_agg$leaflight != "sun-high",]  
gm_water_agg2$rowseq <- seq(1:nrow(gm_water_agg2))
gm_water_agg2 <- droplevels(gm_water_agg2) 

rowrm <- c(9,36,39,102)
gm_water_agg3 <- gm_water_agg2[! gm_water_agg2$rowseq %in% c(9, 36,39, 102),] 
gm_water_sp <- dlply(gm_water_agg3, .(leaflight), function(x) c(x$pairid <- seq(1:nrow(x)), return(x)))

gm_water_agg4 <- rbind.fill(gm_water_sp) 
gm_water_agg4$pairid2 <- as.factor(paste("a", gm_water_agg4$pairid, sep="-"))

#run model with id (pairid) used as random effect
lightson_mod <-  lmer(Photo~ gm + (gm|pairid2), data=gm_water_agg4)

# Average relationship, by averaging group-wise slopes and intercepts
lightson_mod <- lmList(Photo ~ gm|pairid2, data=gm_water_agg4)
with(gm_water_agg4, plot(gm, Photo, col=leaflight))
b <- colMeans(coef(lightson_mod))
abline(b[1], b[2])

abline(lm(Photo ~ gm, data=gm_water_agg4), col="red")






summary(lightson_mod)


#####plots with lines between paired points---------------------------------------------------------------

##split by light (dfr for drought and watered shade leaves)

wet_ss <- gm_water[, c("chamber", "id", "Photo", "gm", "leaf", "light", "temp", "drydown", "leaflight", "Month")]
wet_ss2 <- wet_ss[wet_ss$leaf == "shade",]
  wet_sp <- dlply(wet_ss2, .(leaflight))
  
dry_ss <- gm_drought[, c("chamber", "id", "Photo", "gm", "leaf", "light", "temp", "drydown", "leaflight", "Month")]
dry_ss2 <- dry_ss[dry_ss$leaf == "shade",]
  dry_sp <- dlply(dry_ss2, .(leaflight))  

wet_high <- data.frame(wet_sp[1])
  wet_high_agg <- summaryBy(shade.high.Photo+shade.high.gm ~ shade.high.chamber+shade.high.Month, data = wet_high, 
                      FUN=mean, keep.names = TRUE)
dry_high <- data.frame(dry_sp[1])
  dry_high_agg <- summaryBy(shade.high.Photo+shade.high.gm ~ shade.high.chamber+shade.high.Month, data = dry_high, 
                            FUN=mean, keep.names = TRUE)
  
  names(wet_high_agg) <- c("chamber", "Month", "Photo.high", "gm.high")
  names(dry_high_agg) <- c("chamber", "Month", "Photo.high", "gm.high")

wet_low<- data.frame(wet_sp[2])
  wet_low_agg <- summaryBy(shade.low.Photo+shade.low.gm ~ shade.low.chamber+shade.low.Month, data = wet_low, 
                     FUN=mean, keep.names = TRUE)
  
dry_low<- data.frame(dry_sp[2])
  dry_low_agg <- summaryBy(shade.low.Photo+shade.low.gm ~ shade.low.chamber+shade.low.Month, data = dry_low, 
                           FUN=mean, keep.names = TRUE)  
  
  names(wet_low_agg) <- c("chamber", "Month", "Photo.low", "gm.low")
  names(dry_low_agg) <- c("chamber", "Month", "Photo.low", "gm.low")

##merge by pair
wet_pair <- merge(wet_high_agg, wet_low_agg, by=c("chamber","Month"), all=TRUE)
  #wet_pair2 <- wet_pair[complete.cases(wet_pair),]
  wet_pair2 <- na.omit(wet_pair)
  wet_pair2 <- arrange(wet_pair2, gm.low)
  wet_pair3 <- addtrt_func(wet_pair2)

dry_pair <- merge(dry_high_agg, dry_low_agg, by=c("chamber","Month"), all=TRUE)
  dry_pair2 <- na.omit(dry_pair)
  dry_pair2 <- arrange(dry_pair2, gm.low)
  dry_pair3 <- addtrt_func(dry_pair2)   
    
##split for lines
wetpair_sp <- split(wet_pair2, cumsum(1:nrow(wet_pair2)))
drypair_sp <- split(dry_pair2, cumsum(1:nrow(dry_pair2)))

##split but by temp trt
wet_at <- wet_pair3[wet_pair3$temp == "ambient",]
wet_et <- wet_pair3[wet_pair3$temp == "elevated",]

  wet_at_sp <- split(wet_at, cumsum(1:nrow(wet_at)))
  wet_et_sp <- split(wet_et, cumsum(1:nrow(wet_et)))
  
dry_at <- dry_pair3[dry_pair3$temp == "ambient",]
dry_et <- dry_pair3[dry_pair3$temp == "elevated",]
  
  dry_at_sp <- split(dry_at, cumsum(1:nrow(dry_at)))
  dry_et_sp <- split(dry_et, cumsum(1:nrow(dry_et)))

###Well watered
windows(10,8)
plot(Photo.high~gm.high, data=wet_pair3,  pch=16, col=lightscol, ylim=c(0,30), xlim=c(0,.3), 
     xlab=gmlab, ylab="", cex=1.5)
  points(Photo.low~gm.low, data=wet_pair3,pch=16, col=shacol, cex=1.5)

#l_ply(light_sp,  function(x) lines(x=c(x$gm.low,x$gm.high) , y=c(x$Photo.low,x$Photo.high),
#                    type='l', col="black", lty=1, lwd=1))

l_ply(wet_at_sp,  function(x) lines(x=c(x$gm.low,x$gm.high) , y=c(x$Photo.low,x$Photo.high),
                    type='l', col="blue", lty=3, lwd=2))
l_ply(wet_et_sp,  function(x) lines(x=c(x$gm.low,x$gm.high) , y=c(x$Photo.low,x$Photo.high),
                    type='l', col="red", lty=3, lwd=2))

###drought
  plot(Photo.high~gm.high, data=dry_pair2,  pch=16, col=lightscol, ylim=c(0,30), xlim=c(0,.3), 
       xlab=gmlab, ylab="", cex=1.5)
  points(Photo.low~gm.low, data=dry_pair2, pch=16, col=shacol, cex=1.5)
  
  #l_ply(light_sp,  function(x) lines(x=c(x$gm.low,x$gm.high) , y=c(x$Photo.low,x$Photo.high),
  #                    type='l', col="black", lty=1, lwd=1))
  
  l_ply(dry_at_sp,  function(x) lines(x=c(x$gm.low,x$gm.high) , y=c(x$Photo.low,x$Photo.high),
                                  type='l', col="blue", lty=3, lwd=2))
  l_ply(dry_et_sp,  function(x) lines(x=c(x$gm.low,x$gm.high) , y=c(x$Photo.low,x$Photo.high),
                                  type='l', col="red", lty=3, lwd=2))
  
  
  

  #plot low vs high light shade leaves-----------------------------------------------------------------------------
  
  ##water
  plot(Photo~gm, data=gm_water, subset=leaflight=="shade-high", pch=16, col=lightscol, ylim=c(0,30), xlim=c(0,.4), 
       xlab=gmlab, ylab="", cex=1.25)
  points(Photo~gm, data=gm_water, subset=leaflight=="shade-low",pch=16, col=shacol, cex=1.25)
  legend("topright", lightleg, pch=16,inset = 0.02, col=lightlab) 
  legend("topleft", "Shade Leaves (well watered)", bty='n') 
  title(ylab=satlab, mgp=ypos, cex=1.2)
  
  ##drought
  plot(Photo~gm, data=gm_drought, subset=leaflight=="shade-high", pch=16, col=lightscol, ylim=c(0,30), xlim=c(0,.4), 
       xlab=gmlab, ylab="", cex=1.25)
  points(Photo~gm, data=gm_drought, subset=leaflight=="shade-low",pch=16, col=shacol, cex=1.25)
  legend("topright", lightleg, pch=16,inset = 0.02, col=lightlab) 
  legend("topleft", "Shade Leaves (drought)", bty='n') 
  title(ylab=satlab, mgp=ypos, cex=1.2)
  
  
  