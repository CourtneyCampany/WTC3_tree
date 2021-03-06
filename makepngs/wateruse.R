source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")
library(mgcv)
library(lme4)

####panel a = ITE and VPD----------------------------------------------------------------------------------------------------

ite <- read.csv("calculated_data/gmes_wellwatered.csv")
g1_ite <- read.csv("calculated_data/g1_ite.csv")

###get average by id
ite_agg <- summaryBy(Photo+Cond+ Trmmol+VpdL ~ chamber + id + leaf + light + temp + leaflight + Month, 
                     data=ite, FUN=mean, keep.names=TRUE)

#add iWUE
ite_agg$ite <- with(ite_agg, Photo/Trmmol)

##remove shade-high
ite_sunsha <- ite_agg[ite_agg$leaflight != "shade-high",]
ite_sunsha <- droplevels(ite_sunsha)


##dfr with lights on
ite_lightson <- ite_agg[ite_agg$leaflight == "shade-high",]
ite_lightson <- droplevels(ite_lightson)

####panel B: 13C and N area--------------------------------------------------------------------------------------------------
#read data
treatments <- read.csv("raw data/temp_trt.csv")

leaf_chem <- read.csv("raw data/leaf_chem.csv")
leaf_mass <- read.csv("raw data/leaf_data.csv")

#get vcmax per chamber 
aciparam <- read.csv("calculated_data/aciparameters.csv")

#calculate nitrogen content in leaves
canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
#add treatments  
canopy_chem <- addtrt_func(canopy_chem)
canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
canopy_chem<- add_campaign(canopy_chem)

chem_leaf <- lm(c13 ~ leafN_area, data=canopy_chem)



png(filename = "makepngs/wue1.png", width = 11, height = 8.5, units = "in", res= 400)
 
par(mar=c(5,5.5,1,1),  mgp=c(3,1,0), las=1, cex.lab=1.75, cex.axis=1.5)
plot(ite~VpdL, data=ite_sunsha, subset=leaflight=="sun-high",  col=suncol, xlab=vpdlab, ylab=itelab,
     xlim=c(0,4), ylim=c(0,16),  pch=c(16, 17)[pch=ite_sunsha$temp], cex=2)
  points(ite~VpdL, data=ite_sunsha, subset=leaflight=="shade-low", col=shacol, pch=c(16, 17)[pch=ite_sunsha$temp], cex=2) 
  # # points(ite~VpdL, data=ite_lightson, col=lightscol, pch=c(16, 17)[pch=ite_lightson$temp])
  # ##now add curves
  # p2 <- g1_ite[1:2,1]
  # f <- function(VpdL, g1)(400*102.3) / (1.6*(g1*sqrt(VpdL)+VpdL))/1000
  # for(i in 1:2)curve(f(x, p2[i]), from=min(ite_lightson$VpdL),to= max(ite_lightson$VpdL),add=T, col=lightscol, lty=ltys[i], lwd=2)
  # 
  p <- g1_ite[3:6,1]
  f <- function(VpdL, g1)(400*102.3) / (1.6*(g1*sqrt(VpdL)+VpdL))/1000
  for(i in 1:4)curve(f(x, p[i]), add=T, col=colaci2[i], lty=ltys[i], lwd=2,from=min(ite_sunsha$VpdL),to= max(ite_sunsha$VpdL))

  legend("topright", c("Sun", "Shade-Low Light", "AT", "ET"),
         pch=c(16,16,16,17), col=c(suncol, shacol, "black", "black"),
         lty=c(-1,-1,1,2),bty='n',cex=1.25)
  
  dev.off()
  
  
png(filename = "makepngs/wue2.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,5.5,1,1),  mgp=c(3,1,0), las=1,cex.axis=1.5, cex.lab=1.75)
  plot(c13 ~ leafN_area, data=canopy_chem, subset=leaf=="sun",  col=suncol, ylim=c(-33.5,-26), xlim=c(0,4.5),
       pch=c(16, 17)[pch=canopy_chem$temp], xlab=narealab, ylab=c13lab, cex=2)
  points(c13 ~ leafN_area, data=canopy_chem,  subset=leaf=="shade", col=shacol, 
         pch=c(16, 17)[pch=canopy_chem$temp], cex=2)
  ablineclip(chem_leaf, x1=min(canopy_chem$leafN_area), x2=max(canopy_chem$leafN_area), lwd=2, lty=2 )
  
  legend("topleft", leglab2, pch=c(16,17,16,17), col=trtcols, bty='n',cex=1.25)

dev.off()
  

