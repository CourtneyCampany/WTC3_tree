# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
# source("functions and packages/packages.R")
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

# ite_mod <- lme(ite ~ leaf ,random=~1|chamber, data=ite_sunsha)
# summary(ite_mod)
# ite_sun <- mean(ite_sunsha[ite_sunsha$leaf == "sun", "ite"])
# ite_sha<- mean(ite_sunsha[ite_sunsha$leaf == "shade", "ite"])

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

# chem_mod <- lm(c13 ~ leafN_area, data=canopy_chem)
# summary(chem_mod)

chem_leaf <- lm(c13 ~ leafN_area, data=canopy_chem)


#### Multi panel plot of WUE and 13C--------------------------------------------------------------------------------------
# windows(7,7)
#png(filename = "figpic/wue.png", width = 11, height = 8.5, units = "in", res= 400)

par(mfrow=c(2,1))
 
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(ite~VpdL, data=ite_sunsha, subset=leaflight=="sun-high",  col=suncol, xlab=vpdlab, ylab=itelab,
     xlim=c(0,4), ylim=c(0,16),  pch=c(16, 17)[pch=ite_sunsha$temp])
  points(ite~VpdL, data=ite_sunsha, subset=leaflight=="shade-low", col=shacol, pch=c(16, 17)[pch=ite_sunsha$temp]) 
  points(ite~VpdL, data=ite_lightson, col=lightscol, pch=c(16, 17)[pch=ite_lightson$temp])
  ##now add curves
  p2 <- g1_ite[1:2,1]
  f <- function(VpdL, g1)(400*102.3) / (1.6*(g1*sqrt(VpdL)+VpdL))/1000
  for(i in 1:2)curve(f(x, p2[i]), from=min(ite_lightson$VpdL),to= max(ite_lightson$VpdL),add=T, col=lightscol, lty=ltys[i], lwd=2)
  
  p <- g1_ite[3:6,1]
  f <- function(VpdL, g1)(400*102.3) / (1.6*(g1*sqrt(VpdL)+VpdL))/1000
  for(i in 1:4)curve(f(x, p[i]), add=T, col=colaci2[i], lty=ltys[i], lwd=2,from=min(ite_sunsha$VpdL),to= max(ite_sunsha$VpdL))
  
  text(x=0, y=15.5 ,"(a)", cex=1)
  
  legend("topright", alllab, pch=c(16,16,16,16,17), col=c(suncol, shacol, lightscol2, "black", "black"),lty=c(-1,-1,-1,1,2),
         bty='n',cex=.8)

##panel2
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
  plot(c13 ~ leafN_area, data=canopy_chem, subset=leaf=="sun",  col=suncol, ylim=c(-33.5,-26), xlim=c(0,4.5),
       pch=c(16, 17)[pch=canopy_chem$temp], xlab=narealab, ylab=c13lab)
  points(c13 ~ leafN_area, data=canopy_chem,  subset=leaf=="shade", col=shacol, pch=c(16, 17)[pch=canopy_chem$temp])
  ablineclip(chem_leaf, x1=min(canopy_chem$leafN_area), x2=max(canopy_chem$leafN_area), lwd=2, lty=2 )
  
  legend("bottomright", leglab2, pch=c(16,17,16,17), col=trtcols, bty='n',cex=.8)
  text(x=0, y=-26.25 ,"(b)", cex=1)

  

