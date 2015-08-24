source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")
library(mgcv)
library(lme4)

####panel a = ITE and VPD----------------------------------------------------------------------------------------------------

ite <- read.csv("calculated_data/gmes_wellwatered.csv")
g1_ite <- read.csv("calculated_data/g1_ite.csv")

###get average by id
ite_agg <- summaryBy(Photo+Cond+ Trmmol+VpdL ~ chamber+id+leaf +light+temp+leaflight+Month, 
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

###leaf delta plot
png(filename = "makepngs/leafdelta.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,6,1,1), las=1, cex.axis=1.5, cex.lab=2, mgp=c(3.5,1,0))
plot(c13 ~ leafN_area, data=canopy_chem, subset=leaf=="sun",  col=suncol, ylim=c(-33.5,-26), xlim=c(0,4.5),
     pch=c(16, 17)[pch=canopy_chem$temp], xlab=narealab, ylab=c13lab, cex=2.5)
points(c13 ~ leafN_area, data=canopy_chem,  subset=leaf=="shade", col=newshacol, pch=c(16, 17)[pch=canopy_chem$temp], cex=2.5)
ablineclip(chem_leaf, x1=min(canopy_chem$leafN_area), x2=max(canopy_chem$leafN_area), lwd=3, lty=2 )

legend("bottomright", leglab2, pch=c(16,17,16,17), col=trtcols,inset = 0.01, bty='n',cex=.7)
text(x=0, y=-26 ,"(b)", cex=1)

dev.off()



