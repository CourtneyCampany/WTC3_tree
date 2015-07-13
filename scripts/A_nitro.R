source("functions and packages/packages.R")
source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")

treatments <- read.csv("raw data/temp_trt.csv") 

photo <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
photo_agg <- summaryBy(Photo ~ chamber+id+leaf +light+temp+leaflight+Month, data=photo, FUN=mean, keep.names=TRUE)
photo2 <- summaryBy(Photo~ chamber+leaf+temp+leaflight+Month, data=photo_agg, FUN=mean, keep.names=TRUE)


###plot 13 vs Narea------------------------------------------------------------------------------------------------------
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
  
canopy_chem2 <- canopy_chem[canopy_chem$drydown=="control",]
canopy_chem3 <- canopy_chem2[, c("chamber", "Month", "leaf", "leafN_area")]
###merge photo with leaf N datasets

Anitro <- merge(photo2, canopy_chem3, by=c("chamber", "Month", "leaf"))  

nitrovc_mod <- lm(Photo~leafN_area, data=Anitro[Anitro$leaflight != "shade-high",])

###plottinng

windows(8,6)
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(Photo ~ leafN_area, data=Anitro[Anitro$leaflight=="sun-high",], col=suncol,  xlim=c(0,4), ylim=c(0,30),
     pch=c(16, 17)[pch=Anitro$temp], ylab=satlab, xlab=narealab)
  points(Photo ~ leafN_area, data=Anitro[Anitro$leaflight=="shade-low",], col=shacol,  pch=c(16, 17)[pch=Anitro$temp])
  ablineclip(nitrovc_mod, x1=min(Anitro$leafN_area), x2=max(Anitro$leafN_area), lwd=2, lty=3)
  legend("topleft", leglab2, pch=c(16,17,16,17), col=trtcols,inset = 0.01, bty='n',cex=.8)

dev.copy2pdf(file="master_scripts/paper_figures/A_nitro.pdf")
dev.off()