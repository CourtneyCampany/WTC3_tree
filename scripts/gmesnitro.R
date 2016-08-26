source("functions and packages/packages.R")
source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
treatments <- read.csv("raw data/temp_trt.csv") 
##reviewer: N and gm for aquaporin

###Narea data------------------------------------------------------------------------------------------------------
leaf_chem <- read.csv("raw data/leaf_chem.csv")
leaf_mass <- read.csv("raw data/leaf_data.csv")

canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
  #add treatments  
  canopy_chem <- addtrt_func(canopy_chem)
  canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
  canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
  canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
  canopy_chem<- add_campaign(canopy_chem)

canopy_chem2 <- canopy_chem[canopy_chem$drydown=="control",]
  canopy_chem2$leafN_mass <- with(canopy_chem2, leafN_area/lma)
canopy_chem3 <- canopy_chem2[, c("chamber", "Month", "leaf", "leafN_area", "leafN_mass")]

###gm data-------------------------------------------------------------------------------------------------------------
gasex <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <- summaryBy(gm_bar+Cond+Photo ~ chamber+id+leaf +light+temp+leaflight+Month, 
                    data=gasex, FUN=mean, keep.names=TRUE)

gmnorm <- gm_agg[gm_agg$leaflight != "shade-high",]
gmhigh <- gm_agg[gm_agg$leaflight != "shade-low",]

##compare N concentration to gmes (without all=TRUE there is no drought trt)
nitrogm_norm <- merge(canopy_chem3, gmnorm)
nitrogm_high <- merge(canopy_chem3, gmhigh)


#---------------------------------------------------------------------------------------------------------------------------
windows()
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(gm_bar~leafN_area, data=nitrogm_norm, col=leafcol2[leaf], xlab=narealab, ylab=gmlab2,pch=c(16, 17)[temp])
legend("topleft", leglab2, pch=c(16,17,16,17), col=trtcols,inset = 0.01, bty='n',cex=.8)
# predline(lm(gm_bar~leafN_area, data=nitrogm_norm[nitrogm_norm$leaf=="sun",]), col=suncol2,lwd=2)
# predline(lm(gm_bar~leafN_area, data=nitrogm_norm[nitrogm_norm$leaf=="shade",]), col=shacol2,lwd=2)

predline(lm(gm_bar~leafN_area, data=nitrogm_norm), col="black",lwd=2)
dev.copy2pdf(file="gmN.pdf")
dev.off()  
anova(lm(gm_bar~leafN_area, data=nitrogm_norm))

anova(lm(gm_bar~leafN_area, data=nitrogm_norm[nitrogm_norm$leaf=="sun",]))
anova(lm(gm_bar~leafN_area, data=nitrogm_norm[nitrogm_norm$leaf=="shade",]))
#with sunfleck shade gmes
#plot(gm_bar~leafN_area, data=nitrogm_high, pch=16, col=leafcol3[leaf])
#nmass
#plot(gm_bar~leafN_mass, data=nitrogm_norm, col=leafcol2[leaf], xlab=narealab, ylab=gmlab2,pch=c(16, 17)[temp])






