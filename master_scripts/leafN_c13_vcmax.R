 source("functions and packages/functions.R")
 source("master_scripts/plot_objects.R")
 library(doBy)

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

# canopy_agg2 <- summaryBy(c13+ lma+ leafN + leafN_area ~ leaf+campaign+temp, data=canopy_chem, FUN=c(mean,se))
  
##panel2 data, N and aci by chamber
  
Nagg <- summaryBy(leafN_area ~ chamber + leaf, data=canopy_chem)
  
N_aci <- merge(aciparam, Nagg, by= c("chamber", "leaf"))

  

###plot 13 vs Narea------------------------------------------------------------------------------------------------------
palette(c(shacol, suncol))
 
#windows(8,8)
par(mfrow=c(2,1))  
  
#png(filename = "markdown/c13_nitro.png", width = 11, height = 8.5, units = "in", res= 400)
par(mar=c(0,5,2,2), cex=1.25, las=1, cex.axis=.8, cex.lab=.96)
plot(c13 ~ leafN_area, data=canopy_chem, subset=leaf=="sun",  col=suncol, ylim=c(-33.5,-26), xlim=c(0,4.5), xaxt="n",
     pch=c(16, 17)[pch=canopy_chem$temp], xlab="", ylab=c13lab)
points(c13 ~ leafN_area, data=canopy_chem,  subset=leaf=="shade", col=shacol, pch=c(16, 17)[pch=canopy_chem$temp])

legend("topleft", leglab2, pch=c(16,17,16,17), col=colaci,inset = 0.01, bty='n',cex=.8)

par(mar=c(5,5,0,2), cex=1.25, las=1,cex.axis=1.25, cex.axis=.8, cex.lab=.96)
plot(Vcmax~leafN_area.mean, data=N_aci, col=as.factor(leaf),  ylim=c(0, 133),xlim=c(0,4.5),
     pch=c(16, 17)[pch=N_aci$temp],xlab=narealab, ylab=vclab)

# dev.copy2pdf(file="master_scripts/paper_figures/leafN_c13_vcmax.pdf")
# dev.off()
