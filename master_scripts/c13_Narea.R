source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
library(doBy)

#read dataq
treatments <- read.csv("raw data/temp_trt.csv")

leaf_chem <- read.csv("raw data/leaf_chem.csv")

leaf_mass <- read.csv("raw data/leaf_data.csv")

leaf_par <- read.csv("raw data/par.csv")
  leaf_par <- parformat(leaf_par)


#calculate nitrogen content in leaves
canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
  #add treatments  
  canopy_chem <- addtrt_func(canopy_chem)
  canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
  canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
  canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
  canopy_chem<- add_campaign(canopy_chem)

canopy_agg2 <- summaryBy(c13+ lma+ leafN + leafN_area ~ leaf+campaign+temp, data=canopy_chem, FUN=c(mean,se))

###plotting

palette(c(shacol, suncol))

###c13 vs Narea

#windows(11,8)
png(filename = "markdown/c13_nitro.png", width = 11, height = 8.5, units = "in", res= 400)
par(mar=c(5,5,2,2), cex=1.25, cex=1.25, las=1)
plot(c13 ~ leafN_area, data=canopy_chem,  col=as.factor(leaf), ylim=c(-33,-26), xlim=c(0,4.5), pch=c(16, 17)[pch=canopy_chem$temp],
     cex=1.25, xlab=narealab, ylab=c13lab)

legend("topleft", leglab2, pch=c(16,17,16,17), col=colaci,inset = 0.01, bty='n',cex=1)

#dev.copy2pdf(file="master_scripts/figures/c13_nitro.pdf")
dev.off()
