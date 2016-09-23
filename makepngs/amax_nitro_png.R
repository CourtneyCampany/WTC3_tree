###Amax versus N
source("functions and packages/packages.R")
source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")

treatments <- read.csv("raw data/temp_trt.csv") 

###Photosythesis data-----------------------------------------------------------------------------------------------
photo <- read.csv("calculated_data/gmes_wellwatered.csv")
###get average by id
photo_agg <- summaryBy(Photo ~ chamber+id+leaf +light+temp+leaflight+Month, data=photo, FUN=mean, keep.names=TRUE)
photo2 <- summaryBy(Photo~ chamber+leaf+temp+leaflight+Month, data=photo_agg, FUN=mean, keep.names=TRUE)

###data for aci curves----------------------------------------------------------------------------------------------  
  
  #read ACi datasets 
acishade <- read.csv("raw data/shadeaci.csv")
acishade <- merge(acishade, treatments)
  
  #clean from previous script
  acishade_clean <- acishade[acishade$chamber != "ch02",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch07",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch09",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch11",]

  shade_redo <- read.csv("raw data/shadeaci_redo.csv")
  shade_redo <- merge(shade_redo, treatments)
  
  sunaci <- read.csv("raw data/sunaci.csv")
  sunaci <-merge(sunaci, treatments)
    
  #clean from previous script
  sunaci_clean <- sunaci[sunaci$chamber != "ch06", ]
  sunaci_clean2 <- sunaci_clean[sunaci_clean$chamber != "ch04", ]
  
  tdlaci2 <- read.csv("raw data/tdlaci2.csv")
    tdlaci2$chamber <- gsub("r", "", tdlaci2$chamber)
    tdlaci2 <-merge(tdlaci2, treatments)


#Pull Amax at common Ci (1800)
  shade_clean1800 <- acishade_clean[acishade_clean$CO2R > 1700,]
  shade_redo1800 <- shade_redo[shade_redo$CO2R > 1700,]
  
  shademax <- rbind(shade_clean1800[,c("chamber", "Photo", "CO2R", "Ci")], shade_redo1800[,c("chamber", "Photo", "CO2R","Ci")])
    shademax$leaf <- "shade"
  
  sunaci_1800 <- sunaci_clean2[sunaci_clean2$CO2R > 1700,]
  tdlaci1800 <- tdlaci2[tdlaci2$CO2R > 1700,]
  
  sunmax <- rbind(sunaci_1800[,c("chamber", "Photo", "CO2R", "Ci")], tdlaci1800[,c("chamber", "Photo", "CO2R", "Ci")])
    sunmax$leaf <- "sun"
  
amax_dat <- rbind(shademax, sunmax)  
  amax_dat <- merge(amax_dat, treatments)
  
###Narea data------------------------------------------------------------------------------------------------------
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

##Amax and Narea dataset  (with stats for results)----------------------------------------------------------------
Nagg <- summaryBy(leafN_area ~ chamber + leaf, data=canopy_chem, FUN=mean, keep.names = TRUE)

N_amax <- merge(Nagg, amax_dat, by= c("chamber", "leaf"))

nitroamax_mod <- lm(Photo~leafN_area, data=N_amax)


##Amax vs Narea
png(filename = "makepngs/amaxnitro.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,5.5,1,5), mgp=c(3,1,0), las=1,cex.axis=1.25, cex.lab=1.75)
plot(Photo~leafN_area, data=N_amax, col=leafcol3[as.factor(leaf)],xlim=c(01.5,3.5),ylim=c(15,35),
       pch=c(16, 17)[pch=N_amax$temp],xlab=narealab, ylab=amaxlab, cex=2)

ablineclip(nitroamax_mod, x1=min(N_amax$leafN_area), x2=max(N_amax$leafN_area), lwd=2, lty=3)

legend("topleft", leglab2, pch=c(16,17,16,17), col=c(suncol,suncol, 
      lightscol, lightscol),inset = 0.01, bty='n',cex=1.25) 

dev.off()
