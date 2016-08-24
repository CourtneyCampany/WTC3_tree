source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

###Read data and format
leafK <- read.csv("calculated_data/leafK_nodrought.csv")

vla=7.35 #vein density of E.glob

leaf_mass <- read.csv("raw data/leaf_data.csv")
  leaf_mass$lma <- with(leaf_mass, leaf_mass/(leaf_area/10000))

leafsun <- leaf_mass[leaf_mass$leaf=="sun",]
  sunpre <- leafsun[leafsun$wp_type=="pre",]
    names(sunpre)[6] <- "predawn"
    names(sunpre)[9] <- "lma_predawn"
  sunmid <- leafsun[leafsun$wp_type=="mid",]
    names(sunmid)[6] <- "midday"
    names(sunmid)[9] <- "lma_midday"
  sun_dat <-  merge(sunpre[,c(1,3:4,6,9)], sunmid[,c(1,3:4,6,9)])

leafshade<- leaf_mass[leaf_mass$leaf=="shade",]
  shapre <- leafshade[leafshade$wp_type=="pre",]
    names(shapre)[6] <- "predawn"
    names(shapre)[9] <- "lma_predawn"
  shamid <- leafshade[leafshade$wp_type=="mid",]
    names(shamid)[6] <- "midday"
    names(shamid)[9] <- "lma_midday"
  shade_dat <-  merge(shapre[,c(1,3:4,6,9)], shamid[,c(1,3:4,6,9)])  
  
leaf_dat <- rbind(shade_dat, sun_dat)  
    
gasex <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <- summaryBy(gm_bar+Cond+Photo ~ chamber+id+leaf +light+temp+leaflight+Month, 
                    data=gasex, FUN=mean, keep.names=TRUE)
gm_norm <- gm_agg[gm_agg$leaflight != "shade-high",]

##this merged dataset will exclude drought treatments for water potential data
leaf_dat2 <- merge(leaf_dat, gm_norm[, c(1,3,5,7:10)])
  leaf_dat2$kleaf <- with(leaf_dat2, gm_bar + .001*lma_midday + .01*vla - .293)

leafdat3 <- merge(leaf_dat2, leafK[,c(1:3,7,12)])

##plots--------------------------------------------------------------------------------------------------------------

plot(kleaf~leaf, data=leaf_dat2)
plot(leafK~leaf, data=leafK)

plot(Photo~leafK, data=leafdat3, col=leafcol[col=leaf], pch=16)
plot(gm_bar~leafK, data=leafdat3, col=leafcol[col=leaf], pch=16)
plot(Cond~leafK, data=leafdat3, col=leafcol[col=leaf], pch=16)

plot((Photo/lma_midday)~leafK, data=leafdat3, col=leafcol[col=leaf], pch=16)
plot(gm_bar/lma_midday~leafK, data=leafdat3, col=leafcol[col=leaf], pch=16)
plot(Cond/lma_midday~leafK, data=leafdat3, col=leafcol[col=leaf], pch=16)

###gmes vs gs
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(gm_bar~Cond, data=leafdat3, ylab = gmlab2, xlab=condlab, xlim=c(0, .5), type='n')

  predline(lm(gm_bar~Cond, data=leafdat3[leafdat3$leaf=="sun",]), col=suncol2,lwd=2)
  predline(lm(gm_bar~Cond, data=leafdat3[leafdat3$leaf=="shade",]), col=shacol2,lwd=2)
  predline(lm(gm_bar~Cond, gm_agg[gm_agg$leaflight == "shade-high",]), col=lightscol2,lwd=2)
  
  points(gm_bar~Cond, data=leafdat3[leafdat3$leaf == "sun",], col=suncol, pch=c(16, 17)[gm_agg$temp])
  points(gm_bar~Cond, data=leafdat3[leafdat3$leaf == "shade",], col=shacol, pch=c(16, 17)[gm_agg$temp])
  points(gm_bar~Cond, data=gm_agg[gm_agg$leaflight == "shade-high",], col=lightscol, pch=c(16, 17)[gm_agg$temp])
  
  legend("topright", alllab, pch=c(16,16,16,16,17), col=allcols,inset = 0.01, bty='n',cex=.8)

  
  #anova(lm(gm_bar~Cond, gm_agg[gm_agg$leaflight == "shade-high",]))
