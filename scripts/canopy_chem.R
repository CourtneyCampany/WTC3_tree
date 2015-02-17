source("functions and packages/functions.R")
library(doBy)
library(scales)

treatments <- read.csv("raw data/temp_trt.csv")

#read in leaf data
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

#   #order my month over 2013-14
#   Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")
#   canopy_chem$Month <- factor(canopy_chem$Month, levels = Morder)


###treatment data subsets
# canopy_ambT<- subset(canopy_chem, temp == "ambient")
# canopy_eleT<- subset(canopy_chem, temp == "elevated")
# canopy_drought <- subset(canopy_chem, Month %in% c("Mar", "Apr"))

canopy_nodrought <- subset(canopy_chem, drydown != "drought")

canopy_agg <- summaryBy(c13+ lma+ leafN + leafN_area ~ leaf+campaign+temp, data=canopy_nodrought, FUN=c(mean,se))

##plots leaf traits across campaigns
suncols <- c("blue", "red")
shadecols <- alpha(suncols, .5)
palette( c("blue", "red"))

Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")


#c13 over time
plot(c13.mean ~ campaign, data=canopy_agg,col=tempcols, pch=16, xlab="", ylab= "delta 13C", ylim=c(-35,-25), 
     cex=1.5, axes=FALSE, type='n')
  with(canopy_agg, arrows(campaign, c13.mean, campaign, c13.mean+c13.se, angle=90, col=suncols,
                          length=0.03, cex=1.5))
  with(canopy_agg, arrows(campaign, c13.mean, campaign, c13.mean-c13.se, angle=90, col=shadecols,
                          length=0.03, cex=1.5 ))  

  points(c13.mean ~ campaign, data=canopy_agg,bg=shadecols, col="grey30",pch=21,subset=leaf == "shade", cex=1.5)
  points(c13.mean ~ campaign, data=canopy_agg,col=suncols, pch=16,subset=leaf == "sun", cex=1.5)

axis(1, labels = Morder, at= c(1,2,3,4,5,6))
axis(2, labels=TRUE)
box()

#ablineclip(massgm_lm, lty=2, x1=xmin, x2=xmax)


###c13 vs Narea
plot(c13.mean ~ leafN_area.mean, data=canopy_agg, pch=16, col=as.factor(leaf), ylim=c(-35,-25), xlim=c(0,3.5), cex=1.25,
     xlab=narealab, ylab=c13lab)


plot(c13 ~ leafN_area, data=canopy_chem, pch=16, col=as.factor(leaf), ylim=c(-35,-25), xlim=c(0,4), 
      cex=1.25, xlab=narealab, ylab=c13lab)



