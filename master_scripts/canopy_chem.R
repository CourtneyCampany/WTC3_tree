source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
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

###write file
#write.csv(canopy_chem, "calculated_data/leaf_chemistry.csv", row.names=FALSE)

canopy_agg2 <- summaryBy(c13+ lma+ leafN + leafN_area ~ leaf+campaign+temp, data=canopy_chem, FUN=c(mean,se))
#write.csv(canopy_agg2, "calculated_data/leaftraits_summary.csv",row.names=FALSE)
                         
                         
#   #order my month over 2013-14
#   Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")
#   canopy_chem$Month <- factor(canopy_chem$Month, levels = Morder)


###treatment data subsets
# canopy_ambT<- subset(canopy_chem, temp == "ambient")
# canopy_eleT<- subset(canopy_chem, temp == "elevated")
# canopy_drought <- subset(canopy_chem, Month %in% c("Mar", "Apr"))


####remove drought for now
canopy_nodrought <- subset(canopy_chem, drydown != "drought")
canopy_agg <- summaryBy(c13+ lma+ leafN + leafN_area ~ leaf+campaign+temp, data=canopy_nodrought, FUN=c(mean,se))


##plots leaf traits across campaigns----------------------------------------------------------------------
suncols <- c("blue", "red")
shadecols <- alpha(suncols, .5)
palette( c("blue", "red"))
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")



###simple figure on Narea
canopy_nodrought$leaf <- gsub("s", "S", canopy_nodrought$leaf)
narea_agg <- summaryBy(leafN_area ~ leaf, data=canopy_nodrought, FUN=c(mean,se))

png(filename = "makepngs/narea.png", width = 11, height = 8.5, units = "in", res= 400)

bar(leafN_area, leaf, canopy_nodrought, col=c(shacol, suncol), xlab="", half.errbar=FALSE, ylim=c(0,3),
    mar=c(5,7,2,2), ylab=narealab, cex.axis=1.75, cex.lab = 2, cex.names=2,legend = FALSE)
dev.off()


#c13 plots (means and across campaigns)----------------------------------------------------------------------

#means
mean(canopy_chem[canopy_chem$leaf=="shade", "c13"])
mean(canopy_chem[canopy_chem$leaf=="sun", "c13"])

bar(c13, leaf, canopy_chem, col=c(shacol, suncol), xlab="", half.errbar=FALSE, ylim=c(-35, -20),
    mar=c(5,7,2,2), ylab=c13lab,legend = FALSE)


windows(8,8)
par(mar=c(5,5,2,2))
plot(c13.mean ~ campaign, data=canopy_agg,col=temp, pch=16, xlab="", ylab= c13lab, ylim=c(-33,-25), 
     cex=1.5, cex.lab=1.5,axes=FALSE, type='n')
    #sun-elevated
    with(canopy_agg[canopy_agg$leaf=="sun" & canopy_agg$temp=="elevated", ], 
         arrows(campaign, c13.mean, campaign, c13.mean+c13.se, angle=90, col="grey75",length=0.03, cex=2.25))
    with(canopy_agg[canopy_agg$leaf=="sun" & canopy_agg$temp=="elevated", ], 
         arrows(campaign, c13.mean, campaign, c13.mean-c13.se, angle=90, col="grey75",length=0.03, cex=2.25))
    
    points(c13.mean ~ campaign, data=canopy_agg[canopy_agg$leaf=="sun" & canopy_agg$temp=="elevated", ],
           bg="grey75", col="red",pch=21, cex=2.25, lwd=1.9)
    
    #sun-ambient
    with(canopy_agg[canopy_agg$leaf=="sun" & canopy_agg$temp=="ambient", ], 
         arrows(campaign, c13.mean, campaign, c13.mean+c13.se, angle=90, col="grey75",length=0.03, cex=2.25))
    with(canopy_agg[canopy_agg$leaf=="sun" & canopy_agg$temp=="ambient", ], 
         arrows(campaign, c13.mean, campaign, c13.mean-c13.se, angle=90, col="grey75",length=0.03, cex=2.25))
    
    points(c13.mean ~ campaign, data=canopy_agg[canopy_agg$leaf=="sun" & canopy_agg$temp=="ambient", ],
           bg="grey75", col="blue",pch=21, cex=2.25, lwd=1.9)
    
    #shade-elevated
    with(canopy_agg[canopy_agg$leaf=="shade" & canopy_agg$temp=="elevated", ], 
         arrows(campaign, c13.mean, campaign, c13.mean+c13.se, angle=90, col="grey25",length=0.03, cex=2.25))
    with(canopy_agg[canopy_agg$leaf=="shade" & canopy_agg$temp=="elevated", ], 
         arrows(campaign, c13.mean, campaign, c13.mean-c13.se, angle=90, col="grey25",length=0.03, cex=2.25))
    
    points(c13.mean ~ campaign, data=canopy_agg[canopy_agg$leaf=="shade" & canopy_agg$temp=="elevated", ],
           bg="grey25", col="red",pch=21, cex=2.25, lwd=1.9)
    
    #shade-ambient
    with(canopy_agg[canopy_agg$leaf=="shade" & canopy_agg$temp=="ambient", ], 
         arrows(campaign, c13.mean, campaign, c13.mean+c13.se, angle=90, col="grey25",length=0.03, cex=2.25))
    with(canopy_agg[canopy_agg$leaf=="shade" & canopy_agg$temp=="ambient", ], 
         arrows(campaign, c13.mean, campaign, c13.mean-c13.se, angle=90, col="grey25",length=0.03, cex=2.25))
    
    points(c13.mean ~ campaign, data=canopy_agg[canopy_agg$leaf=="shade" & canopy_agg$temp=="ambient", ],
           bg="grey25", col="blue",pch=21, cex=2.25, lwd=1.9)


axis(1, labels = Morder, at= c(1,2,3,4,5,6))
axis(2, labels=TRUE)
box()
dev.copy2pdf(file="master_scripts/figures/c13.pdf")
dev.off()

canopy_agg[canopy_agg$leaf=="sun" & canopy_agg$temp=="elevated", "c13.mean"]
#ablineclip(massgm_lm, lty=2, x1=xmin, x2=xmax)

palette(c("yellowgreen", "green4"))

###c13 vs Narea
windows(7,5)
plot(c13.mean ~ leafN_area.mean, data=canopy_agg2, pch=16, col=as.factor(leaf), ylim=c(-35,-25), xlim=c(0,3.5), cex=1.25,
     xlab=narealab, ylab=c13lab)
legend("bottomright", leaflab, pch=16, col=c("yellowgreen", "green4"),pt.cex=1.5,inset = 0.03)  
dev.copy2pdf(file="master_scripts/figures/c13_nitro.pdf")
dev.off()

windows(8,8)
plot(c13 ~ leafN_area, data=canopy_chem, pch=16, col=as.factor(leaf), ylim=c(-35,-25), xlim=c(0,4), 
      cex=1.25, xlab=narealab, ylab=c13lab)


plot(c13 ~ leafN_area, data=canopy_chem, pch=16, col=c(shacol, suncol), ylim=c(-35,-25), xlim=c(0,4), 
     cex=1.25, xlab=narealab, ylab=c13lab)





