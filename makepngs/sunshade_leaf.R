
source("functions and packages/packages_md.R")
source("functions and packages/functions.R")
source("functions and packages/plot_objects_all.R")


leaftraits <- read.csv("calculated_data/leaftraits_summary.csv") 

##this has droughtleaves


png(filename = "makepngs/c13_N.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,5,2,2), cex.lab=1.25)

palette(c("yellow4", "forestgreen"))
par(mar=c(5,5,2,2))
plot(c13.mean ~ leafN_area.mean, data=leaftraits, pch=16, col=as.factor(leaf), ylim=c(-35,-25), 
     xlim=c(1,3.5), cex=1.5, xlab=narealab, ylab=c13lab)

legend("topright", leaflab2, pch=16,inset = 0.03, col=leafcol,bty='n', cex=1.25) 

dev.off()


###----aci

aci_leaf <- read.csv("calculated_data/tdl_aci.csv")

#simulate ACi curves for each leaf, ambient and elevated T
sunAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[3,3], Jmax=aci_leaf[3,4],PPFD=1408)
sunET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[4,3], Jmax=aci_leaf[4,4], PPFD=1408)
shaAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[1,3], Jmax=aci_leaf[1,4], PPFD=375)
shaET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[2,3], Jmax=aci_leaf[2,4],PPFD=375)


#plot

png(filename = "makepngs/aci.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(5,5,2,2), cex.lab=1.25)
plot(sunAT_sim$Ci, sunAT_sim$ALEAF, col="forestgreen", pch=21,  cex=1.1,xlab=cilab, ylab="", type="l", lwd=2, ylim=c(0,30))
points(sunET_sim$Ci, sunET_sim$ALEAF, col="forestgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
points(shaAT_sim$Ci, shaAT_sim$ALEAF, col="yellow4", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
points(shaET_sim$Ci, shaET_sim$ALEAF, col="yellow4", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
legend("bottomright", leglab2, lty=c(1,2,1,2), lwd=2, col=colaci, pt.bg=col4,inset = 0.03,bty='n', cex=1.25)
title(ylab=satlab, mgp=ypos)

dev.off()



###lma
lma <- read.csv("calculated_data/leaf_chemistry.csv")
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")
#data format for bar
lmabar <- subset(lma, select = c("lma", "Month", "leaf"))
lmabar$Month <- factor(lmabar$Month, levels = Morder)


png(filename = "makepngs/lma.png", width = 11, height = 8.5, units = "in", res= 400)

bar(lma, c(leaf, Month), lmabar, col=c("yellow4", "forestgreen"), xlab="", ylab=lmalab, ylim=c(0, 185),cex.lab=1.25 ,
    half.errbar=FALSE,mar=c(5,5,2,2))
dev.off()


