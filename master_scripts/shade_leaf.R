source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
library(doBy)
library(xtable)

#wtc treatments
treatments <- read.csv("raw data/temp_trt.csv")


###aci graph and table---------------------------------------------------------------------------------
aci_leaf <- read.csv("calculated_data/tdl_aci.csv")

aci_table <- xtable(aci_leaf)
digits(aci_table)[c(3,5:6)] <- 3
digits(aci_table)[c(4)] <- 4
print(aci_table,floating=FALSE)

#simulated curves
  library(plantecophys)

  #simulate ACi curves for each leaf, ambient and elevated T
  sunAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[3,3], Jmax=aci_leaf[3,4])
  sunET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[4,3], Jmax=aci_leaf[4,4])
  shaAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[1,3], Jmax=aci_leaf[1,4])
  shaET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=aci_leaf[2,3], Jmax=aci_leaf[2,4])

#plot
plot(sunAT_sim$Ci, sunAT_sim$ALEAF, col="forestgreen", pch=21,  cex=1.1,xlab=cilab, ylab="", type="l", lwd=2)
  points(sunET_sim$Ci, sunET_sim$ALEAF, col="forestgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  points(shaAT_sim$Ci, shaAT_sim$ALEAF, col="yellowgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
  points(shaET_sim$Ci, shaET_sim$ALEAF, col="yellowgreen", pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  legend("topleft", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)
  title(ylab=satlab, mgp=ypos)


#####leaf c13v N---------------------------------------------------------------------------------------
leaftraits <- read.csv("calculated_data/leaftraits_summary.csv")
##this has droughtleaves

palette(c("yellowgreen", "green4"))

plot(c13.mean ~ leafN_area.mean, data=leaftraits, pch=16, col=as.factor(leaf), ylim=c(-35,-25), 
                 xlim=c(1,3.5), cex=1.25, xlab=narealab, ylab=c13lab)
legend("bottomright", leaflab, pch=16, col=c("yellowgreen", "green4"),pt.cex=1.5,inset = 0.03)  


####PPFD-----------------------------------------------------------------------------------------------
par <- read.csv("raw data/par.csv")
treatments <- read.csv("raw data/chamber_trt.csv")

#format function
par<- parformat(par)

par_leaf <- subset(par, ID !="shade-high")
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")

#data format for bar
parbar <- subset(par_leaf, select = c("par", "month", "leaf_type"))
parbar$month <- factor(parbar$month, levels = Morder)


bar(par, c(leaf_type, month), parbar, col=c("yellowgreen", "green4"), xlab="", ylab="", ylim=c(0, 2000), 
    half.errbar=FALSE)
title(ylab=parlab, mgp=ypos)


###SLA---------------------------------------------------------------------------------------------------

#run formatting functions
leaf <- addtrt_func(leaf)
leaf$sla <- with(leaf, leaf_area/leaf_mass)

###mean sla by chamber and campaing
leaf_agg <- summaryBy(leaf_mass+leaf_area+sla ~ Month+chamber+leaf+temp, data=leaf,FUN=mean, keep.names=TRUE)
leaf_agg <- add_campaign(leaf_agg)

leaf_agg2 <- summaryBy(leaf_mass+leaf_area+sla ~ leaf+temp, data=leaf,FUN=c(mean,se))

