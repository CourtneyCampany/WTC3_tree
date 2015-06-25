source("functions and packages/functions.R")
source("functions and packages/packages.R")
#source("scripts/read_data.R")
source("master_scripts/plot_objects_paired.R")

#read in gmsunsha
#gm data, no drought, no shade high
gm<- read.csv("calculated_data/gmes_wtc_pair.csv")
gm$variable <- as.factor(paste(gm$leaf, gm$temp, sep="-"))

gm2 <- gm[gm$leaflight != "shade-high",]

#paired id dfr
idDF <- data.frame(pair_id = unique(gm$pair_id))

gm_id <- summaryBy(gm~ variable+pair_id, data=gm2, FUN=c(mean), keep.names=TRUE)
#this next section merges with all possible pair ids so missing trees are accounted for
gm_sp <- dlply(gm_id, .(variable), function(x) merge(x, idDF, all = TRUE))

###plotting--------------------------------------------------------------------------------------------------

###all pairs
plot(gm_sp[[4]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab=suatgm, xlim=c(0,.5), ylim=c(0,.5)) 
abline(0,1, lty=2)
title(ylab=gmlab, mgp=ypos, cex=1.2)
legend("topright", pairlab, pch=21, pt.bg=paircol, bg="white",pt.cex=2,inset = 0.03)  
points(gm_sp[[1]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5)   
points(gm_sp[[2]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5) 


###panel fig

windows(7,10)
par(cex.axis=1.21, cex.lab=1.51,las=1,mgp=c(3.5,1,0),mfrow=c(3,1),  
    omi=c(.5,0,0.1,0.1))   

# First Panel
par(mar=c(0,7,2,2))
plot(gm_sp[[4]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[3], cex=2.5, ylab="", xlab="", xlim=c(0,.5), ylim=c(0,.5),
     axes=FALSE) 
abline(0,1, lty=2)
axis(1, labels=FALSE) 
axis(2, labels=TRUE) 
box()

legend(.4,.45, pairlab[1], pch=21, pt.bg=paircol[1], pt.cex=2,bg="white",inset = 0.06)  

# Second panel   
par(mar=c(0,7,0,2))
plot(gm_sp[[1]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[1], cex=2.5, ylab=gmlab, xlim=c(0,.5), ylim=c(0,.5),xlab="",
     axes=FALSE)     
abline(0,1, lty=2)
legend(.4,.45, pairlab[2], pch=21, pt.bg=paircol[2], pt.cex=2,bg="white",inset = 0.06)
axis(1, labels=FALSE) 
axis(2, labels=TRUE) 
box()

#third panel
par(mar=c(5,7,0,2))
plot(gm_sp[[2]][,3]~gm_sp[[3]][,3], pch=21, bg=wa[5], cex=2.5, ylab="", xlim=c(0,.5), ylim=c(0,.5),xlab=suatgm)     
abline(0,1, lty=2)
legend(.4,.45, pairlab[3], pch=21, pt.bg=paircol[3], pt.cex=2,bg="white",inset = 0.06) 


