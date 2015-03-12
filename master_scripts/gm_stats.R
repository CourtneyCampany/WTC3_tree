source("master_scripts/plot_objects.R")
source("functions and packages/functions.R")
library(doBy)

gmes <- read.csv("calculated_data/gmes_WTC.csv")

###for analysis first subset well watered and drought treatments
gm_drought <- gmes[gmes$drydown == "drought",]

gm_water <- gmes[gmes$drydown == "control",]
#write.csv(gm_water, "calculated_data/gmes_wellwatered.csv", row.names=FALSE)

palette(c("blue", "red"))
###drought----------------------------------------------------------------------------------------------------

##plots
plot(Photo~gm, data=gm_drought, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0, .5))
  points(Photo~gm, data=gm_drought, subset=leaflight=="shade-low",pch=1, col=temp)
  legend("topright", templab, pch=16,inset = 0.03, col=palette()) 



###well watered------------------------------------------------------------------------------------------------

##sun-shade
plot(Photo~gm, data=gm_water, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0,.5), 
     xlab=gmlab, ylab="")
points(Photo~gm, data=gm_water, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)

##shade low-high
plot(Photo~gm, data=gm_water, subset=leaflight=="shade-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0,.5), 
     xlab=gmlab, ylab="")
points(Photo~gm, data=gm_water, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)


#leaf types
sun_gm <- gm_water[gm_water$leaf =="sun",]
shade_gm <- gm_water[gm_water$leaflight == "shade-low",]
lightson <- gm_water[gm_water$leaflight == "shade-high",]

#simple plots through time
plot(gm~campaign, data=sun_gm, pch=16, col=temp, ylim=c(0, .6), ylab="sun")
plot(gm~campaign, data=shade_gm, pch=16, col=temp, ylim=c(0, .6), ylab="shade")
plot(gm~campaign, data=lightson, pch=16, col=temp, ylim=c(0, .6), ylab="shade-high")


#means
gm_agg <- summaryBy(gm~ temp+leaf+light, data=gm_water,FUN=mean, keep.names=TRUE)
gm_drought_agg <- summaryBy(gm~ temp+leaf+light, data=gm_drought,FUN=mean, keep.names=TRUE)


##gm-temp
gm_ch<- summaryBy(gm+Photo+Cond+CTleaf ~id+chamber+temp+leaf+light+leaflight+Month, data=gm_water,FUN=mean,
                  keep.names=TRUE)

palette(c("blue", "red"))
   
##photosynthesis and temperature
plot(Photo~CTleaf, data=gm_ch, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,25), xlim=c(15, 40), 
     ylab="", xlab=leaftlab)
  points(Photo~CTleaf, data=gm_ch, subset=leaflight=="shade-low",pch=1, col=temp)
  legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
  title(ylab=satlab, mgp=ypos, cex=1.2)

plot(gm~CTleaf, data=gm_ch, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,.5), xlim=c(15, 40), 
     ylab="", xlab=leaftlab)
points(gm~CTleaf, data=gm_ch, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=gmlab, mgp=ypos, cex=1.2)


plot(Photo~gm, data=gm_ch, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0,.5), 
     ylab="", xlab=gmlab)
points(Photo~gm, data=gm_ch, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)

plot(gm~Cond, data=gm_ch, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,.4), xlim=c(0,.4), 
     ylab="", xlab=gmlab)
points(gm~Cond, data=gm_ch, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=condlab, mgp=ypos, cex=1.2)

###make a table of means and se of gm
tablenames <- c( "Leaf Type", "Light", "Temperature","Gm", "Gm(se)")

gm_dfr <- summaryBy(gm~ leaf+light+temp, data=gm_water,FUN=c(mean,se))
names(gm_dfr) <- tablenames

library(xtable)

gm_table <- xtable(gm_dfr)
digits(gm_table)[4:5] <- 4
print(gm_table,floating=FALSE)




# 
# ###These presentations plots moved from paired script (generated from old gm)
# png(filename = "output/presentations/ciA.png", width = 11, height = 8.5, units = "in", res= 400)
# plot(Photo~Ci, data=gm, pch=21, bg=cl[leaf], xlim=c(0,400), ylim=c(0,30), xlab=cilab, ylab="", cex=1.3)
# title(ylab=satlab, mgp=ypos, cex=1.2)
# legend("topright", sslab, pch=21, pt.bg=cl, pt.cex=2, bg="white",inset = 0.03) 
# dev.off()
# 
# png(filename = "output/presentations/gmA.png", width = 11, height = 8.5, units = "in", res= 400)
# plot(Photo~gm, data=gm, pch=21, bg=cl[leaf], xlim=c(0,1), ylim=c(0,30), xlab=gmlab, ylab="", cex=1.3)
# title(ylab=satlab, mgp=ypos, cex=1.2)
# legend("topright", sslab, pch=21, pt.bg=cl, pt.cex=2, bg="white",inset = 0.03) 
# dev.off()
# 
# png(filename = "output/presentations/ccA.png", width = 11, height = 8.5, units = "in", res= 400)
# plot(Photo~Cc, data=gm, pch=21, bg=cl[leaf], xlim=c(0,400), ylim=c(0,30), xlab=cclab, ylab="", cex=1.3)
# title(ylab=satlab, mgp=ypos, cex=1.2)
# legend("topright", sslab, pch=21, pt.bg=cl, pt.cex=2, bg="white",inset = 0.03) 
# dev.off()
# 
# gm$ratio<- with(gm, gm/Cond)
# plot(Photo~ratio, data=gm, pch=21, bg=cl[leaf], xlim=c(0,25), ylim=c(0,30), xlab=cclab, ylab="", cex=1.3)
# title(ylab=satlab, mgp=ypos, cex=1.2)
# legend("topright", sslab, pch=21, pt.bg=cl, pt.cex=2, bg="white",inset = 0.03) 
# 
# 
# 
