source("master_scripts/plot_objects.R")

library(doBy)
#read in gm data set (no drought)

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
Ci_bar <- read.csv("calculated_data/Ci_bar.csv")


##calculate CC
gmes$Cc<- with(gmes, Ci-Photo/gm)
gmes$cc_ci<- with(gmes, Ci/Cc)

##need to combine Ci_bar with CC and gm

###get average by id
gm_agg <- summaryBy(Photo+Cond+Ci+Cc+gm+VpdL+xsi+DELTA+cc_ci ~ id+leaf +light+temp+leaflight, 
                    data=gmes, FUN=mean, keep.names=TRUE)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
  gm_sunsha <- droplevels(gm_sunsha)
  gm_sunsha$id <- gsub("-high", "", gm_sunsha$id)
  gm_sunsha$id <- gsub("-low", "", gm_sunsha$id)

##merge
gm_c13 <- merge(gm_sunsha, Ci_bar[, c(2,8)], by="id")
  gm_c13$Cc_bar <- with(gm_c13, ci_bar-Photo/gm)


###plots of gm, cc, photo

##cc from ci gas exchange
plot(Photo~Cc, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0,300), xlab=cclab)
points(Photo~Cc, data=gm_c13, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)

#cc from ci_bar (discrimination)
plot(Photo~Cc_bar, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=temp, ylim=c(0,25), xlim=c(0,300), xlab=cclab2)
points(Photo~Cc_bar, data=gm_c13, subset=leaflight=="shade-low",pch=1, col=temp)
legend("topright", templab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)



palette(c("darkgoldenrod2", "forestgreen", "red"))

plot(Photo~gm, data=gm_agg, col=leaflight, pch=16, ylim=c(0,25), xlim=c(0,.4), xlab=gmlab, ylab="")
legend("topright", leaflightlab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)

plot(Photo~Ci, data=gm_agg, col=leaflight, pch=16,ylim=c(0,25), xlim=c(0,400),xlab=cilab, ylab="")
legend("topright", leaflightlab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)

plot(Photo~Cc, data=gm_agg, col=leaflight, pch=16,ylim=c(0,25),xlim=c(0,400),xlab=cclab, ylab="")
legend("topright", leaflightlab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)

plot(Photo~cc_ci, data=gm_agg, col=leaflight, pch=16,ylim=c(0,25),xlim=c(0,6), ylab="")
legend("topright", leaflightlab, pch=16,inset = 0.03, col=palette()) 
title(ylab=satlab, mgp=ypos, cex=1.2)

