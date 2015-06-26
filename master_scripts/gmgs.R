source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Cond+gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)

windows(8,6)
par(mar=c(5,5,2,2), cex.axis=1, cex.lab=1.25, las=1, cex=1.25)
plot(gm~Cond, data=gm_agg, ylim=c(0,.35), xlim=c(0 ,.35), xlab=condlab, ylab=gmlab, pch="")
abline(0,1, lty=2)
points(gm~Cond, data=gm_agg[gm_agg$leaflight=="sun-high",], col=suncol, cex=1,pch=c(16, 17)[pch=gm_agg$temp])
points(gm~Cond, data=gm_agg[gm_agg$leaflight=="shade-low",], col=shacol, cex=1,pch=c(16, 17)[pch=gm_agg$temp])
points(gm~Cond, data=gm_agg[gm_agg$leaflight=="shade-high",], col=lightscol, cex=1,pch=c(16, 17)[pch=gm_agg$temp])

legend("topleft", leglab3, pch=rep(c(16,17),3), col=leafcols,inset = 0.01, bty='n',cex=.8)