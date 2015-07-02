source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ Cond + gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
gm_agg$leaf <- gsub("s", "S", gm_agg$leaf)
gm_agg$light <- gsub("high", "Sun-light", gm_agg$light)
gm_agg$light <- gsub("low", "Shade-light", gm_agg$light)

palette(c(lightscol, shacol, suncol))
# windows(8,8)
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(gm~Cond, data=gm_agg,  col=leaflight, pch=c(16, 17)[gm_agg$temp], xlim=c(0,.45), ylim=c(0,.45),
     xlab=condlab, ylab=gmlab)
abline(0,1, lty=2)
legend("topleft", alllab, pch=c(16,16,16,16,17), col=allcols,inset = 0.01, bty='n',cex=.8)


# dev.copy2pdf(file="master_scripts/paper_figures/gmgs.pdf")
# dev.off()
