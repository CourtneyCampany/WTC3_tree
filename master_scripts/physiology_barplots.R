##physiology bar plots of gs, gm and A overall means with sun, shade and lights on

# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
# source("functions and packages/packages.R")

gasex <- read.csv("calculated_data/gmes_wellwatered.csv")


gasex_agg <- summaryBy(Photo+gm+Cond~ chamber+id+leaf +light+temp+leaflight, data=gasex, FUN=mean, keep.names=TRUE)
  gasex_agg$leaflight <- gsub("shade-low", "Shade", gasex_agg$leaflight)
  gasex_agg$leaflight <- gsub("sun-high", "Sun", gasex_agg$leaflight)
  gasex_agg$leaflight <- gsub("shade-high", "Sunfleck", gasex_agg$leaflight)


leaforder <- c("Shade", "Sun", "Sunfleck")  
gasex_agg$leaflight <- factor(gasex_agg$leaflight, levels = leaforder)

#lightson <- gasex_agg[gasex_agg$leaflight != "sun-high",]

cols <- c(shacol, suncol, lightscol)

####PLOTTING-------------------------------------------------------------------------------------------------------------

#1: stomatal conductance

# windows(12,6)

par(mfrow=c(1,3))

bar(Cond, leaflight, gasex_agg, col=cols, xlab="", half.errbar=FALSE, ylim=c(0, 0.3),
    mar=c(5,5,2,1), ylab=condlab, cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
abline(v=2.5, lty=5)
text(x=.25, y=.2925, "(a)", cex=1.5)

####mesophyll conductance

#par(fig=c(0,0.5,0,.3), new=TRUE)

bar(gm, leaflight, gasex_agg, col=cols, xlab="", half.errbar=FALSE, ylim=c(0, 0.225),
    mar=c(5,5,2,1), ylab=gmlab,  cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
abline(v=2.5, lty=5)
text(x=.25, y=.22, "(b)", cex=1.5)

####photosynthesis
#par(fig=c(0.5,1,0,.3), new=TRUE)

bar(Photo, leaflight, gasex_agg, col=cols, xlab="", half.errbar=FALSE, ylim=c(0, 20),
    mar=c(5,5,2,2), ylab=satlab,  cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
abline(v=2.5, lty=5)
text(x=.25, y=19.5, "(c)", cex=1.5)

# dev.copy2pdf(file="master_scripts/paper_figures/physiology_bar.pdf")
# dev.off()
