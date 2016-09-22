#physiology boxplots to replace barplots in manuscript (SVC)

source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")


##physiology bar plots of gs, gm and A overall means with sun, shade and lights on
##make this a boxplot or enhanced bar plot (change from physiology bar)


###read and format gmes data------------------------------------------------------------------------------------------
gasex <- read.csv("calculated_data/gmes_wellwatered.csv")

gasex_agg <- summaryBy(Photo+gm+Cond~ chamber+id+leaf +light+temp+leaflight, data=gasex, FUN=mean, keep.names=TRUE)
  gasex_agg$leaflight <- gsub("shade-low", "Shade", gasex_agg$leaflight)
  gasex_agg$leaflight <- gsub("sun-high", "Sun", gasex_agg$leaflight)
  gasex_agg$leaflight <- gsub("shade-high", "Shade \n High Light", gasex_agg$leaflight)


leaforder <- c("Shade", "Shade \n High Light","Sun")  
gasex_agg$leaflight <- factor(gasex_agg$leaflight, levels = leaforder)

#lightson <- gasex_agg[gasex_agg$leaflight != "sun-high",]

cols <- c(shacol, lightscol, suncol)

####PLOTTING boxplots--------------------------------------------------------------------------------

# windows(12,6)
#png(filename = "figpic/phys_barplot.png", width = 11, height = 8.5, units = "in", res= 400)

par(mfrow=c(1,3),cex.axis=1.25, cex.lab = 1.5, mar=c(5,5,2,1))

boxplot(Cond ~leaflight, gasex_agg,  col=cols,ylab=condlab, names=FALSE, outline=FALSE, ylim=c(0, .5))
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=.5, "(a)", cex=1.5)

boxplot(gm ~leaflight, gasex_agg, col=cols,ylab=gmlab2, names=FALSE, outline=FALSE,ylim=c(0, .5))
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=.5, "(b)", cex=1.5)

boxplot(Photo ~leaflight, gasex_agg, col=cols,ylab=photolab, xlab="", names=FALSE, outline=FALSE, ylim=c(0, 25))
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=25, "(c)", cex=1.5)
  



