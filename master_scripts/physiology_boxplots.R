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

cols <- c(shacol2, lightscol2,suncol2)

####PLOTTING boxplots--------------------------------------------------------------------------------

windows(12,6)
par(mfrow=c(1,3),cex.axis=1.25, cex.lab = 1.5, mar=c(5,5,2,1))

boxplot(Cond ~leaflight, gasex_agg, col=cols, ylab=condlab, names=FALSE, outline=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=.485, "(a)", cex=1.5)

boxplot(gm ~leaflight, gasex_agg, col=cols, ylab=gmlab, names=FALSE, outline=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=.345, "(b)", cex=1.5)

boxplot(Photo ~leaflight, gasex_agg, col=cols, ylab=photolab, xlab="", names=FALSE, outline=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=23.2, "(c)", cex=1.5)
  
dev.copy2pdf(file="master_scripts/paper_figures/physiology_boxplots.pdf")
dev.off()


