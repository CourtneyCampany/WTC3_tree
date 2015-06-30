##physiology bar plots of ci, cc, and drawdown

source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

gasex <- read.csv("calculated_data/gmes_wellwatered.csv")

  ##calculate CC
  gasex$Cc<- with(gasex, Ci-Photo/gm)
  gasex$cc_ci<- with(gasex, Ci/Cc)
  ##calculate drawdown
  gasex$drawdown <- with(gasex, Ci-Cc)

###get average by id
gasex_agg <- summaryBy(Ci+Cc+drawdown~ chamber+id+leaf +light+temp+leaflight+Month, data=gasex, FUN=mean, keep.names=TRUE)

  gasex_agg$leaflight <- gsub("shade-low", "Shade", gasex_agg$leaflight)
  gasex_agg$leaflight <- gsub("sun-high", "Sun", gasex_agg$leaflight)
  gasex_agg$leaflight <- gsub("shade-high", "Sunfleck", gasex_agg$leaflight)

leaforder <- c("Shade", "Sun", "Sunfleck")  
gasex_agg$leaflight <- factor(gasex_agg$leaflight, levels = leaforder)

#lightson <- gasex_agg[gasex_agg$leaflight != "sun-high",]

cols <- c(shacol2, suncol2, lightscol2)

####PLOTTING-------------------------------------------------------------------------------------------------------------

#1: CI

windows(12,6)

par(mfrow=c(1,3))

bar(Ci, leaflight, gasex_agg, col=cols, half.errbar=FALSE, ylim=c(0, 325),names.arg=FALSE,xlab="",
    mar=c(5,5,2,1), ylab=cilab, cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
abline(v=2.5, lty=5)
text(x=.25, y=315, "(a)", cex=1.5)
mtext("Shade", side=1,at=.725, line=2.5 )
mtext("Sun", side=1,at=1.9, line=2.5 )
mtext("Shade \n High Light", side=1,at=3.15, line=3.5 )

#2. CC
bar(Cc, leaflight, gasex_agg, col=cols, xlab="", half.errbar=FALSE, ylim=c(0, 325),names.arg=FALSE,
    mar=c(5,5,2,1), ylab=cclab,  cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
abline(v=2.5, lty=5)
text(x=.25, y=315, "(b)", cex=1.5)
mtext("Shade", side=1,at=.725, line=2.5 )
mtext("Sun", side=1,at=1.9, line=2.5 )
mtext("Shade \n High Light", side=1,at=3.15, line=3.5 )

#3. Drawdown
bar(drawdown, leaflight, gasex_agg, col=cols, xlab="", half.errbar=FALSE, ylim=c(0, 110),names.arg=FALSE,
    mar=c(5,5,2,2), ylab=drawdownlab,  cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
abline(v=2.5, lty=5)
text(x=.25, y=107, "(c)", cex=1.5)
mtext("Shade", side=1,at=.725, line=2.5  )
mtext("Sun", side=1,at=1.9, line=2.5 )
mtext("Shade \n High Light", side=1,at=3.15, line=3.5)

dev.copy2pdf(file="master_scripts/paper_figures/cicc_bar.pdf")
dev.off()
