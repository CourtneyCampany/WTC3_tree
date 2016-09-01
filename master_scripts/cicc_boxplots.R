#cicc boxplots to replace barplots in manuscript (SVC)

source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")


gasex <- read.csv("calculated_data/gmes_wellwatered2.csv")

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

leaforder <- c("Shade", "Sunfleck", "Sun")  
gasex_agg$leaflight <- factor(gasex_agg$leaflight, levels = leaforder)


cols <- c(shacol2, lightscol2,suncol2)


# windows(12,6)
par(mfrow=c(1,3),cex.axis=1.25, cex.lab = 1.5, mar=c(5,5,2,1))

boxplot(Ci ~leaflight, gasex_agg, col=cols, ylab=expression(C[i]~~(paste(mu,bar,sep=""))), names=FALSE, ylim=c(0,325), outline=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=325, "(a)", cex=1.5)

boxplot(Cc ~leaflight, gasex_agg, col=cols, ylab=expression(C[c]~~(paste(mu,bar,sep=""))), names=FALSE,ylim=c(0,325),outline=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=325, "(b)", cex=1.5)

boxplot(drawdown~leaflight, gasex_agg, col=cols, ylab=drawdownlab3, xlab="", names=FALSE, ylim=c(0,325),outline=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=325, "(c)", cex=1.5)
  
