
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

boxplot(Cond ~leaflight, gasex_agg, col=cols, ylab=condlab, names=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=.485, "(a)", cex=1.5)

boxplot(gm ~leaflight, gasex_agg, col=cols, ylab=gmlab, names=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=.345, "(b)", cex=1.5)

boxplot(Photo ~leaflight, gasex_agg, col=cols, ylab=photolab, xlab="", names=FALSE)
  mtext("Shade \n Low Light", side=1,at=1, line=3.5)
  mtext("Shade \n High Light", side=1,at=2, line=3.5)
  mtext("Sun \n High Light", side=1,at=3, line=3.5)
  text(x=.55, y=23.2, "(c)", cex=1.5)


####PLOTTING enhance bars--------------------------------------------------------------------------------
enhancedbarsAlpha <- function(dataframe,resp,treat,meandf,sedf,colpal=palette(), ylabel=NULL, maintitle=NULL,xlabel=NULL, 
                              yadj=-1.7,ylimit=range(resp[!is.na(resp) & is.finite(resp)]), alpha=0.7,yaxis="s",points=TRUE,...){
  
  plot((1:length(meandf)),meandf, ylim=ylimit, 
       cex.axis=1.2, cex=1.2, xlim=c(0.5,length(meandf)+0.5),type='n', ylab="",xaxt='n', yaxt=yaxis, xlab="",main=maintitle)
  
  for(i in 1:length(meandf)){
    rect(i-0.3,0,i+0.3,meandf[i], col=alpha(colpal[i],alpha=alpha), ...)
  }
  if(points == TRUE){
    for(i in 1:length(meandf)){
      points(jitter(rep(i,nrow(dataframe[treat==levels(treat)[i],]))), 
             jitter(resp[treat==levels(treat)[i]], factor=0.1), bg=alpha(colpal[i],alpha=alpha), pch=21)
    }
  }else{}
  
  for(i in 1:length(meandf)){
    arrows(i, meandf[i], i, meandf[i]+sedf[i], 
           length = 0.1, angle = 90)
    arrows(i, meandf[i], i, meandf[i]-sedf[i], 
           length = 0.1, angle = 90)
  }
  axis(side=1, labels=xlabel, at=c(1:length(meandf)), cex=1.2)
  mtext(ylabel, side=2, cex=1.2, padj=-1.7)
  
}

#1: stomatal conductance

# windows(12,6)

# par(mfrow=c(1,3))
# 
# bar(Cond, leaflight, gasex_agg, col=cols, half.errbar=FALSE, ylim=c(0, 0.3),names.arg=FALSE,xlab="",
#     mar=c(5,5,2,1), ylab=condlab, cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
# abline(v=2.5, lty=5)
# text(x=.25, y=.2925, "(a)", cex=1.5)
# mtext("Shade \n Low Light", side=1,at=.725, line=3.5 )
# mtext("Sun \n High Light", side=1,at=1.9, line=3.5 )
# mtext("Shade \n High Light", side=1,at=3.15, line=3.5 )
# 
# ####mesophyll conductance
# bar(gm, leaflight, gasex_agg, col=cols, xlab="", half.errbar=FALSE, ylim=c(0, 0.225),names.arg=FALSE,
#     mar=c(5,5,2,1), ylab=gmlab,  cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
# abline(v=2.5, lty=5)
# text(x=.25, y=.22, "(b)", cex=1.5)
# mtext("Shade \n Low Light", side=1,at=.725, line=3.5 )
# mtext("Sun \n High Light", side=1,at=1.9, line=3.5 )
# mtext("Shade \n High Light", side=1,at=3.15, line=3.5 )
# 
# ####photosynthesis
# bar(Photo, leaflight, gasex_agg, col=cols, xlab="", half.errbar=FALSE, ylim=c(0, 20),names.arg=FALSE,
#     mar=c(5,5,2,2), ylab=photolab,  cex.axis=1.25, cex.lab = 1.5, cex.names=1.5,legend=F)
# abline(v=2.5, lty=5)
# text(x=.25, y=19.5, "(c)", cex=1.5)
# mtext("Shade \n Low Light", side=1,at=.725, line=3.5  )
# mtext("Sun \n High Light", side=1,at=1.9, line=3.5 )
# mtext("Shade \n High Light", side=1,at=3.15, line=3.5)

# dev.copy2pdf(file="master_scripts/paper_figures/physiology_bar.pdf")
# dev.off()