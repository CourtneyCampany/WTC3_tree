##redoes bar as enhanced bar

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

leaforder <- c("Shade", "Sunfleck","Sun")  
gasex_agg$leaflight <- factor(gasex_agg$leaflight, levels = leaforder)

##new color order based on svc
cols <- c(shacol2, lightscol2,suncol2)


##subset dfr for each variable 
ci_dfr<- gasex_agg[,c("leaflight","Ci")]
  ci_mean <- summaryBy(Ci~ leaflight, data=ci_dfr, FUN=mean, keep.names = TRUE)
  ci_se <- summaryBy(Ci~ leaflight, data=ci_dfr, FUN=se, keep.names = TRUE)
cc_dfr<- gasex_agg[,c("leaflight","Cc")]
  cc_mean <- summaryBy(Cc~ leaflight, data=cc_dfr, FUN=mean, keep.names = TRUE)
  cc_se <- summaryBy(Cc~ leaflight, data=cc_dfr, FUN=se, keep.names = TRUE)
draw_dfr<- gasex_agg[,c("leaflight","drawdown")]
  draw_mean <- summaryBy(drawdown ~ leaflight, data=draw_dfr, FUN=mean, keep.names = TRUE)
  draw_se <- summaryBy(drawdown ~ leaflight, data=draw_dfr, FUN=se, keep.names = TRUE)
  
  
  
windows(12,6)  
par(mfrow=c(1,3), cex.axis=1.25,cex.lab = 1.5)
  
#1. Conductance
#first empty plot
plot((1:nrow(ci_mean)),ci_mean$Ci, ylim=c(0, 325), xaxt='n', mar=c(5,5,2,1), ylab=expression(C[i]~~(paste(mu,bar,sep=""))), 
       xlim=c(0.5,nrow(ci_mean)+0.5),type='n', xlab="",mgp=c(2.5,1,0))
  
##add bar plots as rectangles
for(i in 1:nrow(ci_mean)){
    rect(i-0.3,0,i+0.3,ci_mean$Ci[i], col=cols[i])
  }
  
##add points (use raw data)
for(i in 1:nrow(ci_mean)){
    points(rep(i,nrow(ci_dfr[ci_dfr$leaflight==levels(ci_dfr$leaflight)[i],])), 
           ci_dfr$Ci[ci_dfr$leaflight==levels(ci_dfr$leaflight)[i]], bg=alpha(cols[i],alpha=.7), pch=21)
  }
  
##add se to bars
for(i in 1:nrow(ci_mean)){
    arrows(i, ci_mean$Ci[i], i, ci_mean$Ci[i]+ci_se$Ci[i], 
           length = 0.1, angle = 90, lwd=2)
    arrows(i, ci_mean$Ci[i], i, ci_mean$Ci[i]-ci_se$Ci[i], 
           length = 0.1, angle = 90, lwd=2)
  }
  
  mtext("Shade \n Low Light", side=1,at=1, line=3)
  mtext("Shade \n High Light", side=1,at=2, line=3)
  mtext("Sun \n High Light", side=1,at=3, line=3)
  text(x=.55, y=325, "(a)", cex=1.5)
  
#2. cc
#first empty plot
  plot((1:nrow(cc_mean)),cc_mean$Cc, ylim=c(0, 325), xaxt='n',mar=c(5,5,2,1), ylab=expression(C[c]~~(paste(mu,bar,sep=""))),
       xlim=c(0.5,nrow(cc_mean)+0.5),type='n', xlab="",mgp=c(2.5,1,0))
  
##add bar plots as rectangles
for(i in 1:nrow(cc_mean)){
    rect(i-0.3,0,i+0.3,cc_mean$Cc[i], col=cols[i])
  }
  
  ##add points (use raw data)
  for(i in 1:nrow(cc_mean)){
    points(rep(i,nrow(cc_dfr[cc_dfr$leaflight==levels(cc_dfr$leaflight)[i],])), 
           cc_dfr$Cc[cc_dfr$leaflight==levels(cc_dfr$leaflight)[i]], bg=alpha(cols[i],alpha=.7), pch=21)
  }
  
##add se to bars
for(i in 1:nrow(cc_mean)){
    arrows(i, cc_mean$Cc[i], i, cc_mean$Cc[i]+cc_se$Cc[i], 
           length = 0.1, angle = 90, lwd=2)
    arrows(i, cc_mean$Cc[i], i, cc_mean$Cc[i]-cc_se$Cc[i], 
           length = 0.1, angle = 90, lwd=2)
  }
  
  mtext("Shade \n Low Light", side=1,at=1, line=3)
  mtext("Shade \n High Light", side=1,at=2, line=3)
  mtext("Sun \n High Light", side=1,at=3, line=3)
  text(x=.55, y=325, "(b)", cex=1.5)
  
#3.drawdown
#first empty plot
plot((1:nrow(draw_mean)),draw_mean$drawdown, ylim=c(0, 140), xaxt='n',mar=c(5,5,2,2), ylab=drawdownlab3,
       xlim=c(0.5,nrow(draw_mean)+0.5),type='n',xlab="",mgp=c(2.5,1,0))
  
  ##add bar plots as rectangles
for(i in 1:nrow(draw_mean)){
    rect(i-0.3,0,i+0.3,draw_mean$drawdown[i], col=cols[i])
  }
  
  ##add points (use raw data)
for(i in 1:nrow(draw_mean)){
    points(rep(i,nrow(draw_dfr[draw_dfr$leaflight==levels(draw_dfr$leaflight)[i],])), 
           draw_dfr$drawdown[draw_dfr$leaflight==levels(draw_dfr$leaflight)[i]], bg=alpha(cols[i],alpha=.7), pch=21)
  }
  
  ##add se to bars
for(i in 1:nrow(draw_mean)){
    arrows(i, draw_mean$drawdown[i], i, draw_mean$drawdown[i]+draw_se$drawdown[i], 
           length = 0.1, angle = 90, lwd=2)
    arrows(i, draw_mean$drawdown[i], i, draw_mean$drawdown[i]-draw_se$drawdown[i], 
           length = 0.1, angle = 90, lwd=2)
  }
  
  mtext("Shade \n Low Light", side=1,at=1, line=3)
  mtext("Shade \n High Light", side=1,at=2, line=3 )
  mtext("Sun \n High Light", side=1,at=3, line=3 )
  text(x=.55, y=110, "(c)", cex=1.5)
  
dev.copy2pdf(file="master_scripts/paper_figures/cicc_enhancedbar.pdf")
dev.off()
  
  
  
  