##physiology enhanced barplots


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


cols <- c(shacol2, lightscol2,suncol2)

##redo order for SVC
leaforder <- c("Shade", "Shade \n High Light","Sun")  
gasex_agg$leaflight <- factor(gasex_agg$leaflight, levels = leaforder)


##subset dfr for each variable 
cond_dfr<- gasex_agg[,c("leaflight","Cond")]
  cond_mean <- summaryBy(Cond~ leaflight, data=cond_dfr, FUN=mean, keep.names = TRUE)
  cond_se <- summaryBy(Cond~ leaflight, data=cond_dfr, FUN=se, keep.names = TRUE)
gm_dfr<- gasex_agg[,c("leaflight","gm")]
  gm_mean <- summaryBy(gm~ leaflight, data=gm_dfr, FUN=mean, keep.names = TRUE)
  gm_se <- summaryBy(gm~ leaflight, data=gm_dfr, FUN=se, keep.names = TRUE)
photo_dfr<- gasex_agg[,c("leaflight","Photo")]
  photo_mean <- summaryBy(Photo ~ leaflight, data=photo_dfr, FUN=mean, keep.names = TRUE)
  photo_se <- summaryBy(Photo ~ leaflight, data=photo_dfr, FUN=se, keep.names = TRUE)


## 3 panel physiolgoy enhanced bar plot  ---------------------------------------------------------------------------------------
windows(12,6)  
par(mfrow=c(1,3), cex.axis=1.25,cex.lab = 1.5)

#1. Conductance
#first empty plot
plot((1:nrow(cond_mean)),cond_mean$Cond, ylim=c(0, 0.4), xaxt='n', mar=c(5,5,2,1), ylab=condlab, 
     xlim=c(0.5,nrow(cond_mean)+0.5),type='n', xlab="",mgp=c(2.5,1,0))

##add bar plots as rectangles
for(i in 1:nrow(cond_mean)){
  rect(i-0.3,0,i+0.3,cond_mean$Cond[i], col=cols[i])
}

##add points (use raw data)
for(i in 1:nrow(cond_mean)){
  points(rep(i,nrow(cond_dfr[cond_dfr$leaflight==levels(cond_dfr$leaflight)[i],])), 
         cond_dfr$Cond[cond_dfr$leaflight==levels(cond_dfr$leaflight)[i]], bg=alpha(cols[i],alpha=.7), pch=21)
}

##add se to bars
for(i in 1:nrow(cond_mean)){
  arrows(i, cond_mean$Cond[i], i, cond_mean$Cond[i]+cond_se$Cond[i], 
         length = 0.1, angle = 90, lwd=2)
  arrows(i, cond_mean$Cond[i], i, cond_mean$Cond[i]-cond_se$Cond[i], 
         length = 0.1, angle = 90, lwd=2)
}

mtext("Shade \n Low Light", side=1,at=1, line=3)
mtext("Shade \n High Light", side=1,at=2, line=3)
mtext("Sun \n High Light", side=1,at=3, line=3)
text(x=.55, y=.4, "(a)", cex=1.5)

#2. gm
#first empty plot
plot((1:nrow(gm_mean)),gm_mean$gm, ylim=c(0, 0.3), xaxt='n',mar=c(5,5,2,1), ylab=gmlab,
     xlim=c(0.5,nrow(gm_mean)+0.5),type='n', xlab="",mgp=c(2.5,1,0))

##add bar plots as rectangles
for(i in 1:nrow(gm_mean)){
  rect(i-0.3,0,i+0.3,gm_mean$gm[i], col=cols[i])
}

##add points (use raw data)
for(i in 1:nrow(gm_mean)){
  points(rep(i,nrow(gm_dfr[gm_dfr$leaflight==levels(gm_dfr$leaflight)[i],])), 
         gm_dfr$gm[gm_dfr$leaflight==levels(gm_dfr$leaflight)[i]], bg=alpha(cols[i],alpha=.7), pch=21)
}

##add se to bars
for(i in 1:nrow(gm_mean)){
  arrows(i, gm_mean$gm[i], i, gm_mean$gm[i]+gm_se$gm[i], 
         length = 0.1, angle = 90, lwd=2)
  arrows(i, gm_mean$gm[i], i, gm_mean$gm[i]-gm_se$gm[i], 
         length = 0.1, angle = 90, lwd=2)
}

mtext("Shade \n Low Light", side=1,at=1, line=3)
mtext("Shade \n High Light", side=1,at=2, line=3)
mtext("Sun \n High Light", side=1,at=3, line=3)
text(x=.55, y=.3, "(b)", cex=1.5)

#3.Photo
#first empty plot
plot((1:nrow(photo_mean)),photo_mean$Photo, ylim=c(0, 30), xaxt='n',mar=c(5,5,2,2), ylab=photolab,
     xlim=c(0.5,nrow(photo_mean)+0.5),type='n',xlab="",mgp=c(2.5,1,0))

##add bar plots as rectangles
for(i in 1:nrow(photo_mean)){
  rect(i-0.3,0,i+0.3,photo_mean$Photo[i], col=cols[i])
}

##add points (use raw data)
for(i in 1:nrow(photo_mean)){
  points(rep(i,nrow(photo_dfr[photo_dfr$leaflight==levels(photo_dfr$leaflight)[i],])), 
         photo_dfr$Photo[photo_dfr$leaflight==levels(photo_dfr$leaflight)[i]], bg=alpha(cols[i],alpha=.7), pch=21)
}

##add se to bars
for(i in 1:nrow(photo_mean)){
  arrows(i, photo_mean$Photo[i], i, photo_mean$Photo[i]+photo_se$Photo[i], 
         length = 0.1, angle = 90, lwd=2)
  arrows(i, photo_mean$Photo[i], i, photo_mean$Photo[i]-photo_se$Photo[i], 
         length = 0.1, angle = 90, lwd=2)
}

mtext("Shade \n Low Light", side=1,at=1, line=3)
mtext("Shade \n High Light", side=1,at=2, line=3 )
mtext("Sun \n High Light", side=1,at=3, line=3 )
text(x=.55, y=30, "(c)", cex=1.5)

dev.copy2pdf(file="master_scripts/paper_figures/physiology_enhancedbar.pdf")
dev.off()

 