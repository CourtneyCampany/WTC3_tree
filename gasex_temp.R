source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gasex<- read.csv("calculated_data/gmes_wellwatered.csv")
  ##calculate CC
  gasex$Cc<- with(gasex, Ci-Photo/gm)
  ##calculate drawdown
  gasex$drawdown <- with(gasex, Ci-Cc)
  gasex$drawdowngs <- with(gasex, CO2R-Ci)

###get average by id
gasex_agg <- summaryBy(Photo+ gm + drawdown +CTleaf+VpdL+Cond+drawdowngs ~ chamber+id+leaf +light+temp+leaflight+Month, data=gasex, 
                       FUN=mean, keep.names=TRUE)


####show data as a function of campaign (lets shade col by campaign)

##lets order by month
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")

gasex_agg$Month <- factor(gasex_agg$Month, levels = Morder)


###shade black colors for each month to test
library(scales)
###sun col
octcol <- alpha(suncol2, alpha=.4)
deccol <- alpha(suncol2, alpha=.55)
jancol <- alpha(suncol2, alpha=.65)
febcol <- alpha(suncol2, alpha=.75)
marcol <- alpha(suncol2, alpha=.9)
aprcol <- suncol2
##shad cols
octcol2 <- alpha(shacol2, alpha=.4)
deccol2<- alpha(shacol2, alpha=.55)
jancol2 <- alpha(shacol2, alpha=.65)
febcol2 <- alpha(shacol2, alpha=.75)
marcol2 <- alpha(shacol2, alpha=.9)
aprcol2 <- shacol2
#fleck col
octcol3 <- alpha(lightscol2, alpha=.4)
deccol3<- alpha(lightscol2, alpha=.55)
jancol3 <- alpha(lightscol2, alpha=.65)
febcol3 <- alpha(lightscol2, alpha=.75)
marcol3 <- alpha(lightscol2, alpha=.9)
aprcol3 <- lightscol2

fleckalpha <- c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3)
sunalpha <- c(octcol, deccol, jancol, febcol, marcol, aprcol)
shaalpha <- c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2)

##graph photo, gm and drawdown by temperature
windows(8,12)

par(mfrow=c(3,1))
par(mar=c(0,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))

palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
plot(Photo~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(5, 25), xlim=c(15,40),
     xlab="", ylab=satlab, xaxt="n")
  
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(Photo~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)
  
  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(Photo~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
#   legend("topright", Morder, pch=16, col=sunalpha,inset = 0.01, bty='n',cex=.8)
#   legend("topright", Morder, pch=16, col=sunalpha,inset = 0.01, bty='n',cex=.8)
#   legend("bottomright", Morder, pch=16, col=fleckalpha,inset = 0.01, bty='n',cex=.8)
  
##panel 2: gm 
  par(mar=c(0,4,0,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
  palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
  plot(gm~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(0, .4), xlim=c(15,40),
       xlab="", ylab=gmlab,xaxt="n")
  
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(gm~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)
  
  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(gm~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
  
##panel 3: drawdown
  par(mar=c(4,4,0,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
  palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
  plot(drawdown~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(25, 175), xlim=c(15,40),
       xlab=leaftlab, ylab=drawdownlab)
  
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(drawdown~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)
  
  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(drawdown~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
  
   dev.copy2pdf(file="master_scripts/paper_figures/gasex_temp1.pdf")
   dev.off()
  
####second set of figures
  windows(8,12)
  
  par(mfrow=c(3,1))
  par(mar=c(0,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
  ##vpdl
  palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
  plot(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(0, 5), xlim=c(15,40),
       xlab="", ylab=vpdlab, xaxt="n")
  
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)
  
  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
  
  ##panel 2: gs
  par(mar=c(0,4,0,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
  palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
  plot(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(0, .4), xlim=c(15,40),
       xlab="", ylab=gmlab,xaxt="n")
  
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)
  
  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
  
  ##panel 3: drawdowngs
  par(mar=c(4,4,0,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
  palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
  plot(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(50, 350), xlim=c(15,40),
       xlab=leaftlab, ylab=drawdownlab)
  
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)
  
  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
  
   dev.copy2pdf(file="master_scripts/paper_figures/gasex_temp2.pdf")
   dev.off()
