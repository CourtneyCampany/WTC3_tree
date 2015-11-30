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


###shade black colors for each month to test-----------------------------------------------------------------------------
library(scales)
##sun col
octcol <- alpha(suncol2, alpha=.25)
deccol <- alpha(suncol2, alpha=.4)
jancol <- alpha(suncol2, alpha=.55)
febcol <- alpha(suncol2, alpha=.7)
marcol <- alpha(suncol2, alpha=.85)
aprcol <- suncol2
##shad cols
octcol2 <- alpha(shacol2, alpha=.25)
deccol2<- alpha(shacol2, alpha=.4)
jancol2 <- alpha(shacol2, alpha=.55)
febcol2 <- alpha(shacol2, alpha=.7)
marcol2 <- alpha(shacol2, alpha=.85)
aprcol2 <- shacol2
#fleck col
octcol3 <- alpha(lightscol2, alpha=.25)
deccol3<- alpha(lightscol2, alpha=.4)
jancol3 <- alpha(lightscol2, alpha=.55)
febcol3 <- alpha(lightscol2, alpha=.7)
marcol3 <- alpha(lightscol2, alpha=.85)
aprcol3 <- lightscol2

fleckalpha <- c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3)
sunalpha <- c(octcol, deccol, jancol, febcol, marcol, aprcol)
shaalpha <- c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2)

pchmonth <- c(15, 16, 17, 18, 1,2)

oct4 <- alpha("black", alpha=.25)
dec4 <- alpha("black", alpha=.4)
jan4 <- alpha("black", alpha=.55)
feb4 <- alpha("black", alpha=.7)
mar4 <- alpha("black", alpha=.85)
apr4 <- "black"

legalpha <- c(oct4, dec4, jan4, feb4, mar4, apr4)

###mean values of each parameter by leaflight-------------------------------------------------------------------------------
##use these as intercepts for relatioships that are not significant
meanA <-summaryBy(Photo ~ leaflight, data=gasex_agg, FUN=mean)
meangm <-summaryBy(gm ~ leaflight, data=gasex_agg, FUN=mean)
meandraw <-summaryBy(drawdown ~ leaflight, data=gasex_agg, FUN=mean)

###gm has slight negative relationship, use slope and intercept forabline---------------------------------------------------
###ive already tested that models with or without random effects are similar (use simple model for plotting)
gmt_sun_mod <- lm(gm~ CTleaf , data=gasex_agg, subset=leaflight=="sun-high")
gmt_sha_mod <- lm(gm~ CTleaf , data=gasex_agg,subset=leaflight=="shade-low")
gmt_fleck_mod <- lm(gm~ CTleaf , data=gasex_agg,subset=leaflight=="shade-high")

summary(gmt_sun_mod)
summary(gmt_sha_mod)
summary(gmt_fleck_mod)

coefsun <- coef(gmt_sun_mod)
coefsha <- coef(gmt_sha_mod)
coeffleck <- coef(gmt_fleck_mod)


##graph photo, gm and drawdown by temperature---------------------------------------------------------------------------------
windows(7,10)

par(mfrow=c(3,1),oma=c(4, 0, 1,0),mgp=c(2.5,1,0),cex=1.25, las=1, cex.axis=.8, cex.lab=1)

par(mar=c(0,4,0,1))
palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))

plot(Photo~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16,
     ylim=c(5, 27), xlim=c(14,37.5),xlab="", ylab=satlab, xaxt="n", type='n')

  ablineclip(a=meanA[meanA$leaflight=="sun-high",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=suncol2)
  ablineclip(a=meanA[meanA$leaflight=="shade-low",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=shacol2)
  ablineclip(a=meanA[meanA$leaflight=="shade-high",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=lightscol2)
  
  points(Photo~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16)  
  
  ##change palette inbetween leaftypes
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(Photo~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)

  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(Photo~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
  text(x=37.5, y=26.5, "(a)", cex=.9)

  legend("topleft", leaflightlab2, pch=16, col=allcols, bty='n',cex=.8)
  box()
  
##panel 2: gm 
  par(mar=c(0,4,0,1))
  palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
  plot(gm~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(0, .4),xlim=c(14,37.5),
       xlab="", ylab=gmlab,xaxt="n", type='n')
  
  ablineclip(a=coefsun[1],b=coefsun[2] ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=suncol2)
  ablineclip(a=coefsha[1],b=coefsha[2] ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=shacol2)
  ablineclip(a=coeffleck[1],b=coeffleck[2] ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=lightscol2)
  
  points(gm~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16)
  
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(gm~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)
  
  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(gm~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
  text(x=37.5, y=.39, "(b)", cex=.9)
  
  legend("topleft", Morder, pch=16, col=legalpha, bty='n',cex=.8)
  
##panel 3: drawdown
  par(mar=c(0,4,0,1))
  palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
  plot(drawdown~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(25, 175), xlim=c(14,37.5),
       xlab="", ylab=drawdownlab, type='n')
  
  ablineclip(a=meandraw[meandraw$leaflight=="sun-high",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=suncol2)
  ablineclip(a=meandraw[meandraw$leaflight=="shade-low",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=shacol2)
  ablineclip(a=meandraw[meandraw$leaflight=="shade-high",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=lightscol2)
  
  points(drawdown~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16)
  
  palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
  points(drawdown~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)
  
  palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
  points(drawdown~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
  text(x=37.5, y=170, "(c)", cex=.9)
  mtext(leaftlab, side=1, outer=TRUE, line=2.5, cex=1.25)
  
# dev.copy2pdf(file="master_scripts/paper_figures/gasex_temp1.pdf")
# dev.off()
  
