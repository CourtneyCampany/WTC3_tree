# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
# source("functions and packages/packages.R")

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

##sun col
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

pchmonth <- c(15, 16, 17, 18, 1,2)

oct4 <- alpha("black", alpha=.4)
dec4 <- alpha("black", alpha=.55)
jan4 <- alpha("black", alpha=.65)
feb4 <- alpha("black", alpha=.75)
mar4 <- alpha("black", alpha=.9)
apr4 <- "black"

legalpha <- c(oct4, dec4, jan4, feb4, mar4, apr4)

###Stats for regression lines-------------------------------------------------------------------------------------------------
##use these as intercepts for relatioships that are not significant
meanVPD <-summaryBy(VpdL ~ leaflight, data=gasex_agg, FUN=mean)
meangs <-summaryBy(Cond ~ leaflight, data=gasex_agg, FUN=mean)
meandrawgs <-summaryBy(drawdowngs ~ leaflight, data=gasex_agg, FUN=mean)

###gm has slight negative relationship, use slope and intercept forabline---------------------------------------------------
###ive already tested that models with or without random effects are similar (use simple model for plotting)

##conductance
gst_sun_mod <- lm(Cond~ CTleaf , data=gasex_agg, subset=leaflight=="sun-high")
gst_sha_mod <- lm(Cond~ CTleaf , data=gasex_agg,subset=leaflight=="shade-low")
gst_fleck_mod <- lm(Cond~ CTleaf , data=gasex_agg,subset=leaflight=="shade-high")
  # summary(gst_sun_mod)
  # summary(gst_sha_mod)
  # summary(gst_fleck_mod)

##cica drawdown
draw_sun_mod <- lm(drawdowngs~ CTleaf , data=gasex_agg, subset=leaflight=="sun-high")
draw_sha_mod <- lm(drawdowngs~ CTleaf , data=gasex_agg,subset=leaflight=="shade-low")
draw_fleck_mod <- lm(drawdowngs~ CTleaf , data=gasex_agg,subset=leaflight=="shade-high")
  # summary(draw_sun_mod)
  # summary(draw_sha_mod)
  # summary(draw_fleck_mod)

  ##vpd
VpdL_sun_mod <- lm(VpdL~ CTleaf , data=gasex_agg, subset=leaflight=="sun-high")
VpdL_sha_mod <- lm(VpdL~ CTleaf , data=gasex_agg,subset=leaflight=="shade-low")
VpdL_fleck_mod <- lm(VpdL~ CTleaf , data=gasex_agg,subset=leaflight=="shade-high")
  # summary(VpdL_sun_mod)
  # summary(VpdL_sha_mod)
  # summary(VpdL_fleck_mod)
  coefsunvpd <- coef(VpdL_sun_mod)
  coefshavpd <- coef(VpdL_sha_mod)
  coeffleckvpd <- coef(VpdL_fleck_mod)
  
####second set of figures----------------------------------------------------------------------------------------------------
# windows(7,10)

par(mfrow=c(3,1),cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0),oma=c(4, 0, 1,0))

par(mar=c(0,4,0,1))
##vpdl
palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
plot(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(0, 3.5), xlim=c(15,37.5),
     xlab="", ylab=vpdlab, xaxt="n", type='n')
 
 ablineclip(a=coefsunvpd[1],b=coefsunvpd[2],x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=suncol2)
 ablineclip(a=coefshavpd[1],b=coefshavpd[2],x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=shacol2)
 ablineclip(a=coeffleckvpd[1],b=coeffleckvpd[2] ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=lightscol2)

points(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16)
           
palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
points(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)

palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
points(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
text(y=3.4, x=37.5, "(a)", cex=.9)
legend("topleft", leaflightlab2, pch=16, col=allcols, bty='n',cex=.8)


##panel 2: gs
par(mar=c(0,4,0,1))
palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
plot(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(0, .5), xlim=c(15,37.5),
     xlab="", ylab=condlab,xaxt="n",type='n')

ablineclip(a=meangs[meangs$leaflight=="sun-high",2], b=0,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=suncol2)
ablineclip(a=meangs[meangs$leaflight=="shade-low",2],b=0,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=shacol2)
ablineclip(a=meangs[meangs$leaflight=="shade-high",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=lightscol2)

points(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16)

palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
points(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)

palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
points(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
text(y=.48, x=37.5, "(b)", cex=.9)

legend("topleft", Morder, pch=16, col=legalpha, bty='n',cex=.8)

##panel 3: drawdowngs
par(mar=c(0,4,0,1))
palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
plot(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(50, 350), xlim=c(15,37.5),
     xlab="", ylab=drawdownlab2, type='n')

ablineclip(a=meandrawgs[meandrawgs$leaflight=="sun-high",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=suncol2)
ablineclip(a=meandrawgs[meandrawgs$leaflight=="shade-low",2],b=0  ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=shacol2)
ablineclip(a=meandrawgs[meandrawgs$leaflight=="shade-high",2],b=0 ,x1=min(gasex_agg$CTleaf), x2=max(gasex_agg$CTleaf), lwd=2, col=lightscol2)

points(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16)


palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
points(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)

palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
points(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
text(y=340, x=37.5, "(c)", cex=.9)
mtext(leaftlab, side=1, outer=TRUE, line=2.5, cex=1.25)

