# 
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


####second set of figures----------------------------------------------------------------------------------------------------
#windows(8,12)

par(mfrow=c(3,1))
par(mar=c(0,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
##vpdl
palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
plot(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(0, 3.5), xlim=c(15,37.5),
     xlab="", ylab=vpdlab, xaxt="n")

palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
points(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)

palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
points(VpdL~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
text(y=3.5, x=37.5, "(a)", cex=.9)
legend("topleft", leaflightlab2, pch=16, col=allcols,inset = 0.01, bty='n',cex=.8)
legend("bottomright", Morder, pch=16, col=legalpha,inset = 0.01, bty='n',cex=.8)

##panel 2: gs
par(mar=c(0,4,0,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
plot(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(0, .4), xlim=c(15,37.5),
     xlab="", ylab=condlab,xaxt="n")

palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
points(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)

palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
points(Cond~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
text(y=.4, x=37.5, "(b)", cex=.9)

##panel 3: drawdowngs
par(mar=c(4,4,0,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
palette(c(octcol, deccol, jancol, febcol, marcol, aprcol))
plot(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="sun-high",], col=Month, pch=16, ylim=c(50, 350), xlim=c(15,37.5),
     xlab=leaftlab, ylab=drawdownlab2)

palette(c(octcol2, deccol2, jancol2, febcol2, marcol2, aprcol2))
points(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-low",], col=Month, pch=16)

palette(c(octcol3, deccol3, jancol3, febcol3, marcol3, aprcol3))
points(drawdowngs~CTleaf, data=gasex_agg[gasex_agg$leaflight=="shade-high",], col=Month, pch=16)
text(y=350, x=37.5, "(c)", cex=.9)

# dev.copy2pdf(file="master_scripts/paper_figures/gasex_temp2.pdf")
# dev.off()
