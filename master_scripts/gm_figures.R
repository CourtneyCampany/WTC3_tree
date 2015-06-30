# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
# source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ CTleaf+gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
  gm_agg$leaf <- gsub("s", "S", gm_agg$leaf)
  gm_agg$light <- gsub("high", "Sun-light", gm_agg$light)
  gm_agg$light <- gsub("low", "Shade-light", gm_agg$light)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
  gm_sunsha <- droplevels(gm_sunsha)

##sun-shade dataframes
sundat <- gm_sunsha[gm_sunsha$leaflight =="sun-high",]
shadat <- gm_sunsha[gm_sunsha$leaflight =="shade-low",]

##sunfleck dataframe
fleckdat <- gm_agg[gm_agg$leaflight == "shade-high",]

##read bootstrapped data previosuly ran from sunshade phys script
agm_sun <-   read.csv( "master_scripts/bootstrap_results/agm_sun.csv")
agm_sha <-  read.csv( "master_scripts/bootstrap_results/agm_sha.csv") 
agm_fleck <-  read.csv( "master_scripts/bootstrap_results/agm_fleck.csv") 

gmt_sun <-   read.csv( "master_scripts/bootstrap_results/gmt_sun")
gmt_sha <-  read.csv( "master_scripts/bootstrap_results/gmt_sha") 


####plotting (3 panel graph of gm data)----------------------------------------------------------------------------------------
# windows(8.5,11)  
par(mfrow=c(2,1))

#panel1: gm v tleaf
par(mar=c(5,5,2,2), cex.axis=1, cex.lab=1.25, las=1)
plot(gm~CTleaf, data=sundat,  col=suncol, ylim=c(0,.35), xlim=c(15, 36), xlab=leaftlab, ylab=gmlab, 
     pch=c(16, 17)[pch=gm_sunsha$temp])
points(gm~CTleaf, data=shadat, col=shacol, cex=1,pch=c(16, 17)[pch=gm_sunsha$temp])
points(gm~CTleaf, data=fleckdat, col=lightscol, cex=1,pch=c(16, 17)[pch=fleckdat$temp])


with(gmt_sun, {
  lines(CTleaf, lcl, lty=2, lwd=2,col="forestgreen")
  lines(CTleaf, ucl, lty=2, lwd=2,col="forestgreen")
  lines(CTleaf, pred, lty=1, lwd=2,col="forestgreen")
})
with(gmt_sha, {
  lines(CTleaf, lcl, lty=2, lwd=2,col="yellow4")
  lines(CTleaf, ucl, lty=2, lwd=2,col="yellow4")
  lines(CTleaf, pred, lty=1, lwd=2,col="yellow4")
})

ablineclip(h=mean(fleckdat$gm), x1=min(fleckdat$CTleaf), x2=max(fleckdat$CTleaf), lty=5, lwd=2, col="darkorange2")

text(x=36, y=.34, "(a)", cex=1.25)

#panel2: Photosynthesis vs gm

par(mar=c(5,5,0,2), cex.axis=1, cex.lab=1.25, cex=1,las=1)
plot(Photo~gm, data=sundat,  col=suncol, ylim=c(5,25), xlim=c(0,.35), xlab=gmlab, ylab=satlab,
     pch=c(16, 17)[pch=gm_sunsha$temp])
points(Photo~gm, data=shadat, col=shacol, pch=c(16, 17)[pch=gm_sunsha$temp])
points(Photo~gm, data=fleckdat, col=lightscol, pch=c(16, 17)[pch=fleckdat$temp])

with(agm_sun, {
  lines(gm, lcl, lty=2, lwd=2,col="forestgreen")
  lines(gm, ucl, lty=2, lwd=2,col="forestgreen")
  lines(gm, pred, lty=1, lwd=2,col="forestgreen")
})
with(agm_sha, {
  lines(gm, lcl, lty=2, lwd=2,col="yellow4")
  lines(gm, ucl, lty=2, lwd=2,col="yellow4")
  lines(gm, pred, lty=1, lwd=2,col="yellow4")
})

with(agm_fleck, {
  lines(gm, lcl, lty=2, lwd=2,col="darkorange2")
  lines(gm, ucl, lty=2, lwd=2,col="darkorange2")
  lines(gm, pred, lty=1, lwd=2,col="darkorange2")
})

text(x=.35, y=24.5, "(b)", cex=1.25)
legend("topleft", leglab3, pch=rep(c(16,17),3), col=leafcols,inset = 0.01, bty='n',cex=1)

#  dev.copy2pdf(file="master_scripts/paper_figures/gm_figures.pdf")
#  dev.off()




