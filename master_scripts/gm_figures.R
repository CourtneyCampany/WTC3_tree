# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
# source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ CTleaf+gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
  gm_sunsha <- droplevels(gm_sunsha)
  gm_sunsha$leaf <- gsub("s", "S", gm_sunsha$leaf)
  gm_sunsha$light <- gsub("high", "Sun-light", gm_sunsha$light)
  gm_sunsha$light <- gsub("low", "Shade-light", gm_sunsha$light)

##sun-shade dataframes
sundat <- gm_sunsha[gm_sunsha$leaflight =="sun-high",]
shadat <- gm_sunsha[gm_sunsha$leaflight =="shade-low",]

##read bootstrapped data previosuly ran from sunshade phys script
agm_sun <-   read.csv( "master_scripts/bootstrap_results/agm_sun.csv")
agm_sha <-  read.csv( "master_scripts/bootstrap_results/agm_sha.csv") 
gmt_sun <-   read.csv( "master_scripts/bootstrap_results/gmt_sun")
gmt_sha <-  read.csv( "master_scripts/bootstrap_results/gmt_sha") 

####plotting (3 panel graph of gm data)----------------------------------------------------------------------------------------
# windows(8.5,11)  

#panel1: gm v tleaf
par(fig=c(0,1,.65,1), new=TRUE)

par(mar=c(2,5,1,5), cex.axis=1, cex.lab=1.25, las=1)
plot(gm~CTleaf, data=sundat,  col=suncol, ylim=c(0,.35), xlim=c(15, 36), xlab="", ylab=gmlab, cex=1, xaxt="n", yaxt="n",
     pch=c(16, 17)[pch=gm_sunsha$temp])
points(gm~CTleaf, data=shadat, col=shacol, cex=1,pch=c(16, 17)[pch=gm_sunsha$temp])

axis(2, mgp=c(3, .5, 0))
axis(1, mgp=c(3, .5, 0))

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
mtext(leaftlab, side=1, line=1.5,cex=1.25)
text(x=36, y=.34, "(a)", cex=1.25)


#panel2: Photosynthesis vs gm
par(fig=c(0,1,.3,.65), new=TRUE)

par(mar=c(2.5,5,1,5), cex.axis=1, cex.lab=1.25, las=1)
plot(Photo~gm, data=sundat,  col=suncol, ylim=c(5,25), xlim=c(0,.35), xlab="", ylab=satlab, cex=1, xaxt="n", yaxt="n",
     pch=c(16, 17)[pch=gm_sunsha$temp])
points(Photo~gm, data=shadat, col=shacol, cex=1,pch=c(16, 17)[pch=gm_sunsha$temp])

axis(2, mgp=c(3, .5, 0))
axis(1, mgp=c(3, .5, 0))

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
mtext(gmlab, side=1, line=2.25,cex=1.25)
text(x=.35, y=24.5, "(b)", cex=1.25)
legend("topleft", leglab2, pch=c(16,17,16,17), col=colaci,inset = 0.01, bty='n',cex=1)

#panel3/4: photo vs gm barplot

#3:gm 
par(fig=c(0,0.5,0,.3), new=TRUE)

bar(gm, leaf, gm_sunsha, col=c(shacol, suncol), xlab="", half.errbar=FALSE, ylim=c(0, 0.2), yaxt="n",
    mar=c(2,5,1,0), ylab=gmlab, cex.axis=1, cex.lab = 1.25, legend=F, cex.names=1.25,mgp=c(3, .5, 0))
text(x=2.35, y=.185, "(c)", cex=1.25)

axis(2, mgp=c(3, .5, 0))


#4:photo

par(fig=c(0.5,1,0,.3), new=TRUE)

bar(Photo, leaf, gm_sunsha, col=c(shacol, suncol), xlab="", half.errbar=FALSE, ylim=c(0, 20), 
    mar=c(2,0,1,5), ylab="", cex.axis=1, cex.lab = 1.25, legend=F, cex.names=1.25, yaxt='n',mgp=c(3, .5, 0))
axis(side=4, labels=TRUE, cex.axis=1, mgp=c(3, .5, 0))

mtext(photolab, side=4, line=3,cex=1.25, las=3)
text(x=2.35, y=18.5, "(d)", cex=1.25)

# 
# dev.copy2pdf(file="master_scripts/paper_figures/gm_figures.pdf")
# dev.off()




