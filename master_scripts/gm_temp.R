source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

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
gmt_sun <-   read.csv( "master_scripts/bootstrap_results/gmt_sun")
gmt_sha <-  read.csv( "master_scripts/bootstrap_results/gmt_sha") 

##confidence interval of mean of sunfleck has there is no temperature effect
n <- length(fleckdat$gm)
s <- sd(fleckdat$gm) #sample standard dev
serr <- s/(sqrt(n)) #standard error estimate
t <- qt(.975, df=n-1)
xbar <- mean(fleckdat$gm)
E <- t*serr #margin of error
#CI = mean +- E
ucl_fleck <- xbar+E
lcl_fleck <- xbar-E

####plotting (3 panel graph of gm data)----------------------------------------------------------------------------------------
windows(8,6)  

par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(gm~CTleaf, data=sundat,  col=suncol, ylim=c(0,.35), xlim=c(15, 36), xlab=leaftlab, ylab=gmlab, 
     pch=c(16, 17)[pch=gm_sunsha$temp])
points(gm~CTleaf, data=shadat, col=shacol, cex=1,pch=c(16, 17)[pch=gm_sunsha$temp])
points(gm~CTleaf, data=fleckdat, col=lightscol, cex=1,pch=c(16, 17)[pch=fleckdat$temp])


with(gmt_sun, {
  lines(CTleaf, lcl, lty=2, lwd=2,col=suncol2)
  lines(CTleaf, ucl, lty=2, lwd=2,col=suncol2)
  lines(CTleaf, pred, lty=1, lwd=2,col=suncol2)
})
with(gmt_sha, {
  lines(CTleaf, lcl, lty=2, lwd=2,col=shacol2)
  lines(CTleaf, ucl, lty=2, lwd=2,col=shacol2)
  lines(CTleaf, pred, lty=1, lwd=2,col=shacol2)
})

#CI for sunfleck as mean line and CI
ablineclip(h=mean(fleckdat$gm), x1=min(fleckdat$CTleaf), x2=max(fleckdat$CTleaf), lty=1, lwd=2, col=lightscol2)
ablineclip(h=ucl_fleck, x1=min(fleckdat$CTleaf), x2=max(fleckdat$CTleaf), lty=5, lwd=2, col=lightscol2)
ablineclip(h=lcl_fleck, x1=min(fleckdat$CTleaf), x2=max(fleckdat$CTleaf), lty=5, lwd=2, col=lightscol2)


  dev.copy2pdf(file="master_scripts/paper_figures/gm_temp.pdf")
  dev.off()




