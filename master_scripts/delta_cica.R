source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(DELTA + CiCa ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)


##pltot Delta vs cica
palette(c(lightscol, shacol, suncol))
deltalab <-(expression(Delta[obs]))  
cicalab <- "Ci/Ca"


cica_seq <- seq(min(0,1, length=101))

delta_pred <- (4.4+(27-4.4)*cica_seq)
  
windows(8,6)
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(DELTA ~ CiCa, data=gm_agg, col=leaflight,  xlim=c(0,1), ylim=c(0,20),
     xaxs='i', yaxs='i',
     pch=c(16, 17)[pch=gm_agg$temp], ylab=deltalab, xlab=cicalab)
     legend("topleft", alllab, pch=c(16,16,16,16,17), col=allcols,inset = 0.01, bty='n',cex=.8)
points(delta_pred~cica_seq, type='l',lwd=2)


dev.copy2pdf(file="master_scripts/paper_figures/delta_cica.pdf")
dev.off()