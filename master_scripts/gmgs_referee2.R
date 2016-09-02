source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

##read format phys data------------------------------------------------------------------------------------------------
gasex <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <- summaryBy(gm_bar+Cond+Photo ~ chamber+id+leaf +light+temp+leaflight+Month, 
                    data=gasex, FUN=mean, keep.names=TRUE)

###gmes vs gs plot------------------------------------------------------------------------------------------------------
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(gm_bar~Cond, data=gm_agg, ylab = gmlab2, xlab=condlab, xlim=c(0, .5), type='n')

predline(lm(gm_bar~Cond, data=gm_agg[gm_agg$leaflight=="sun-high",]), col=suncol2,lwd=2)
predline(lm(gm_bar~Cond, data=gm_agg[gm_agg$leaflight=="shade-low",]), col=shacol2,lwd=2)
predline(lm(gm_bar~Cond, gm_agg[gm_agg$leaflight == "shade-high",]), col=lightscol2,lwd=2)

points(gm_bar~Cond, data=gm_agg[gm_agg$leaflight == "sun-high",], col=suncol, pch=c(16, 17)[gm_agg$temp])
points(gm_bar~Cond, data=gm_agg[gm_agg$leaflight == "shade-low",], col=shacol, pch=c(16, 17)[gm_agg$temp])
points(gm_bar~Cond, data=gm_agg[gm_agg$leaflight == "shade-high",], col=lightscol, pch=c(16, 17)[gm_agg$temp])

legend("topright", alllab, pch=c(16,16,16,16,17), col=allcols,inset = 0.01, bty='n',cex=.8)

# dev.copy2pdf(file="gmcond.pdf")
# dev.off()  

#anova(lm(gm_bar~Cond, gm_agg[gm_agg$leaflight == "shade-high",]))