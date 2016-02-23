source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ Cond + gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
gm_agg$leaf <- gsub("s", "S", gm_agg$leaf)
gm_agg$light <- gsub("high", "Sun-light", gm_agg$light)
gm_agg$light <- gsub("low", "Shade-light", gm_agg$light)

###Im going to plot gmes of shade at different light levels, so seperate into two dfrs for ease (make sure they match correctly)

shadat <- gm_agg[gm_agg$leaflight == "shade-low",]
shadat <- droplevels(shadat)
shadat2 <- shadat[,c(1,5,7,10)]
names(shadat2)[4] <- "shade_gm"


##dfr with lights on
fleckdat <- gm_agg[gm_agg$leaflight == "shade-high",]
fleckdat <- droplevels(fleckdat)
fleckdat2 <- fleckdat[,c(1,5,7,10)]
names(fleckdat2)[4] <- "fleck_gm"

#merge
shadeleaf <- merge(shadat2, fleckdat2, by=c("chamber", "temp","Month"))

##plot
shagmlab <- expression(g[m]~Low~Light~~(mol~m^-2~s^-1))
fleckgmlab <- expression(g[m]~High~Light~~(mol~m^-2~s^-1))

windows(7,7)
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(fleck_gm~shade_gm, data=shadeleaf, ylim=c(0,.3), xlim=c(0,.3), xlab=shagmlab, ylab=fleckgmlab, 
     pch=c(16, 17)[shadeleaf$temp])
abline(0,1, lty=2)
legend("topleft", c("AT", "ET"), pch=c(16,17), inset = 0.01, bty='n', title="Shade Leaves", cex=.8)

dev.copy2pdf(file="master_scripts/paper_figures/gmshade.pdf")
dev.off()