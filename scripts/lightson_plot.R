source("functions and packages/packages_md.R")
source("functions and packages/functions.R")
source("functions and packages/plot_objects_all.R")

gmes <- read.csv("calculated_data/gmes_WTC.csv")
gmpair<- read.csv("calculated_data/gmes_wtc_pair.csv")
gmwet <- read.csv("calculated_data/gmes_wellwatered.csv")
Ci_bar <- read.csv("calculated_data/Ci_bar.csv")
###for analysis first subset well watered and drought treatments
gm_drought <- gmes[gmes$drydown == "drought",]
gm_water <- gmes[gmes$drydown == "control",]


#plot low vs high light shade leaves
plot(Photo~gm, data=gm_water, subset=leaflight=="shade-high", pch=16, col=lightscol, ylim=c(0,30), xlim=c(0,.4), 
     xlab=gmlab, ylab="", cex=1.25)
points(Photo~gm, data=gm_water, subset=leaflight=="shade-low",pch=16, col=shacol, cex=1.25)
legend("topright", lightleg, pch=16,inset = 0.02, col=lightlab) 
legend("topleft", "Shade Leaves", bty='n') 
title(ylab=satlab, mgp=ypos, cex=1.2)


#####remake plots with lines between paired points

##split by light with only data needed

leaf_ss <- gm_water[, c("chamber", "id", "Photo", "gm", "leaf", "light", "temp", "drydown", "leaflight", "Month")]
leaf_ss2 <- leaf_ss[leaf_ss$leaf == "shade",]

leaf_sp <- dlply(leaf_ss2, .(leaflight))

high <- data.frame(leaf_sp[1])
high_agg <- summaryBy(shade.high.Photo+shade.high.gm ~ shade.high.chamber+shade.high.Month, data = high, 
                      FUN=mean, keep.names = TRUE)
names(high_agg) <- c("chamber", "Month", "Photo.high", "gm.high")

low<- data.frame(leaf_sp[2])
low_agg <- summaryBy(shade.low.Photo+shade.low.gm ~ shade.low.chamber+shade.low.Month, data = low, 
                     FUN=mean, keep.names = TRUE)
names(low_agg) <- c("chamber", "Month", "Photo.low", "gm.low")

##merge by pair

light_pair <- merge(high_agg, low_agg, by=c("chamber","Month"), all=TRUE)
light_pair2 <- light_pair[complete.cases(light_pair),]
light_pair2 <- arrange(light_pair2, gm.low)


plot(Photo.high~gm.high, data=light_pair2,  pch=16, col=lightscol, ylim=c(0,30), xlim=c(0,.4), 
     xlab=gmlab, ylab="", cex=1.25)
  points(Photo.low~gm.low, data=light_pair2,pch=16, col=shacol, cex=1.25)
  lines(x=c(light_pair2$gm.low[1],light_pair2$gm.high[1]) , y=c(light_pair2$Photo.low[1],light_pair2$Photo.high[1]),
        type='l', col="black", lty=1, lwd=1)



