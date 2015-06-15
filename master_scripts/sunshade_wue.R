source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------

Ci_bar <- read.csv("calculated_data/Ci_bar.csv")

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
##calculate CC
gmes$Cc<- with(gmes, Ci-Photo/gm)
gmes$cc_ci<- with(gmes, Ci/Cc)

###get average by id
gm_agg <- summaryBy(Photo+Cond+Ci+Trmmol+Cc+gm+VpdL+xsi+DELTA+cc_ci ~ chamber+id+leaf +light+temp+leaflight+Month, 
                    data=gmes, FUN=mean, keep.names=TRUE)

#add iWUE
gm_agg$iWUE <- with(gm_agg, Photo/Trmmol)

##write Iwue to 
write.csv(gm_agg[, c(1,3,5:7,18)], "calculated_data/ITE.csv", row.names = FALSE)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
gm_sunsha <- droplevels(gm_sunsha)
gm_sunsha$id <- gsub("-high", "", gm_sunsha$id)
gm_sunsha$id <- gsub("-low", "", gm_sunsha$id)

##merge
gm_c13 <- merge(gm_sunsha, Ci_bar[, c(2,8)], by="id")
gm_c13$Cc_bar <- with(gm_c13, ci_bar-Photo/gm)
#add total conductance to CO2
gm_c13$gmgs <- with(gm_c13, gm+Cond)



####simple plots first to look at difference between water use efficiency btw sun shade---------------------  

##palette for temp treatment
palette(c("blue", "red"))

###VPD between leaf types
shadevpd <- gm_c13[gm_c13$leaflight=="shade-low", c("VpdL", "iWUE","id", "chamber", "Month", "temp")]
names(shadevpd)[1:2] <- c("vpd_shade", "ITE_shade")
sunvpd <- gm_c13[gm_c13$leaflight=="sun-high",  c("VpdL", "iWUE", "id", "chamber", "Month", "temp")]
names(sunvpd)[1:2] <- c("vpd_sun", "ITE_sun")
vpd_tree <- merge(shadevpd, sunvpd, by=c("chamber", "Month", "temp"))

plot( vpd_sun~vpd_shade, data=vpd_tree, ylab="vpd_sun", xlab="vpd_shade", ylim=c(0, 4), xlim=c(0,4),
     col=as.factor(temp), pch=16, cex=1.5)
abline(0,1)

plot(ITE_sun~ITE_shade, data=vpd_tree, ylim=c(0, 15), xlim=c(0,15),ylab="ITE_sun", xlab="ITE_shade",
     col=as.factor(temp), pch=16, cex=1.5)
abline(0,1)

##palette for sun shade
palette(c(shacol, suncol))

plot(Cond~Trmmol, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25, xlim=c(0,6), ylim=c(0,.4))
points(Cond~Trmmol, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)

plot(Cond~VpdL, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25, xlim=c(0,5), ylim=c(0,.4))
points(Cond~VpdL, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25) 

bar(iWUE, c(leaf, temp), gm_c13, col=c("yellowgreen", "green4"),half.errbar=FALSE, mar=c(5,7,2,2), 
    legend = FALSE, ylim=c(0,10))
legend("topright", c("Shade", "Sun"), pch=22,inset = 0.01, pt.bg=c("yellowgreen", "green4"),
       bty='n', cex=1.25)  

bar(iWUE,leaf, gm_c13, col=c("yellowgreen", "green4"),half.errbar=FALSE, mar=c(5,7,2,2), 
    legend = FALSE, ylim=c(0,10), xlab="")
legend("topright", c("Shade", "Sun"), pch=22,inset = 0.01, pt.bg=c("yellowgreen", "green4"),
       bty='n', cex=1.25) 

plot(iWUE~VpdL, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25, xlim=c(0,4), ylim=c(0,20))
points(iWUE~VpdL, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25) 
legend("topright", c("Shade", "Sun"), pch=22,inset = 0.01, pt.bg=c("yellowgreen", "green4"),
       bty='n', cex=1.25) 