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


#sunshade diff
gm_sunsha <- gm_water[gm_water$leaflight != "shade-high",]

gm_sunsha_id <- summaryBy(gm +Photo + Cond ~ id + leaf + temp,  data=gm_sunsha,FUN=mean, keep.names=TRUE)
  gm_sunsha_id$leaf <- gsub("s", "S", gm_sunsha_id$leaf)

gm_agg <- summaryBy(gm + Photo + Cond ~ leaf, data=gm_sunsha_id,FUN=c(mean,se))


###lights on 

gm_lightson <-gm_water[gm_water$leaflight != "sun-high",]

gm_lightson_id <- summaryBy(gm +Photo ~ id + leaflight + light+temp,  data=gm_lightson,FUN=mean, keep.names=TRUE)
gm_lightson_id$leaf <- gsub("s", "S", gm_lightson_id$leaf)
gm_lightson_id$light <- gsub("high", "Sun-light", gm_lightson_id$light)
gm_lightson_id$light <- gsub("low", "Shade-light", gm_lightson_id$light)

gm_light_agg <- summaryBy(gm + Photo ~ light, data=gm_lightson_id,FUN=c(mean,se))


##figures-----------------------------------------------------------------------------------

#sunshade gmphoto

#windows(8,5)
png(filename = "makepngs/gmphoto_bar.png", width = 11, height = 8.5, units = "in", res= 400)
par(mfrow=c(1,2))

bar(gm, leaf, gm_sunsha_id, col=c(shacol, suncol), xlab="", half.errbar=FALSE, ylim=c(0, 0.2),
    mar=c(5,5,2,0), ylab=gmlab, cex.axis=1.25, cex.lab = 1.5, legend=F, cex.names=1.5)

bar(Photo, leaf, gm_sunsha_id, col=c(shacol, suncol), xlab="", half.errbar=FALSE, ylim=c(0, 20),
    mar=c(5,0,2,5), ylab="", cex.axis=1.25, cex.lab = 1.5, legend=F, cex.names=1.5, yaxt='n')
axis(side=4, labels=TRUE, cex.axis=1.25)
mtext(photolab, side=4, mgp=c(3, 1, 0), line=3,cex=1.5)

dev.off()

##lightson gmphoto

#windows(8,5)
png(filename = "makepngs/lightson_bar.png", width = 11, height = 8.5, units = "in", res= 400)
par(mfrow=c(1,2))

bar(gm, light, gm_lightson_id, col=c(shacol,lightscol), xlab="", half.errbar=FALSE, ylim=c(0, 0.2),
    mar=c(5,5,2,0), ylab=gmlab, cex.axis=1.25, cex.lab = 1.5, legend=F, cex.names=1.5)
text(.7, .19, "Shade Leaves", cex=1.5)

bar(Photo, light, gm_lightson_id, col=c(shacol,lightscol), xlab="", half.errbar=FALSE, ylim=c(0, 20),
    mar=c(5,0,2,5), ylab="", cex.axis=1.25, cex.lab = 1.5, legend=F, cex.names=1.5, yaxt='n')
axis(side=4, labels=TRUE, cex.axis=1.25)
mtext(photolab, side=4, mgp=c(3, 1, 0), line=3,cex=1.5)

dev.off()


#sunshade gsphoto

#windows(8,5)
png(filename = "makepngs/gsphoto_bar.png", width = 11, height = 8.5, units = "in", res= 400)
par(mfrow=c(1,2))

bar(Cond, leaf, gm_sunsha_id, col=c(shacol, suncol), xlab="", half.errbar=FALSE, ylim=c(0, 0.20),
    mar=c(5,5,2,0), ylab=condlab, cex.axis=1.25, cex.lab = 1.5, legend=F, cex.names=1.5)

bar(Photo, leaf, gm_sunsha_id, col=c(shacol, suncol), xlab="", half.errbar=FALSE, ylim=c(0, 20),
    mar=c(5,0,2,5), ylab="", cex.axis=1.25, cex.lab = 1.5, legend=F, cex.names=1.5, yaxt='n')
axis(side=4, labels=TRUE, cex.axis=1.25)
mtext(photolab, side=4, mgp=c(3, 1, 0), line=3,cex=1.5)

dev.off()

  