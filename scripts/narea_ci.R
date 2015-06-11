source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
library(doBy)
library(scales)

treatments <- read.csv("raw data/temp_trt.csv")

#read in leaf data
leaf_chem <- read.csv("raw data/leaf_chem.csv")

leaf_mass <- read.csv("raw data/leaf_data.csv")


#calculate nitrogen content in leaves
canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
#add treatments  
canopy_chem <- addtrt_func(canopy_chem)
canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
canopy_chem<- add_campaign(canopy_chem)


###ci from gas exchange and delta c13

Ci_bar <- read.csv("calculated_data/Ci_bar.csv")

gmes <- read.csv("calculated_data/gmes_wtc.csv")

###get average by id
ci_agg <- summaryBy(Ci ~ chamber+id+leaf+light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)

##remove shade-high
ci_sunsha <- ci_agg[ci_agg$leaflight != "shade-high",]
ci_sunsha <- droplevels(ci_sunsha)
ci_sunsha$id <- gsub("-high", "", ci_sunsha$id)
ci_sunsha$id <- gsub("-low", "", ci_sunsha$id)

##merge
ci_c13 <- merge(ci_sunsha, Ci_bar[, c(2,8)], by="id")

ci_narea <- merge(ci_c13, canopy_chem[,c(1:3, 8:9, 12:13)])


##plot
palette(c(shacol, suncol))

greyalpha <- alpha("grey95", alpha=.75)


windows(8.8)
par(mar=c(5,5,2,2), cex=1.5)
plot(Ci ~ leafN_area, data=ci_narea, col=as.factor(leaf), pch=16, ylim=c(0, 400), xlim=c(0,5), ylab=cilab, xlab=narealab)
points(ci_bar ~ leafN_area, data=ci_narea,pch=21, col=as.factor(leaf), bg=greyalpha)
dev.copy2pdf(file="master_scripts/figures/ci_narea.pdf")
dev.off()










