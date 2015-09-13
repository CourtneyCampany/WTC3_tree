gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###make data for OULA

##only sun
gm_sun <- gmes[gmes$leaf == "sun",]


##just variables I need

gm_sun2 <- gm_sun[, c(1, 19, 28,80, 85:87)]


library(doBy)

gm_agg <- summaryBy(gm +Tleaf+ PARi~ Month+chamber+temp+drydown, data=gm_sun2, FUN=mean, keep.names=TRUE)


Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")

gm_agg2 <- gm_agg[gm_agg$Month == "Oct" | gm_agg$Month == "Jan" | gm_agg$Month == "Apr",]


##drought_apr--------------------------------------------------------------------------------------------------------
gmes2 <- read.csv("calculated_data/gmes_drought.csv")

gm_sun_dry <- gmes2[gmes2$leaf == "sun",]
gm_sun_dry <- gm_sun_dry[, c(1, 19, 28,80, 85:87)]
gm_agg_dry <- summaryBy(gm +Tleaf+ PARi~ Month+chamber+temp+drydown, data=gm_sun_dry, FUN=mean, keep.names=TRUE)
gm_agg_dry2 <- gm_agg_dry[gm_agg_dry$Month =="Apr",]

gm_oula <- rbind(gm_agg2, gm_agg_dry2)

write.csv(gm_oula, "output/dataforoula.csv", row.names=FALSE)
