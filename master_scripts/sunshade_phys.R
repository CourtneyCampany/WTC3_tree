
#read in gm data set (no drought)

gmes <- read.csv("calculated_data/gmes_wtc.csv")


##calculate CC
gmes$Cc<- with(gmes, Ci-Photo/gm)


###plots of gm, cc, photo
palette(c("darkgoldenrod2", "forestgreen", "red"))

plot(Photo~gm, data=gmes, col=leaflight, pch=16)
plot(Photo~Ci, data=gmes, col=leaflight, pch=16)
plot(Photo~Cc, data=gmes, col=leaflight, pch=16)
