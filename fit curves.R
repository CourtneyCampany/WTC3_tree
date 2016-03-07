library(plantecophys)

sun <- read.csv("sunleafaci.csv")
sun <- droplevels(subset(sun, chamber != "ch04"))

meso <- read.csv("gm_means.csv")


shade <- read.csv("shadeleafaci.csv")

# fit normal Aci curve (no gmeso)
fsun <- fitacis(sun, "chamber")
fshade <- fitacis(shade, "chamber")

# simulate from normal Aci curve
a <- Aci(Ci=seq(50,2000,by=10), Vcmax=mean(coef(fsun)$Vcmax), Jmax=mean(coef(fsun)$Jmax), Rd=mean(coef(fsun)$Rd))

# calculate Cc for the simulated curves
a$Cc <- with(a, Ci - ALEAF / 0.15)

# calculate Cc on measurements *using same gmeso as in above line*
sun$Cc <- with(sun, Ci - Photo/0.15)  # use correct gmeso!

# measurements
with(sun, plot(Cc, Photo))
# simulations
with(a, lines(Cc, ALEAF, col="red"))


a$Aleaf2 <- a$ALEAF + rnorm(nrow(a), mean=0, sd=0.1)
f <- fitaci(a, varnames = list(ALEAF = "Aleaf2", Tleaf = "Tleaf", Ci = "Ci", PPFD = "PPFD"),
            gmeso=0.15)

