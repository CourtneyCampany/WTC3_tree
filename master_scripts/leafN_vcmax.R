source("master_scripts/plot_objects.R")

###need to correlate vcmax with leaf N

###get vcmax value per chamber per leaf

aciparam <- read.csv("calculated_data/aciparameters.csv")


### get leaf N by chamber
leafN <- read.csv("calculated_data/leaf_chemistry.csv")
#no drought
leafN2 <- leafN[leafN$drydown != "drought",]

library(doBy)
palette(c(shacol, suncol))

Nagg <- summaryBy(leafN_area ~ chamber + leaf, data=leafN2)

N_aci <- merge(aciparam, Nagg, by= c("chamber", "leaf"))

sun <- N_aci[N_aci$leaf=="sun",]
shade <- N_aci[N_aci$leaf=="shade",]


###stats

library(mgcv)
library(lme4)
library(lmerTest)
library(plotrix)

aciN_sun_lm <- lm(Vcmax~ leafN_area.mean, data=sun)
aciN_sha_lm <- lm(Vcmax~ leafN_area.mean, data=shade)


plot(Vcmax~leafN_area.mean, data=N_aci, col=as.factor(leaf), xlim=c(1,4), ylim=c(0, 130), pch=16)
ablineclip(aciN_sun_lm, x1=min(sun$leafN_area.mean), x2=max(sun$leafN_area.mean), col=suncol, lwd=2)
ablineclip(aciN_sha_lm, x1=min(shade$leafN_area.mean), x2=max(shade$leafN_area.mean), col=shacol, lwd=2)
