### read in gm and test if different by temp

gmes <- read.csv("calculated_data/gmes_WTC.csv")

###for analysis first subset well watered and drought treatments
gm_drought <- gmes[gmes$drydown == "drought",]
gm_water <- gmes[gmes$drydown == "control",]

library(lme4)

gmt_sun_wet <- lmer(gm~ CTleaf + (1|chamber), data=gm_water, subset=leaflight=="sun-high")
gmt_sha_wet <- lmer(gm~ CTleaf + (1|chamber), data=gm_water,subset=leaflight=="sun-high")
gmt_sun_dry <- lmer(gm~ CTleaf + (1|chamber), data=gm_drought,subset=leaflight=="shade-low")
gmt_sha_dry <- lmer(gm~ CTleaf + (1|chamber), data=gm_drought,subset=leaflight=="shade-low")

test <- lmer(gm~ CTleaf + (1|chamber), data=gm_water)

###summary
summary(gmt_sun_wet)
gmt_sun_wet2 <- update(gmt_sun_wet, REML=FALSE)
gmt_sun_wet3 <- lm(gm~ CTleaf, data=gm_water, subset=leaflight=="sun-high")


plot(gm~CTleaf, data=gm_water,col=leaflight, pch=16)
  plot(gm~CTleaf, data=gm_water,subset=leaflight=="sun-high", col=leaflight, pch=16)
  plot(gm~CTleaf, data=gm_water,subset=leaflight=="shade-low",col=leaflight, pch=16)
  plot(gm~CTleaf, data=gm_water,subset=leaflight=="shade-high",col=leaflight, pch=16)

plot(gm~CTleaf, data=gm_drought, col=leaflight, pch=16)
  plot(gm~CTleaf, data=gm_drought,subset=leaflight=="sun-high", col=leaflight, pch=16)
  plot(gm~CTleaf, data=gm_drought,subset=leaflight=="shade-low",col=leaflight, pch=16)
  plot(gm~CTleaf, data=gm_drought,subset=leaflight=="shade-high",col=leaflight, pch=16)
