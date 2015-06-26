### read in gm and test if different by temp

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ CTleaf+gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)

library(visreg)
library(multcomp)
library(nlme)

##linear model with chamber as random effect

gmt_sun_mod <- lme(gm~ CTleaf ,random=~1|chamber, data=gm_agg, subset=leaflight=="sun-high")
gmt_sha_mod <- lme(gm~ CTleaf ,random=~1|chamber, data=gm_agg,subset=leaflight=="shade-low")
gmt_fleck_mod <- lme(gm~ CTleaf ,random=~1|chamber, data=gm_agg,subset=leaflight=="shade-high")

  #sun leaves
  summary(gmt_sun_mod)
  anova(gmt_sun_mod)
  visreg(gmt_sun_mod)
  #shade leaves
  summary(gmt_sha_mod)
  anova(gmt_sha_mod)
  visreg(gmt_sha_mod)
  #sunfleck leaves
  summary(gmt_fleck_mod)
  anova(gmt_fleck_mod)
  visreg(gmt_fleck_mod)
  
##full model with temperature treatment
library(lme4)
sun_mod2 <- lmer(gm~ CTleaf * temp + (1|chamber), data=gm_agg, subset=leaflight =="sun-high")
  summary(sun_mod2)
  anova(sun_mod2)
  visreg(sun_mod2)

sha_mod2 <- lmer(gm~ CTleaf * temp + (1|chamber), data=gm_agg, subset=leaflight =="shade-low")
  summary(sha_mod2)
  anova(sha_mod2)
  visreg(sha_mod2)

fleck_mod2 <- lmer(gm~ CTleaf * temp + (1|chamber), data=gm_agg, subset=leaflight =="shade-high")
  summary(fleck_mod2)
  anova(fleck_mod2)
  visreg(fleck_mod2)



