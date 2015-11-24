### read in gm and test if different by temp

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ Cond+CTleaf+gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)

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
  
gmt_sun_mod2 <- lm(gm~ CTleaf, data=gm_agg, subset=leaflight=="sun-high")  
  summary(gmt_sun_mod2)  
  
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


####Cond
condt_sun_mod <- lme(Cond~ CTleaf ,random=~1|chamber, data=gm_agg, subset=leaflight=="sun-high")
condt_sha_mod <- lme(Cond~ CTleaf ,random=~1|chamber, data=gm_agg,subset=leaflight=="shade-low")
condt_fleck_mod <- lme(Cond~ CTleaf ,random=~1|chamber, data=gm_agg,subset=leaflight=="shade-high")
  
  #sun leaves
  summary(condt_sun_mod)
  anova(condt_sun_mod)
  visreg(condt_sun_mod)
  #shade leaves
  summary(condt_sha_mod)
  anova(condt_sha_mod)
  visreg(condt_sha_mod)
  #sunfleck leaves
  summary(condt_fleck_mod)
  anova(condt_fleck_mod)
  visreg(condt_fleck_mod)
  
condt_fleck_mod2 <- lm(Cond~ CTleaf,data=gm_agg,subset=leaflight=="shade-high")  
summary(condt_fleck_mod2)
  
####A
  At_sun_mod <- lme(Photo~ CTleaf ,random=~1|chamber, data=gm_agg, subset=leaflight=="sun-high")
  At_sha_mod <- lme(Photo~ CTleaf ,random=~1|chamber, data=gm_agg,subset=leaflight=="shade-low")
  At_fleck_mod <- lme(Photo~ CTleaf ,random=~1|chamber, data=gm_agg,subset=leaflight=="shade-high")
  
  #sun leaves
  summary(At_sun_mod)
  anova(At_sun_mod)
  visreg(At_sun_mod)
  #shade leaves
  summary(At_sha_mod)
  anova(At_sha_mod)
  visreg(At_sha_mod)
  #sunfleck leaves
  summary(At_fleck_mod)
  anova(At_fleck_mod)
  visreg(At_fleck_mod)  
  
  At_sun_mod2 <- lm(Photo~ CTleaf, data=gm_agg, subset=leaflight=="sun-high")  
  summary(At_sun_mod2)
  