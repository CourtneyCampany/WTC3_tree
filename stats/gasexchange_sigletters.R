source("functions and packages/functions.R")
library(doBy)

gasexchange  <- read.csv("calculated_data/gmes_wtc.csv")

###leaf data
ge_agg <- summaryBy(Photo+Cond+Ci+Trmmol+gm+VpdL ~ chamber+id+leaf +light+temp+leaflight+Month+drydown, 
                    data=gasexchange, FUN=mean, keep.names=TRUE)

###split by drought and then rearrange
ge_wet <- ge_agg[ge_agg$drydown == "control",]
ge_dry <- ge_agg[ge_agg$drydown == "drought",]


###linear models of gas exchange by treatments
library(visreg)
library(multcomp)
library(nlme)

#Photosynthesis
A_sun_temp <- lme(Photo ~ temp ,random=~1|chamber, data=ge_wet, subset=leaflight=="sun-high")
  summary(A_sun_temp)
  anova(A_sun_temp)
  visreg(A_sun_temp)

A_sha_temp <- lme(Photo ~ temp ,random=~1|chamber, data=ge_wet, subset=leaflight=="shade-low")
  summary(A_sha_temp)
  anova(A_sha_temp)
  visreg(A_sha_temp)

A_leaf <- lme(Photo~ leaf, random=~1|chamber, data=ge_wet, subset=leaflight != "shade-high")
  summary(A_leaf)
  anova(A_leaf)
  visreg(A_leaf)
  
  
Photo_mod <- lme(Photo ~ leaf * temp, random=~1|chamber, data=ge_wet, subset=leaflight != "shade-high")  
  summary(Photo_mod)
  anova(Photo_mod)
  visreg(Photo_mod)
  
library(lme4)
 Photo_mod2 <- lmer(Photo ~ leaf * temp + (1|chamber), data=ge_wet, subset=leaflight != "shade-high")
 summary(Photo_mod2)
 anova(Photo_mod2)
  
#mesophyll conductance
gm_sun_temp <- lme(gm ~ temp ,random=~1|chamber, data=ge_wet, subset=leaflight=="sun-high")
  summary(A_sun_temp)
  anova(A_sun_temp)
  visreg(A_sun_temp)

gm_sha_temp <- lme(gm ~ temp ,random=~1|chamber, data=ge_wet, subset=leaflight=="shade-low")
  summary(A_sha_temp)
  anova(A_sha_temp)
  visreg(A_sha_temp)

gm_leaf <- lme(gm~ leaf, random=~1|chamber, data=ge_wet, subset=leaflight != "shade-high")
  summary(A_leaf)
  anova(A_leaf)
  visreg(A_leaf)

#stomatal conducatnce
gs_sun_temp <- lme(Cond ~ temp ,random=~1|chamber, data=ge_wet, subset=leaflight=="sun-high")
  summary(gs_sun_temp)
  anova(gs_sun_temp)
  visreg(gs_sun_temp)
  
gs_sha_temp <- lme(Cond ~ temp ,random=~1|chamber, data=ge_wet, subset=leaflight=="shade-low")
  summary(gs_sha_temp)
  anova(gs_sha_temp)
  visreg(gs_sha_temp)
  
gs_leaf <- lme(Cond~ leaf, random=~1|chamber, data=ge_wet, subset=leaflight != "shade-high")
  summary(gs_leaf)
  anova(gs_leaf)
  visreg(gs_leaf)
