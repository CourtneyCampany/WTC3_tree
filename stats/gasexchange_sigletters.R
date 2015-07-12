source("functions and packages/functions.R")
library(doBy)

gasexchange  <- read.csv("calculated_data/gmes_wellwatered.csv")

###leaf data
ge_agg <- summaryBy(Photo+Cond+Ci+Trmmol+gm+VpdL ~ chamber+id+leaf +light+temp+leaflight+Month+drydown, 
                    data=gasexchange, FUN=mean, keep.names=TRUE)
ge_agg$tukeyid <- as.factor(paste(ge_agg$leaflight, ge_agg$temp, sep="-"))

###linear models of gas exchange by treatments
library(visreg)
library(multcomp)
library(nlme)

#Photosynthesis
ge_sunsha <- ge_agg[ge_agg$leaflight != "shade-high",]
ge_sunsha <- droplevels(ge_sunsha)

A_sun_temp <- lme(Photo ~ temp ,random=~1|chamber, data=ge_sunsha, subset=leaflight=="sun-high")
summary(A_sun_temp)
anova(A_sun_temp)
visreg(A_sun_temp)

A_sha_temp <- lme(Photo ~ temp ,random=~1|chamber, data=ge_sunsha, subset=leaflight=="shade-low")
summary(A_sha_temp)
anova(A_sha_temp)
visreg(A_sha_temp)
##first no difference between temp treatments with sun shad leaves, 
##look at model without for sun vs shade

A_leaf <- lme(Photo~ leaf, random=~1|chamber, data=ge_sunsha, subset=leaflight != "shade-high")
summary(A_leaf)
anova(A_leaf)
visreg(A_leaf)
##A higher in sun leaves (23%)

##run full model to get sigletters for data table  
Photo_mod <- lme(Photo ~ tukeyid, random=~1|chamber, data=ge_sunsha, )  
Photo_mod2 <- lm(Photo ~ tukeyid,  data=ge_sunsha) 
#cant get tukeys to work with lme, so use simple model (for now)
anova(Photo_mod,Photo_mod2)  ###not different 

summary(Photo_mod)
  anova(Photo_mod)
  visreg(Photo_mod)
  
tukey_photo<- glht(A_leaf, linfct = mcp(leaf = "Tukey"))
photo_siglets2 <- tukey_photo$mcletters$Letters
write.csv(photo_siglets2, "master_scripts/sigletters/sl_photo.csv", row.names=FALSE)
  

# library(lme4)
#  Photo_mod2 <- lmer(Photo ~ leaf * temp + (1|chamber), data=ge_agg, subset=leaflight != "shade-high")
#  summary(Photo_mod2)
#  anova(Photo_mod2)
 
  
#mesophyll conductance
gm_sun_temp <- lme(gm ~ temp ,random=~1|chamber, data=ge_agg, subset=leaflight=="sun-high")
  summary(A_sun_temp)
  anova(A_sun_temp)
  visreg(A_sun_temp)

gm_sha_temp <- lme(gm ~ temp ,random=~1|chamber, data=ge_agg, subset=leaflight=="shade-low")
  summary(A_sha_temp)
  anova(A_sha_temp)
  visreg(A_sha_temp)

gm_leaf <- lme(gm~ leaf, random=~1|chamber, data=ge_agg, subset=leaflight != "shade-high")
  summary(A_leaf)
  anova(A_leaf)
  visreg(A_leaf)

#stomatal conducatnce----------------------------------------------------------------------------
gs_leaf <- lme(Cond~ leaf, random=~1|chamber, data=ge_agg, subset=leaflight != "shade-high")
  summary(gs_leaf)
  anova(gs_leaf)
  visreg(gs_leaf)
  
gs_sun_temp <- lme(Cond ~ temp ,random=~1|chamber, data=ge_agg, subset=leaflight=="sun-high")
  summary(gs_sun_temp)
  anova(gs_sun_temp)
  visreg(gs_sun_temp)
  
gs_sha_temp <- lme(Cond ~ temp ,random=~1|chamber, data=ge_agg, subset=leaflight=="shade-low")
  summary(gs_sha_temp)
  anova(gs_sha_temp)
  visreg(gs_sha_temp)
  
#ITE----------------------------------------------------------------------------
ITE <- read.csv("calculated_data/ITE.csv")  
  
  ite_leaf <- lme(iWUE~ leaf, random=~1|chamber, data=ITE, subset=leaflight != "shade-high")
  summary(ite_leaf)
  anova(ite_leaf)
  visreg(ite_leaf)
  
  ite_sun_temp <- lme(iWUE ~ temp ,random=~1|chamber, data=ITE, subset=leaflight=="sun-high")
  summary(ite_sun_temp)
  anova(ite_sun_temp)
  visreg(ite_sun_temp)
  
  ite_sha_temp <- lme(iWUE ~ temp ,random=~1|chamber, data=ITE, subset=leaflight=="shade-low")
  summary(ite_sha_temp)
  anova(ite_sha_temp)
  visreg(ite_sha_temp)
  
