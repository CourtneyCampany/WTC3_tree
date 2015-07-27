source("functions and packages/functions.R")
library(doBy)

gasexchange  <- read.csv("calculated_data/gmes_wellwatered.csv")

ITE <- read.csv("calculated_data/ITE.csv")  
ITE$tukeyid <- as.factor(paste(ITE$leaflight, ITE$temp, sep="-"))

###leaf data
ge_agg <- summaryBy(Photo+Cond+Ci+Trmmol+gm+VpdL ~ chamber+id+leaf +light+temp+leaflight+Month+drydown, 
                    data=gasexchange, FUN=mean, keep.names=TRUE)
ge_agg$tukeyid <- as.factor(paste(ge_agg$leaflight, ge_agg$temp, sep="-"))

lightson <- ge_agg[ge_agg$leaflight == "shade-high",]
lightson <- droplevels(lightson)

###linear models of gas exchange by treatments
library(visreg)
library(multcomp)
library(nlme)

####photosynthesis------------------------------------------------------------------------------------------
A_temp <- lme(Photo ~ temp ,random=~1|chamber, data=lightson)
  summary(A_temp)
  anova(A_temp)
  visreg(A_temp)

##marginal difference of temp
##look at model without for sun vs shade

A_leaf <- lme(Photo~ tukeyid, random=~1|chamber, data=ge_agg)
  summary(A_leaf)
  anova(A_leaf)
  
tukey_photo<- glht(A_leaf, linfct = mcp(tukeyid = "Tukey"))
  A_lightson_siglets<- cld(tukey_photo)
  A_lightson_siglets2 <- A_lightson_siglets$mcletters$Letters

  visreg(A_leaf)
write.csv(A_lightson_siglets2, "master_scripts/sigletters/sl_A_lightson.csv", row.names=FALSE)

###ITE----------------------------------------------------------------------------------------------------------------
ite_leaf <- lme(iWUE ~ tukeyid ,random=~1|chamber, data=ITE)
summary(ite_leaf)
anova(ite_leaf)
visreg(ite_leaf)

tukey_ite<- glht(ite_leaf, linfct = mcp(tukeyid = "Tukey"))
ite_lightson_siglets<- cld(tukey_ite)
ite_lightson_siglets2 <- ite_lightson_siglets$mcletters$Letters

write.csv(ite_lightson_siglets2, "master_scripts/sigletters/sl_ite_lightson.csv", row.names=FALSE)

##no effect of warming
ite_temp_lo <- lme(iWUE ~ temp ,random=~1|chamber, data=ITE, subset=leaflight=="shade-high")
summary(ite_temp_lo)
anova(ite_temp_lo)
visreg(ite_temp_lo)


##stomatal conductance-----------------------------------------------------------------------------------------

gs_temp <- lme(Cond ~ temp ,random=~1|chamber, data=lightson)
summary(gs_temp)
anova(gs_temp)


gs_leaf <- lme(Cond~ tukeyid, random=~1|chamber, data=ge_agg)
summary(gs_leaf)
anova(gs_leaf)
visreg(gs_leaf)

tukey_gs<- glht(gs_leaf, linfct = mcp(tukeyid = "Tukey"))
gs_lightson_siglets<- cld(tukey_gs)
gs_lightson_siglets2 <- gs_lightson_siglets$mcletters$Letters

visreg(gs_leaf)
write.csv(gs_lightson_siglets2, "master_scripts/sigletters/sl_gs_lightson.csv", row.names=FALSE)
  
###mesophyll conductance-----------------------------------------------------------------------------------------
gm_temp <- lme(gm ~ temp ,random=~1|chamber, data=lightson)
summary(gm_temp)
anova(gm_temp)

gm_leaf <- lme(gm~ tukeyid, random=~1|chamber, data=ge_agg)
summary(gm_leaf)
anova(gm_leaf)
visreg(gm_leaf)

tukey_gm<- glht(gm_leaf, linfct = mcp(tukeyid = "Tukey"))
gm_lightson_siglets<- cld(tukey_gm)
gm_lightson_siglets2 <- gm_lightson_siglets$mcletters$Letters

visreg(gm_leaf)
write.csv(gm_lightson_siglets2, "master_scripts/sigletters/sl_gm_lightson.csv", row.names=FALSE)

###vpd-----------------------------------------------------------------------------------------
vpd_temp <- lme(VpdL ~ temp ,random=~1|chamber, data=lightson)
summary(vpd_temp)
anova(vpd_temp)
visreg(vpd_temp)

vpd_leaf <- lme(VpdL~ tukeyid, random=~1|chamber, data=ge_agg)
  summary(vpd_leaf)
  anova(vpd_leaf)
  visreg(vpd_leaf)

tukey_vpd<- glht(vpd_leaf, linfct = mcp(tukeyid = "Tukey"))
vpd_lightson_siglets<- cld(tukey_vpd)
vpd_lightson_siglets2 <- vpd_lightson_siglets$mcletters$Letters

write.csv(vpd_lightson_siglets2, "master_scripts/sigletters/sl_vpd_lightson.csv", row.names=FALSE)

###E-----------------------------------------------------------------------------------------
e_temp <- lme(Trmmol ~ temp ,random=~1|chamber, data=lightson)
summary(e_temp)
anova(e_temp)

e_leaf <- lme(Trmmol~ tukeyid, random=~1|chamber, data=ge_agg)
summary(e_leaf)
anova(e_leaf)
visreg(e_leaf)

tukey_e<- glht(e_leaf, linfct = mcp(tukeyid = "Tukey"))
e_lightson_siglets<- cld(tukey_e)
e_lightson_siglets2 <- e_lightson_siglets$mcletters$Letters

write.csv(e_lightson_siglets2, "master_scripts/sigletters/sl_e_lightson.csv", row.names=FALSE)
  