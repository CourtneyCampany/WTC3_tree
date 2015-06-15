aci <- read.csv("calculated_data/aciparameters.csv")


###linear models of gas exchange by treatments
library(visreg)
library(multcomp)
library(nlme)

####leaf Narea--------------------------------------------------------------------------------------------------
#examite data with boxplots, then remove any outliers
boxplot(Jmax~temp, data=aci[aci$leaf =="sun",])
boxplot(Jmax~temp, data=aci[aci$leaf =="shade",])

boxplot(Vcmax~temp, data=aci[aci$leaf =="sun",])
boxplot(Vcmax~temp, data=aci[aci$leaf =="shade",])

#Jmax different by leaf type but not warming
J_sun_temp <- lme(Jmax ~ temp ,random=~1|chamber, data=aci, subset=leaf=="sun")
summary(J_sun_temp)
anova(J_sun_temp)
visreg(J_sun_temp)

J_sha_temp <- lme(Jmax ~ temp ,random=~1|chamber, data=aci, subset=leaf=="shade")
summary(J_sha_temp)
anova(J_sha_temp)
visreg(J_sha_temp)

J_leaf <- lme(Jmax ~ leaf, random=~1|chamber, data=aci)
summary(J_leaf)
anova(J_leaf)
visreg(J_leaf)


#Vcmax different by leaf type but not warming
V_sun_temp <- lme(Vcmax ~ temp ,random=~1|chamber, data=aci, subset=leaf=="sun")
summary(V_sun_temp)
anova(V_sun_temp)
visreg(V_sun_temp)

V_sha_temp <- lme(Vcmax ~ temp ,random=~1|chamber, data=aci, subset=leaf=="shade")
summary(V_sha_temp)
anova(V_sha_temp)
visreg(V_sha_temp)

V_leaf <- lme(Vcmax ~ leaf, random=~1|chamber, data=aci)
summary(V_leaf)
anova(V_leaf)
visreg(V_leaf)



