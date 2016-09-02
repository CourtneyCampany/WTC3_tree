#leaf chemistry stats
source("functions and packages/functions.R")
library(doBy)
palette = c("blue", "red")


leafchem <- read.csv("calculated_data/leaf_chemistry.csv")


###split by drought and then rearrange
leaf_wet <- leafchem[leafchem$drydown == "control",]
leaf_dry <- leafchem[leafchem$drydown == "drought",]

leaf_wet$id <- paste(leaf_wet$campaign, leaf_wet$leaf, leaf_wet$chamber, sep="-")


###linear models of gas exchange by treatments
library(visreg)
library(multcomp)
library(nlme)

####leaf Narea--------------------------------------------------------------------------------------------------
#examite data with boxplots, then remove any outliers
boxplot(leafN_area~temp, data=leaf_wet[leaf_wet$leaf =="sun",])
boxplot(leafN_area~temp, data=leaf_wet[leaf_wet$leaf =="shade",])

leafN_clean <- leaf_wet[leaf_wet$leafN_area <= 3.53 & leaf_wet$leafN_area >= 1.0,]

#N area not different by temp treatment in sun but is in shade, also higher in sun than shade
N_sun_temp <- lme(leafN_area ~ temp ,random=~1|chamber, data=leafN_clean, subset=leaf=="sun")
summary(N_sun_temp)
anova(N_sun_temp)
visreg(N_sun_temp)

N_sha_temp <- lme(leafN_area ~ temp ,random=~1|chamber, data=leafN_clean, subset=leaf=="shade")
summary(N_sha_temp)
anova(N_sha_temp)
visreg(N_sha_temp)
#how big and when was this effect present
Nshade <- summaryBy(leafN_area ~ temp, data=leafN_clean[leafN_clean$leaf=="shade",],  FUN=c(mean,se))


N_leaf <- lme(leafN_area ~ leaf, random=~1|chamber, data=leafN_clean)
summary(N_leaf)
anova(N_leaf)
visreg(N_leaf)

##overall model
leafN_mod <- lme(leafN_area ~ leaf * temp, random=~1|chamber, data=leafN_clean)  
summary(leafN_mod)
anova(leafN_mod)
visreg(leafN_mod)

###leaf mass per area---------------------------------------------------------------------------------------------------
#examite data with boxplots, then remove any outliers
boxplot(lma~temp, data=leaf_wet[leaf_wet$leaf =="sun",])
boxplot(lma~temp, data=leaf_wet[leaf_wet$leaf =="shade",])

lma_clean <- leaf_wet[leaf_wet$lma <= 170,]

###lma not different between leaf types or temp treatment.
lma_leaf <- lme(lma ~ leaf, random=~1|chamber, data=lma_clean)
summary(lma_leaf)
anova(lma_leaf)

lma_sun_temp <- lme(lma ~ temp ,random=~1|chamber, data=lma_clean, subset=leaf=="sun")
summary(lma_sun_temp)
anova(lma_sun_temp)

lma_sha_temp <- lme(lma ~ temp ,random=~1|chamber, data=lma_clean, subset=leaf=="shade")
summary(lma_sha_temp)
anova(lma_sha_temp)

####leaf K---------------------------------------------------------------------------------------------------------------

leafK <- read.csv("calculated_data/leafK_nodrought.csv")
#with highlight instead
leafK2 <- read.csv("calculated_data/leafK_nodrought_highlight.csv")

#examite data with boxplots, then remove any outliers
boxplot(leafK~temp, data=leafK[leafK$leaf =="sun",])
boxplot(leafK~temp, data=leafK[leafK$leaf =="shade",])

boxplot(leafK~temp, data=leafK2[leafK2$leaf =="sun",])
boxplot(leafK~temp, data=leafK2[leafK2$leaf =="shade",])

leafK_clean <- leafK[leafK$leafK <=4.75,]
leafK2_clean <- leafK2[leafK2$leafK <=4.75,]

leafK_leaf <- lme(leafK ~ leaf, random=~1|chamber, data=leafK2_clean)
summary(leafK_leaf)
anova(leafK_leaf)
visreg(leafK_leaf)


##no temperature effects
leafK_sun_temp <- lme(leafK ~ temp ,random=~1|chamber, data=leafK2_clean, subset=leaf=="sun")
summary(leafK_sun_temp)
anova(leafK_sun_temp)

leafK_sha_temp <- lme(leafK ~ temp ,random=~1|chamber, data=leafK2_clean, subset=leaf=="shade")
summary(leafK_sha_temp)
anova(leafK_sha_temp)


