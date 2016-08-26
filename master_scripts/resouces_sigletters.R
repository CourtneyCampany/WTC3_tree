source("functions and packages/functions.R")
library(doBy)
library(visreg)
library(multcomp)
library(nlme)


treatments <- read.csv("raw data/temp_trt.csv") 

####generate sig letters for data tables for wtc3 manuscript

##variables include:  WP*2, K, Narea, 13C, Vcmax, Jmax



####Narea/LMA/13c--------------------------------------------------------------------------------------------------------
leaf_chem <- read.csv("raw data/leaf_chem.csv")
leaf_mass <- read.csv("raw data/leaf_data.csv")

#calculate nitrogen content in leaves
canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
  #add treatments  
  canopy_chem <- addtrt_func(canopy_chem)
  canopy_chem$tukeyid <- as.factor(paste(canopy_chem$leaf, canopy_chem$temp, sep="-"))
  canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
  canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
  canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
  canopy_chem<- add_campaign(canopy_chem)
  
  canopy_chem2 <- canopy_chem[canopy_chem$drydown=="control",]
  
  
#3: lma: no effect of ET  
lma_temp <- lme(lma ~ temp ,random=~1|chamber, data=canopy_chem2)
  summary(lma_temp)
  anova(lma_temp)
  visreg(lma_temp)
  
##full model
lma_leaf <- lme(lma~ tukeyid, random=~1|chamber, data=canopy_chem2)
  summary(lma_leaf)
  anova(lma_leaf)
  visreg(lma_leaf)
  
tukey_lma<- glht(lma_leaf, linfct = mcp(tukeyid = "Tukey"))
lma_siglets<- cld(tukey_lma)
lma_siglets2 <- lma_siglets$mcletters$Letters
  
write.csv(lma_siglets2, "master_scripts/sigletters/slr_lma.csv", row.names=FALSE)

#4: leaf N
n_temp <- lme(leafN_area ~ temp ,random=~1|chamber, data=canopy_chem2)
  summary(n_temp)
  anova(n_temp)
  visreg(n_temp)

##full model
n_leaf <- lme(leafN_area~ tukeyid, random=~1|chamber, data=canopy_chem2)
  summary(n_leaf)
  anova(n_leaf)
  visreg(n_leaf)

tukey_n<- glht(n_leaf, linfct = mcp(tukeyid = "Tukey"))
  n_siglets<- cld(tukey_n)
  n_siglets2 <- n_siglets$mcletters$Letters

write.csv(n_siglets2, "master_scripts/sigletters/slr_n.csv", row.names=FALSE)

#4: c13
c13_temp <- lme(c13 ~ temp ,random=~1|chamber, data=canopy_chem2)
  summary(c13_temp)
  anova(c13_temp)
  visreg(c13_temp)

##full model
c13_leaf <- lme(c13~ tukeyid, random=~1|chamber, data=canopy_chem2)
  summary(c13_leaf)
  anova(c13_leaf)
  visreg(c13_leaf)

tukey_c13<- glht(c13_leaf, linfct = mcp(tukeyid = "Tukey"))
c13_siglets<- cld(tukey_c13)
c13_siglets2 <- c13_siglets$mcletters$Letters

write.csv(c13_siglets2, "master_scripts/sigletters/slr_c13.csv", row.names=FALSE)


####waterpotential and leafK-------------------------------------------------------------------------------------------------
waterdat <- read.csv("calculated_data/leafK_nodrought.csv")
  waterdat$tukeyid <- as.factor(paste(waterdat$leaf, waterdat$temp, sep="-"))
  
##leaf K stats moved to newleafK.R in masters scripts and ran with high light shade leaves, new sigletters under same name  
  
#5: leafK
# k_temp <- lme(leafK ~ temp ,random=~1|chamber, data=waterdat)
#   summary(k_temp)
#   anova(k_temp)
#   visreg(k_temp)
#   
# ##full model
# k_leaf <- lme(leafK~ tukeyid, random=~1|chamber, data=waterdat)
#   summary(k_leaf)
#   anova(k_leaf)
#   visreg(k_leaf)
#   
# tukey_k<- glht(k_leaf, linfct = mcp(tukeyid = "Tukey"))
# k_siglets<- cld(tukey_k)
# k_siglets2 <- k_siglets$mcletters$Letters
# 
# write.csv(k_siglets2, "master_scripts/sigletters/slr_k.csv", row.names=FALSE)

#6: wp_pre
pre_temp <- lme(pre_mp ~ temp ,random=~1|chamber, data=waterdat)
  summary(pre_temp)
  anova(pre_temp)
  visreg(pre_temp)

##full model
pre_leaf <- lme(pre_mp~ tukeyid, random=~1|chamber, data=waterdat)
  summary(pre_leaf)
  anova(pre_leaf)
  visreg(pre_leaf)

tukey_pre<- glht(pre_leaf, linfct = mcp(tukeyid = "Tukey"))
pre_siglets<- cld(tukey_pre)
pre_siglets2 <- pre_siglets$mcletters$Letters

write.csv(pre_siglets2, "master_scripts/sigletters/slr_pre.csv", row.names=FALSE)

#7: wp_mid
mid_temp <- lme(mid_mp ~ temp ,random=~1|chamber, data=waterdat)
  summary(mid_temp)
  anova(mid_temp)
  visreg(mid_temp)

##full model
mid_leaf <- lme(mid_mp~ tukeyid, random=~1|chamber, data=waterdat)
  summary(mid_leaf)
  anova(mid_leaf)
  visreg(mid_leaf)

tukey_mid<- glht(mid_leaf, linfct = mcp(tukeyid = "Tukey"))
mid_siglets<- cld(tukey_mid)
mid_siglets2 <- mid_siglets$mcletters$Letters

write.csv(mid_siglets2, "master_scripts/sigletters/slr_mid.csv", row.names=FALSE)


####aci parameters--------------------------------------------------------------------------------------------------



####new ACc stats are in the jmaxvcmaxgmes script
###these are now redundant

# aciparam <- read.csv("calculated_data/aciparameters.csv")
# aciparam$tukeyid <- as.factor(paste(aciparam$leaf, aciparam$temp, sep="-"))
# 
# #1: jmax : no effect of ET
# J_temp <- lme(Jmax ~ temp ,random=~1|chamber, data=aciparam)
# summary(J_temp)
# anova(J_temp)
# visreg(J_temp)
# 
# ##full model
# 
# J_leaf <- lme(Jmax~ tukeyid, random=~1|chamber, data=aciparam)
# summary(J_leaf)
# anova(J_leaf)
# visreg(J_leaf)
# 
# tukey_Jmax<- glht(J_leaf, linfct = mcp(tukeyid = "Tukey"))
# Jmax_siglets<- cld(tukey_Jmax)
# Jmax_siglets2 <- Jmax_siglets$mcletters$Letters
# 
# write.csv(Jmax_siglets2, "master_scripts/sigletters/slr_jmax.csv", row.names=FALSE)
# 
# #2: Vcmax: no effect of ET
# 
# vc_temp <- lme(Vcmax ~ temp ,random=~1|chamber, data=aciparam)
# summary(vc_temp)
# anova(vc_temp)
# visreg(vc_temp)
# 
# ##full model
# 
# vc_leaf <- lme(Vcmax~ tukeyid, random=~1|chamber, data=aciparam)
# summary(vc_leaf)
# anova(vc_leaf)
# visreg(vc_leaf)
# 
# tukey_vcmax<- glht(vc_leaf, linfct = mcp(tukeyid = "Tukey"))
# vcmax_siglets<- cld(tukey_vcmax)
# vcmax_siglets2 <- vcmax_siglets$mcletters$Letters
# 
# write.csv(vcmax_siglets2, "master_scripts/sigletters/slr_vcmax.csv", row.names=FALSE)


