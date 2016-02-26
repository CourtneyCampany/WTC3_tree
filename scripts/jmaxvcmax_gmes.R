source("functions and packages/functions.R")

#####redo aci curves with gmes to get new Jmax and Vcmax

#read in gmes, get simple means by treatment
gm <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <-summaryBy(gm ~ leaflight+temp, data=gm, FUN=mean, keep.names=TRUE)

#read in temperature treatments
treatments <- read.csv("raw data/temp_trt.csv") 

##read in aci data
acishade <- read.csv("raw data/shadeaci.csv")
acishade <- merge(acishade, treatments)

##shade leaves-------------------------------------------------------------------------------------------------------------

#clean from previous script
acishade_clean <- acishade[acishade$chamber != "ch02",]
acishade_clean <- acishade_clean[acishade_clean$chamber != "ch07",]
acishade_clean <- acishade_clean[acishade_clean$chamber != "ch09",]
acishade_clean <- acishade_clean[acishade_clean$chamber != "ch11",]

##will have to include temperature treatment so aci data will have to be further subsetted
acishade_clean_at <- acishade_clean[acishade_clean$temp == "ambient",]
  acishade_clean_at <-droplevels(acishade_clean_at)

acishade_clean_et <- acishade_clean[acishade_clean$temp == "elevated",]
  acishade_clean_et <- droplevels(acishade_clean_et)

shade_redo <- read.csv("raw data/shadeaci_redo.csv")
  shade_redo <- merge(shade_redo, treatments)

shade_redo_at <- shade_redo[shade_redo$temp == "ambient",]
  shade_redo_at <- droplevels(shade_redo_at)

shade_redo_et <- shade_redo[shade_redo$temp == "elevated",]
  shade_redo_et <- droplevels(shade_redo_et)


##sunleaves-------------------------------------------------------------------------------------------------------------------
sunaci <- read.csv("raw data/sunaci.csv")
sunaci <-merge(sunaci, treatments)

#clean from previous script
sunaci_clean <- sunaci[sunaci$chamber != "ch06", ]
  sunaci_clean2 <- sunaci_clean[sunaci_clean$chamber != "ch04", ]
  
  sunaci_clean2_at <- sunaci_clean2[sunaci_clean2$temp == "ambient",]
  sunaci_clean2_at <- droplevels(sunaci_clean2_at)
  
  sunaci_clean2_et <- sunaci_clean2[sunaci_clean2$temp == "elevated",]
  sunaci_clean2_et <- droplevels(sunaci_clean2_et)

  
##this dataframe is only at treatments  
tdlaci2 <- read.csv("raw data/tdlaci2.csv")
  tdlaci2 <-merge(tdlaci2, treatments)
  tdlaci2 <- droplevels(tdlaci2)


library(plantecophys)

##fitaci with gmes for sun and shade leaves by temperature treatment----------------------------------------------------------

#shade leaves
fitacishade_at<-fitacis(acishade_clean_at, "chamber", varnames = list(ALEAF="Photo", gmes0 = gm_agg[3,3],
                Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), Tcorrect=TRUE)

fitacishade_et<-fitacis(acishade_clean_et, "chamber", varnames = list(ALEAF="Photo", gmeso = gm_agg[4,3],
                Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), Tcorrect=TRUE)

fitacishade_redo_at <- fitacis(shade_redo_at, "chamber", varnames = list(ALEAF="Photo", gmes0 = gm_agg[3,3],
                      Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), Tcorrect=TRUE)

fitacishade_redo_et <- fitacis(shade_redo_et, "chamber", varnames = list(ALEAF="Photo", gmes0 = gm_agg[4,3],
                      Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), Tcorrect=TRUE)

#sun leaves
fitacitdlaci2 <- fitacis(tdlaci2, "chamber", varnames = list(ALEAF="Photo", gmes0 = gm_agg[5,3],
                             Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), Tcorrect=TRUE)

fitacisunaci_clean2_at <- fitacis(sunaci_clean2_at, "chamber", varnames = list(ALEAF="Photo", gmes0 = gm_agg[5,3],
                                  Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), Tcorrect=TRUE)

fitacisunaci_clean2_et <- fitacis(sunaci_clean2_et, "chamber", varnames = list(ALEAF="Photo", gmes0 = gm_agg[6,3],
                                  Tleaf = "Tleaf", Ci="Ci", PPFD="PARi"), Tcorrect=TRUE)


#extract jmax and vcmax (compare to standard aci curves)
shade_at_coef <- coef(fitacishade_at)
shade_et_coef <- coef(fitacishade_et)
shade_redo_at_coef <- coef(fitacishade_redo_at)
shade_redo_et_coef <- coef(fitacishade_redo_et)

tdlaci2_coef <- coef(fitacitdlaci2)
sunaci_clean2_at_coef <- coef(fitacisunaci_clean2_at)
sunaci_clean2_et_coef <- coef(fitacisunaci_clean2_et)

#generate treatment means for vcmax and jmax

#merge shade, add leaf type designation
jmaxvcmax_gmes_sha <- rbind(shade_at_coef, shade_et_coef)
jmaxvcmax_gmes_sha <- rbind(jmaxvcmax_gmes_sha, shade_redo_at_coef)
jmaxvcmax_gmes_sha <- rbind(jmaxvcmax_gmes_sha, shade_redo_et_coef)
jmaxvcmax_gmes_sha$leaf <- "shade"


jmaxvcmax_gmes_sun <- rbind(tdlaci2_coef, sunaci_clean2_at_coef)
jmaxvcmax_gmes_sun <- rbind(jmaxvcmax_gmes_sun, sunaci_clean2_et_coef)
jmaxvcmax_gmes_sun$leaf <- "sun"

jmaxvcmax_gmes <- rbind(jmaxvcmax_gmes_sun, jmaxvcmax_gmes_sha)


#order chambers
chamberorder<-order(jmaxvcmax_gmes$chamber, by=jmaxvcmax_gmes$Vcmax)
jmaxvcmax_gmes <- jmaxvcmax_gmes[chamberorder,]
jmaxvcmax_gmes <- merge(jmaxvcmax_gmes, treatments, by = "chamber")

library(doBy)
aci_means <- summaryBy(Vcmax+Jmax ~ temp+leaf , data = jmaxvcmax_gmes,  FUN=c(mean,se))
aci_means2 <- summaryBy(Vcmax+Jmax ~ leaf , data = jmaxvcmax_gmes,  FUN=c(mean,se))

##compare these to infinite gm parameters
aci_nogm <- read.csv("calculated_data/aci_sunsha.csv")

#write to csv
write.csv(jmax_vcmax, file = "calculated data/jmax_vcmax_gmes.csv", row.names=FALSE)   


##run some stats on jmax, vcmax----------------------------------------------------------------------------------------
jmaxvcmax_gmes$tukeyid <- as.factor(paste(jmaxvcmax_gmes$leaf, jmaxvcmax_gmes$temp, sep="-"))
library(visreg)
library(multcomp)

J_temp <- lme(Jmax ~ temp ,random=~1|chamber, data=jmaxvcmax_gmes) ##no warming effect
summary(J_temp)
anova(J_temp)
visreg(J_temp)

##full model

J_leaf <- lme(Jmax~ tukeyid, random=~1|chamber, data=jmaxvcmax_gmes)
summary(J_leaf)
anova(J_leaf)
visreg(J_leaf)

tukey_Jmax<- glht(J_leaf, linfct = mcp(tukeyid = "Tukey"))
Jmax_siglets<- cld(tukey_Jmax)
Jmax_siglets2 <- Jmax_siglets$mcletters$Letters


#2: Vcmax: no effect of ET

vc_temp <- lme(Vcmax ~ temp ,random=~1|chamber, data=jmaxvcmax_gmes)
summary(vc_temp)
anova(vc_temp)
visreg(vc_temp)

##full model

vc_leaf <- lme(Vcmax~ tukeyid, random=~1|chamber, data=jmaxvcmax_gmes)
summary(vc_leaf)
anova(vc_leaf)
visreg(vc_leaf)

tukey_vcmax<- glht(vc_leaf, linfct = mcp(tukeyid = "Tukey"))
vcmax_siglets<- cld(tukey_vcmax)
vcmax_siglets2 <- vcmax_siglets$mcletters$Letters

