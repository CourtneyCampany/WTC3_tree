library(plantecophys)
library(doBy)
library(plotrix)
library(scales)

source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")


####dataset for ACi curve predictions------------------------------------------------------------------------------

#read in gm data set (no drought) and CI from discrimination
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
Ci_bar <- read.csv("calculated_data/Ci_bar.csv")
  ##calculate CC
  gmes$Cc<- with(gmes, Ci-Photo/gm)
  gmes$cc_ci<- with(gmes, Ci/Cc)

  ##need to combine Ci_bar with CC and gm
  
  ###get average by id
  gm_agg <- summaryBy(Photo+Cond+Ci+Cc+gm+VpdL+xsi+DELTA+cc_ci+PARi ~ id+leaf +light+temp+leaflight, 
                    data=gmes, FUN=mean, keep.names=TRUE)

  ##remove shade-high
  gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
  gm_sunsha <- droplevels(gm_sunsha)
  gm_sunsha$id <- gsub("-high", "", gm_sunsha$id)
  gm_sunsha$id <- gsub("-low", "", gm_sunsha$id)

  ##merge
  gm_c13 <- merge(gm_sunsha, Ci_bar[, c(2,8)], by="id")
  gm_c13$Cc_bar <- with(gm_c13, ci_bar-Photo/gm)
  
  
  ###Photosynthesis vs Ci--------------------------------------------------------------------------------
  
  #SUN leaves
  Aci_sun_lm <- lm(Photo~ Ci, data=gm_c13,subset=leaflight=="sun-high")
  
  #predict
  #get apprpriate vector CC from sun leaves
  cidat <- gm_c13[gm_c13$leaflight=="sun-high", "Ci"]
  #generate sequence and then predict
  cisun_seq <- seq(min(cidat), max(cidat), length=101)
  cisun_pred <- predict.lm(Aci_sun_lm, newdata=data.frame(Ci=cisun_seq), interval="confidence")
  
  #SHADE leaves
  Aci_shade_lm <- lm(Photo~ Ci, data=gm_c13, subset=leaflight=="shade-low")
  
  #get apprpriate vector CC from sun leaves
  cidat2 <- gm_c13[gm_c13$leaflight=="shade-low", "Ci"]
  #generate sequence and then predict
  cisha_seq <- seq(min(cidat2), max(cidat2), length=101)
  cisha_pred <- predict.lm(Aci_shade_lm, newdata=data.frame(Ci=cisha_seq), interval="confidence")


    
####use plant ecophys to predict aci curve--------------------------------------------------------------------------
  #### plotbits

  shade <- gm_c13[gm_c13$leaf=="shade",]
  sun <- gm_c13[gm_c13$leaf=="sun",]
  
  #read in jmax vcmax
  jvc<- read.csv("calculated_data/tdl_aci.csv")

  
  #simulate ACi curves for each leaf, ambient and elevated T
  sunAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[3,3], Jmax=jvc[3,4],PPFD=mean(sun$PARi))
  sunET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[4,3], Jmax=jvc[4,4], PPFD=mean(sun$PARi))
  shaAT_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[1,3], Jmax=jvc[1,4], PPFD=mean(shade$PARi))
  shaET_sim <- Aci(Ci=seq(50,1000,length=101), Vcmax=jvc[2,3], Jmax=jvc[2,4],PPFD=mean(shade$PARi))
  
  
#   windows(10,8)
#   plot(sunAT_sim$Ci, sunAT_sim$ALEAF, col=suncol, pch=21,  cex=1.1,xlab=cilab, ylab="", type="l", lwd=2)
#   points(sunET_sim$Ci, sunET_sim$ALEAF, col=suncol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
#   points(shaAT_sim$Ci, shaAT_sim$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
#   points(shaET_sim$Ci, shaET_sim$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
#   
#   points(Photo~Ci, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
#   points(Photo~Ci, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25)
#   
#   legend("topleft", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)
#   legend("bottomright", leaflab2, pch=16,inset = 0.03, col=leafcol) 
#   title(ylab=satlab, mgp=ypos)
#   
#   dev.copy2pdf(file="master_scripts/figures/aci_predict.pdf")
#   dev.off()
  
  
  
####make png figure for talk that also plots points from licor------------------------------------------------  
  
  #read ACi datasets 
  acishade <- read.csv("raw data/shadeaci.csv")
  #clean from previous script
  acishade_clean <- acishade[acishade$chamber != "ch02",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch07",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch09",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch11",]

  shade_redo <- read.csv("raw data/shadeaci_redo.csv")
  
  sunaci <- read.csv("raw data/sunaci.csv")
  #clean from previous script
  sunaci_clean <- sunaci[sunaci$chamber != "ch06", ]
  sunaci_clean2 <- sunaci_clean[sunaci_clean$chamber != "ch04", ]
  
  tdlaci2 <- read.csv("raw data/tdlaci2.csv")

  library(scales)
  shacol50 <- alpha(shacol, alpha=.5)
  suncol50 <- alpha(suncol, alpha=.5)
  
  #simulate ACi curves at 1800 par
  sunAT_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[3,3], Jmax=jvc[3,4],PPFD=1800)
  sunET_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[4,3], Jmax=jvc[4,4], PPFD=1800)
  shaAT_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[1,3], Jmax=jvc[1,4], PPFD=1800)
  shaET_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[2,3], Jmax=jvc[2,4],PPFD=1800)

  
 ###png
  png(filename = "makepngs/aci_sunsha.png", width = 11, height = 8.5, units = "in", res= 400)
  
  par(mar=c(5,5,2,2), cex.lab=1.5)
  
  plot(Photo~Ci ,data= acishade_clean, pch=16, col=shacol50, ylim=c(0, 35), xlim=c(0,2000), xlab=cilab, 
       ylab=photolab, cex.axis=1.25)
  points(Photo~Ci ,data= shade_redo, pch=16, col=shacol50)
  points(Photo~Ci ,data= sunaci_clean2, pch=16, col=suncol50)
  points(Photo~Ci ,data= tdlaci2, pch=16, col=suncol50)

  points(sunAT_sim2$Ci, sunAT_sim2$ALEAF, col=suncol, pch=21,  cex=1.1,xlab=cilab, ylab="", type="l", lwd=3)
  points(sunET_sim2$Ci, sunET_sim2$ALEAF, col=suncol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=3, lty=2)
  points(shaAT_sim2$Ci, shaAT_sim2$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=3)
  points(shaET_sim2$Ci, shaET_sim2$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=3, lty=2)

  legend("bottomright", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03, bty='n',cex=1.25)

  dev.off()
  
  