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
  
  
  windows(10,8)
  plot(sunAT_sim$Ci, sunAT_sim$ALEAF, col=suncol, pch=21,  cex=1.1,xlab=cilab, ylab="", type="l", lwd=2)
  points(sunET_sim$Ci, sunET_sim$ALEAF, col=suncol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  points(shaAT_sim$Ci, shaAT_sim$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2)
  points(shaET_sim$Ci, shaET_sim$ALEAF, col=shacol, pch=21,  cex=1.1,xlab="Ci", ylab="", type="l", lwd=2, lty=2)
  
  points(Photo~Ci, data=gm_c13, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  points(Photo~Ci, data=gm_c13, subset=leaflight=="sun-high", pch=16, col=suncol, cex=1.25)
  
  legend("topleft", leglab2, lty=c(1,2,1,2), lwd=2,col=colaci, pt.bg=col4,inset = 0.03)
  legend("bottomright", leaflab2, pch=16,inset = 0.03, col=leafcol) 
  title(ylab=satlab, mgp=ypos)
  
  dev.copy2pdf(file="master_scripts/figures/aci_predict.pdf")
  dev.off()
  
  
  
  