source("functions and packages/packages.R")
source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")

#read in parameters from aci curves with gm included
coefs <- read.csv("calculated_data/aci_gm_param.csv")

#####redo aci curves with gmes 
gm <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <-summaryBy(gm ~ leaflight+temp, data=gm, FUN=mean, keep.names=TRUE)

test <- gm[gm$leaf=="sun" & gm$temp=="ambient",]
####model ACC with above parameters 

ci_seq <- seq(50, 2000, length=101)

acisunat<- Aci(ci_seq, Vcmax=coefs[3,3], Jmax=coefs[3,4], Rd=coefs[3,5], gmeso=gm_agg[5,3])
acisunet<- Aci(ci_seq, Vcmax=coefs[4,3], Jmax=coefs[4,4], Rd=coefs[4,5], gmeso=gm_agg[6,3])
acishaat<- Aci(ci_seq, Vcmax=coefs[1,3], Jmax=coefs[1,4], Rd=coefs[1,5], gmeso=gm_agg[1,3])
acishaet<- Aci(ci_seq, Vcmax=coefs[2,3], Jmax=coefs[2,4], Rd=coefs[2,5], gmeso=gm_agg[2,3])

acisunat$Cc <- with(acisunat, Ci - ALEAF / gm_agg[5,3])
acisunet$Cc <- with(acisunet, Ci - ALEAF / gm_agg[6,3])
acishaat$Cc <- with(acishaat, Ci - ALEAF / gm_agg[1,3])
acishaet$Cc <- with(acishaet, Ci - ALEAF / gm_agg[2,3])

###get Cc points for figure, must use ACi curve data---------------------------------------------------------------------------
treatments <- read.csv("raw data/temp_trt.csv") 
#read ACi datasets (there are several that must be cleaned)


acishade <- read.csv("raw data/shadeaci.csv")
  acishade <- merge(acishade, treatments)
#clean from previous script
acishade_clean <- acishade[!(acishade$chamber %in% c("ch02","ch07","ch09","ch11")),]
 ##calculate Cc using appropriate gmes
  acishade_clean$gm <- ifelse(acishade_clean$temp=="ambient", gm_agg[1,3],gm_agg[2,3])
  acishade_clean$Cc <- with(acishade_clean, Ci-Photo/gm)

  
shade_redo <- read.csv("raw data/shadeaci_redo.csv")
  shade_redo <- merge(shade_redo, treatments)
  ##calculate Cc using appropriate gmes
  shade_redo$gm_bar <- ifelse(shade_redo$temp=="ambient", gm_agg[1,3],gm_agg[2,3])
  shade_redo$Cc <- with(shade_redo, Ci-Photo/gm_bar)
  ##calculate Cc using appropriate gmes
  shade_redo$gm_bar <- ifelse(shade_redo$temp=="ambient", gm_agg[1,3],gm_agg[2,3])
  shade_redo$Cc <- with(shade_redo, Ci-Photo/gm_bar)


sunaci <- read.csv("raw data/sunaci.csv")
  sunaci <-merge(sunaci, treatments)

sunaci_clean <- sunaci[sunaci$chamber != "ch04", ]
  ##calculate Cc using appropriate gmes
  sunaci_clean$gm_bar <- ifelse(sunaci_clean$temp=="ambient", gm_agg[5,3],gm_agg[6,3])
  sunaci_clean$Cc <- with(sunaci_clean, Ci-Photo/gm_bar)


tdlaci2 <- read.csv("raw data/tdlaci2.csv")
  tdlaci2 <-merge(tdlaci2, treatments)
  ##calculate Cc using appropriate gmes
  tdlaci2$gm_bar <- ifelse(tdlaci2$temp=="ambient", gm_agg[5,3],gm_agg[6,3])
  tdlaci2$Cc <- with(tdlaci2, Ci-Photo/gm_bar)



###plot of model ACC curves
 windows(8,6)

par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(ALEAF~Cc, data=acisunat, pch=16, col=suncol2, type='l',lwd=3,ylab=photolab, xlab=cclab,ylim=c(0, 35), xlim=c(0,2000))
  points(ALEAF~Cc, data=acisunet,  col=suncol2,type='l',lwd=3, lty=2)
  points(ALEAF~Cc, data=acishaat,  col=lightscol2,type='l',lwd=3)
  points(ALEAF~Cc, data=acishaet,  col=lightscol2,type='l',lwd=3, lty=2)
  legend("bottomright", c("Sun-AT", "Sun-ET", "Shade-AT", "Shade-ET"), lty=c(1,2, 1,2),lwd=2,
          col=c(suncol2, suncol2,lightscol2,lightscol2),inset = 0.01, bty='n',cex=.8)
  
###Add points from ACi curves, where Cc is predicted
  points(Photo~Cc ,data= acishade_clean, col=lights50col, pch=c(16, 17)[pch=acishade_clean$temp])
  points(Photo~Cc ,data= shade_redo, col=lights50col,  pch=c(16, 17)[pch=shade_redo$temp])
  
  points(Photo~Cc ,data= sunaci_clean,  col=suncol50,  pch=c(16, 17)[pch=sunaci_clean$temp])
  points(Photo~Cc ,data= tdlaci2,  col=suncol50,  pch=c(16, 17)[pch=tdlaci2$temp])


###inset as in aci
# par(fig=c(0.45, 0.95, 0.15,0.55), mar=c(2,2,0,0),new=T, cex=.7, las=1,  cex.axis=.7, cex.lab=.7, tcl=-.25)
# 
# plot(ALEAF~Cc ,data= acisunat, ylim=c(0, 15.5), xlim=c(45,250), xlab="", ylab="",xaxt="n", yaxt="n", pch="")
# axis(2, mgp=c(3, .5, 0))
# axis(1, mgp=c(3, 0, 0))
# 
#   points(ALEAF~Cc, col=suncol2, data=acisunat, type="l", lwd=2)
#   points(ALEAF~Cc, col=suncol2, data=acisunet, type="l", lwd=2, lty=2)
#   points(ALEAF~Cc, col=lightscol2, data=acishaat, type="l", lwd=2)
#   points(ALEAF~Cc, col=lightscol2, data=acishaet, type="l", lwd=2, lty=2)  


dev.copy2pdf(file="master_scripts/paper_figures/Acc_test.pdf")
dev.off()