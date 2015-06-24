# source("functions and packages/packages.R")
# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")
# 
# treatments <- read.csv("raw data/temp_trt.csv") 

###make png figure for talk that also plots points from licor------------------------------------------------  
  
  #read ACi datasets 
  acishade <- read.csv("raw data/shadeaci.csv")
  acishade <- merge(acishade, treatments)
  
  #clean from previous script
  acishade_clean <- acishade[acishade$chamber != "ch02",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch07",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch09",]
  acishade_clean <- acishade_clean[acishade_clean$chamber != "ch11",]

  shade_redo <- read.csv("raw data/shadeaci_redo.csv")
  shade_redo <- merge(shade_redo, treatments)
  
  sunaci <- read.csv("raw data/sunaci.csv")
  sunaci <-merge(sunaci, treatments)
    
  #clean from previous script
  sunaci_clean <- sunaci[sunaci$chamber != "ch06", ]
  sunaci_clean2 <- sunaci_clean[sunaci_clean$chamber != "ch04", ]
  
  tdlaci2 <- read.csv("raw data/tdlaci2.csv")
  tdlaci2 <-merge(tdlaci2, treatments)

  ##colors 
  shacol50 <- alpha(shacol, alpha=.5)
  suncol50 <- alpha(suncol, alpha=.5)
  
  #read in jmax vcmax
  jvc<- read.csv("calculated_data/tdl_aci.csv")
  
  #simulate ACi curves at 1800 par
  sunAT_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[3,3], Jmax=jvc[3,4],PPFD=1800)
  sunET_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[4,3], Jmax=jvc[4,4], PPFD=1800)
  shaAT_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[1,3], Jmax=jvc[1,4], PPFD=1800)
  shaET_sim2 <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[2,3], Jmax=jvc[2,4],PPFD=1800)

  
 ###png
  #png(filename = "markdown/aci_curves.png", width = 11, height = 8.5, units = "in", res= 400)
  
  #windows(11,8.5)
  par(mar=c(5,7,2,2), cex.lab=2, las=1)
  
  plot(Photo~Ci ,data= acishade_clean, col=shacol50, ylim=c(0, 39), xlim=c(0,2000), xlab="", 
       ylab=photolab, cex.axis=1.75, cex=1.5, pch=c(16, 17)[pch=acishade_clean$temp])
  points(Photo~Ci ,data= shade_redo, col=shacol50, cex=1.5, pch=c(16, 17)[pch=shade_redo$temp])
  points(Photo~Ci ,data= sunaci_clean2,  col=suncol50, cex=1.5, pch=c(16, 17)[pch=sunaci_clean2$temp])
  points(Photo~Ci ,data= tdlaci2,  col=suncol50, cex=1.5, pch=c(16, 17)[pch=tdlaci2$temp])
  title(xlab=cilab, mgp=c(3.5,1,1))

  points(sunAT_sim2$Ci, sunAT_sim2$ALEAF, col=suncol, cex=1.1,xlab=cilab, ylab="", type="l", lwd=3.5)
  points(sunET_sim2$Ci, sunET_sim2$ALEAF, col=suncol, cex=1.1,xlab="", ylab="", type="l", lwd=3.5, lty=2)
  points(shaAT_sim2$Ci, shaAT_sim2$ALEAF, col=shacol, cex=1.1,xlab="", ylab="", type="l", lwd=3.5)
  points(shaET_sim2$Ci, shaET_sim2$ALEAF, col=shacol, cex=1.1,xlab="", ylab="", type="l", lwd=3.5, lty=2)
  
  legend("topleft", leglab2, pch=c(16,17,16,17), col=colaci,inset = 0.01, bty='n',cex=1.25)
  legend(x=160, y=40, legend=c("", "" ), lty=c(1,2), lwd=2, col=suncol, bty='n',cex=1.25)
  legend(x=210, y=37, legend=c("", "" ), lty=c(1,2), lwd=2, col=shacol, bty='n',cex=1.25)
  
###inset figure  

  par(fig=c(0.45, 0.975, 0.05,0.5), new=T, cex=1, las=1, cex.axis=1.25)
    
  plot(Photo~Ci ,data= acishade_clean, ylim=c(0, 15.5), xlim=c(45,300), xlab="", ylab="",xaxt="n", yaxt="n", pch="")
  axis(2, mgp=c(3, .5, 0))
  axis(1, mgp=c(3, .5, 0))

  points(Photo~Ci ,data= acishade_clean, col=shacol50,pch=c(16, 17)[pch=acishade_clean$temp])
  points(Photo~Ci ,data= shade_redo,col=shacol50, pch=c(16, 17)[pch=shade_redo$temp])
  points(Photo~Ci ,data= sunaci_clean2, col=suncol50, pch=c(16, 17)[pch=sunaci_clean2$temp])
  points(Photo~Ci ,data= tdlaci2, col=suncol50, pch=c(16, 17)[pch=tdlaci2$temp])
  
  points(sunAT_sim2$Ci, sunAT_sim2$ALEAF, col=suncol, cex=1,xlab=cilab, ylab="", type="l", lwd=2.5)
  points(sunET_sim2$Ci, sunET_sim2$ALEAF, col=suncol, cex=1,xlab="Ci", ylab="", type="l", lwd=2.5, lty=2)
  points(shaAT_sim2$Ci, shaAT_sim2$ALEAF, col=shacol, cex=1,xlab="Ci", ylab="", type="l", lwd=2.5)
  points(shaET_sim2$Ci, shaET_sim2$ALEAF, col=shacol, cex=1,xlab="Ci", ylab="", type="l", lwd=2.5, lty=2)
  
  #dev.copy2pdf(file="master_scripts/paper_figures/aci_curves.pdf")
  #dev.off()
  
  
#   subplot(plot(Photo~Ci ,data= acishade_clean, pch=16, col=shacol50, ylim=c(0, 15), xlim=c(0,300), xlab="", 
#                ylab="", cex.axis=1, cex=1), x=1500, y=8, size=c(4.5,2.5))
#   
  #Return par(fig=) to full size for next plot
  #par(fig=c(0,1,0,1))
  
  
  
  