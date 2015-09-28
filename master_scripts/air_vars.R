 # source("functions and packages/packages.R")
 # source("functions and packages/functions.R")
 # source("master_scripts/plot_objects.R")


 treatments <- read.csv("raw data/temp_trt.csv")
 #read in calculated met data by chamber
 
 PPFD_outside<- read.csv("calculated_data/PPFD_outside.csv")
 PPFD_outside$Date <- as.Date(PPFD_outside$Date)
  
 PPFD_chamber<- read.csv("calculated_data/PPFD_chamber.csv")
  PPFD_chamber$chamber <- gsub("C", "ch", PPFD_chamber$chamber)
  PPFD_chamber <- merge(PPFD_chamber, treatments)
  PPFD_chamber$chamber <- as.factor(PPFD_chamber$chamber)
  PPFD_chamber$Date <- as.Date(PPFD_chamber$Date)
 
 met_chamber<- read.csv("calculated_data/met_chamber.csv")
   met_chamber$chamber <- gsub("C", "ch", met_chamber$chamber)
  met_chamber <- merge(met_chamber, treatments)
  met_chamber$Date <- as.Date(met_chamber$Date)

PPFD_chamber_clean <- PPFD_chamber[PPFD_chamber$chamber!="ch03",]

PPFD_agg <- summaryBy(PPFD_day~temp+Date, data=PPFD_chamber_clean, FUN=mean, keep.names = TRUE)   
met_cham_agg <- summaryBy(VPD.max+Tair_al.min+Tair_al.max ~ Date+temp, data=met_chamber, FUN=mean, keep.names = TRUE)

##3 PANEl plots with PPFD, TEMP, VPD----------------------------------------------------------------

startday <- as.Date(strptime("10-01-2013", format = "%m-%d-%Y", tz=""))
xAT <- seq.Date(startday, by="month", length=8,format = "%m-%d-%Y")
tminlab <- expression(T[min])
tmaxlab <- expression(T[max])
templab <- expression(Temperature~~(degree*C))
vpdmax <- expression(VPD[max]~~(kPa))

xlim1 <- as.Date(strptime("10-01-2013", format = "%m-%d-%Y", tz=""))
xlim2 <- as.Date(strptime("05-31-2013", format = "%m-%d-%Y", tz=""))
xlimdays <- c(xlim1, xlim2)
dayparlab <- expression(PPFD[day]~~(mols~m^-2~d^-1))


# windows(7,10)

par(mfrow=c(3,1), las=1, cex.axis=1.21, cex.lab=1.51, mgp=c(2.5,1,0),oma=c(4, 0, 1,0))

#1: PPFD PLOTs
par(mar=c(0,5,0,1))
plot(PPFD_day ~ Date, data=PPFD_outside, type='l',col="blue",lwd=2, lty=1,  xlab="", ylab=dayparlab, axes=FALSE,
     ylim=c(0,65))

axis(2)
axis.Date(1, at=xAT, labels=FALSE)
box()
text(x=15979, y=64, "(a)",cex=1.51)

#2. VPD
par(mar=c(0,5,0,1))
plot(VPD.max ~ Date, data=met_cham_agg[met_cham_agg$temp=="ambient",], type='l',col="black",lwd=2, ylim=c(0,8.5),
     xlab="", ylab=vpdmax, axes=FALSE)
points(VPD.max ~ Date, data=met_cham_agg[met_cham_agg$temp=="elevated",], type='l',col="red",lwd=2)
axis(2)
axis.Date(1, at=xAT, labels=FALSE)
box()
legend("topright",col=c("black","red"),lty=c(1,2),legend=c( "AT", "ET"),inset=.01,  bty='n',cex=1.51)
text(x=15979, y=8.25, "(b)", cex=1.51)

#3:temp plot

# png(filename = "makepngs/airvars.png", width = 11, height = 8.5, units = "in", res= 400)

par(mar=c(0,5,0,1))
plot(Tair_al.max ~ Date, data=met_cham_agg[met_cham_agg$temp=="ambient",], type='l',col="black",lwd=2, ylim=c(0, 50),
     xlab="", ylab=templab, axes=FALSE)
  points(Tair_al.max ~ Date, data=met_cham_agg[met_cham_agg$temp=="elevated",], type='l',col="red",lwd=2)
  points(Tair_al.min ~ Date, data=met_cham_agg[met_cham_agg$temp=="ambient",], type='l',col="black",lwd=2, lty=2)
  points(Tair_al.min ~ Date, data=met_cham_agg[met_cham_agg$temp=="elevated",], type='l',col="red",lwd=2, lty=2)

legend("topright",col=c("black", "black"),lty=c(1,2),legend=c(tmaxlab,tminlab),inset=.01, cex=1.51, bty='n')
  
axis(2)
axis.Date(1, at=xAT, labels=TRUE, outer=TRUE)
box()
text(x=15979, y=48, "(c)", cex=1.51)


# dev.copy2pdf(file="master_scripts/paper_figures/airvars.pdf")
# dev.off()



