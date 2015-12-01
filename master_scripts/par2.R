# source("functions and packages/functions.R")
# source("functions and packages/packages.R")
# source("master_scripts/plot_objects.R")
treatments <- read.csv("raw data/temp_trt.csv")

###need campaign dates in order to remake PAR graph with temp

octdate <- as.Date("2013-10-25")
decdate <- as.Date("2013-12-10")
jandate <- as.Date("2014-01-28")
febdate <- as.Date("2014-02-24")
mardate <- as.Date("2014-03-19")
aprdate <- as.Date("2014-04-21")

campaigns <- c(octdate, decdate, jandate, febdate, mardate, aprdate)
campaigns2 <- data.frame(Month = c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr"), Date = campaigns)

###add temp to par figure so read met data

met <- read.csv("calculated_data/met_chamber.csv")
  met$Date <- as.Date(met$Date)
  met$chamber <- gsub("C", "ch", met$chamber)

metcampaigns <- met[met$Date %in% campaigns,]
  ###i want ambient air temp so need to exclude ET treatments
  metcampaigns <- merge(metcampaigns, treatments)

met_amb <- metcampaigns[metcampaigns$temp == "ambient",]

met_amb_agg <- summaryBy(Tair_al.mean + Tair_al.max ~ Date, data= met_amb, FUN=c(mean, se))


####read PAR data and remake figure----------------------------------------------------------------------------
par <- read.csv("raw data/par.csv")

#format function
par<- parformat(par)

par_leaf <- subset(par, ID !="shade-high")
par_leaf2 <- par_leaf[par_leaf$drydown == "control",]
###use actual date instead of month abbr
par_leaf3 <- merge(par_leaf2, campaigns2)

#data format for bar
parbar <- par_leaf3[ , c(3,5,9)]
parbar$leaf_type <- gsub("s", "S", parbar$leaf_type)

##Create a two row matrix with sun and shade PAR
par_agg <- summaryBy(par ~ leaf_type+Date, data=parbar, FUN=c(mean,se))
sunpar <- par_agg[par_agg$leaf_type == "Sun",]
shapar <- par_agg[par_agg$leaf_type == "Shade",]

barleg <- c("Shade", "Sun", "Temperature")

par_dat <- rbind(shapar$par.mean,sunpar$par.mean)

mean_dat <- c(shapar$par.mean,sunpar$par.mean)
se_dat <- c(shapar$par.se,sunpar$par.se)
#reorder for sha-sun
mean_dat2 <- mean_dat[c(1,7,2,8,3,9,4,10,5,11,6,12)]
se_dat2 <- se_dat[c(1,7,2,8,3,9,4,10,5,11,6,12)]

#windows(7,7)
par(mar=c(5,6,1,5), mgp=c(2.5,1,0), las=1,cex.axis=.8, cex.lab=1)
PAR_bar <- barplot(par_dat, beside=TRUE, names.arg=par_agg$leaf_type, ylim=c(0, 2000), col=c(shacol,suncol), 
                     xaxt='n', ylab=parlab)
arrows(PAR_bar, mean_dat2, PAR_bar, mean_dat2+se_dat2, length=0.1, angle=90)
arrows(PAR_bar, mean_dat2, PAR_bar, mean_dat2-se_dat2, length=0.1, angle=90)
box()
legend("topright",barleg, pch = c(22,22,21), bty='n', pt.bg=c(shacol,suncol,"black"), cex=1, pt.cex=1.25,inset=.01)

#axis(side=1, at=c(2.5, 6.5, 10.5, 14.5), labels=boxlab, padj=.75, cex.axis=1.25)
text(y=-135, x=c(1.5 , 4.5 , 7.5, 10.5 ,13.5 ,16.5), labels = campaigns, srt=45, xpd=TRUE)

par(new=T)
plot(Tair_al.max.mean ~ Date, data=met_amb_agg,ylim=c(0,34), axes=F,xlab="", ylab="",type="b", cex=1.25, pch=16, lwd=1) 
# with(met_amb_agg, arrows(Date, Tair_al.max.mean, Date, Tair_al.max.mean+Tair_al.max.se, angle=90, length=0.03, cex=1))
# with(met_amb_agg, arrows(Date, Tair_al.max.mean, Date, Tair_al.max.mean-Tair_al.max.se, angle=90, length=0.03, cex=1))

axis(4, labels=TRUE) 
mtexti(airtlab, 4, outer=TRUE, cex=1, off=.5)
     
# dev.copy2pdf(file="master_scripts/paper_figures/ppfd2.pdf")
# dev.off()
