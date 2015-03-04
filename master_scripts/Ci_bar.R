source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

###read data--------------------------------------------------------------------------------------------

#gas exchange data
photo <- read.csv("raw data/gm_licor_clean.csv")
#plot summary
treatments <- read.csv("raw data/temp_trt.csv")
#read in leaf data
leaf_chem <- read.csv("raw data/leaf_chem.csv")
leafN <- read.csv("calculated_data/leaf_chemistry.csv")


###format gas exchange data and get mean by id-----------------------------------------------------------

##remove shade high light treatment
photo$leaflight <- as.factor(paste(photo$leaf,photo$light, sep="-"))

ci <- photo[photo$leaflight == "sun-high" | photo$leaflight == "shade-low", c(2:5,7,12,14, 25,64) ]
  
#add months and treatments
  ci$campaign <- as.factor(ci$campaign)
  ci<- chlab_func(ci)
  ci<- add_Month(ci)
  ci<- addtrt_func(ci)
  ci<- droplevels(ci)

##make unique id for merge with delta data
  ci<- chooseidfunc(ci, c("campaign", "chamber",  "leaf"))

###this now has mean of values of sun-high and shade-low
ci_agg <- summaryBy(Ci+ CO2R ~ id+pair_id, data=ci, FUN=mean, keep.names=TRUE)


###format delta c12 data----------------------------------------------------------------------------------
deltaC <- leaf_chem[,4:8]
deltaC <- add_campaign(deltaC)
deltaC<- chooseidfunc(deltaC, c("campaign", "chamber",  "leaf"))


###calculate CI using discrimination equations (CI_bar), add treatments, and remove drought----------------

ci_bar <- merge(ci_agg, deltaC[,c(1,7)], all=TRUE)

ci_bar_calc <- function (x, a=4.4, b=29, c13_source=-8){
  x$D <- (c13_source-x$c13)/(1+(c13_source/1000))
  x$ci_bar <- (x$CO2R *((x$D-a) / (b-a)))
  x$leaf <- as.factor(substring(x$id, 8))
  x$campaign <- as.factor(substring(x$id, 1, 1))
  x$chamber <- as.factor(substring(x$id, 3, 6))
  return(x)
}

ci_bar2 <- ci_bar_calc(ci_bar)
  ##add month and treatments
  ci_bar2 <- add_Month(ci_bar2)
  ci_bar2 <- addtrt_func(ci_bar2)
  
##remove drought treatment
ci_bar3<- ci_bar2[ci_bar2$drydown != "drought",]

write.csv(ci_bar3, "calculated_data/Ci_bar.csv", row.names=FALSE)


###for now remove the pair comparison assocaited with missing gas exchance for 5-ch12-shade
ci_bar4 <- ci_bar3[complete.cases(ci_bar3),]
ci_bar4 <- ci_bar4[ci_bar4$pair_id != "f",]
templab <- c("AT", "ET")

###plot sun v shade
ci_shade <- [leafN$leaf == "shade",]
ci_sun<- leafN[leafN$leaf == "sun",]
cib_shade <- ci_bar4[ci_bar4$leaf == "shade",]
cib_sun<- ci_bar4[ci_bar4$leaf == "sun",]

cib_lm<- lm(cib_shade$ci_bar~ cib_sun$ci_bar)
summary(cib_lm)

####plotting----------------------------------------------------------------------------------

plot(cib_shade$ci_bar ~ cib_sun$ci_bar , pch=16, ylim=c(0,400), xlim=c(0,400))
abline(0,1, lty=1)






test <- dlply(ci_bar4, .(pair_id, chamber))
###calculate the difference Ci/Ci_bar between sun and shade leaves by pairwise comparisons-----------------------
ci_sp <- ddply(ci_bar4, .(pair_id, chamber), function(x) rbind.fill(
               data.frame(cibar_diff = x$ci_bar[1]- x$ci_bar[2], ci_diff= x$Ci[1]- x$Ci[2], temp=x$temp[1])))

####plotting of pairs
palette(c("blue", "red"))


windows(7,5)
plot(ci_diff~cibar_diff,data=ci_sp, col=temp, pch=16, cex=1.5, ylim=c(-200,200), xlim=c(-150, 150),
     xlab=cibarlab, ylab="")
title(ylab=cilab3, mgp=ypos)
abline(h=0, lty=2)
abline(v=0, lty=2)
legend("bottomright", templab, pch=16, col=palette(),pt.cex=1.5,inset = 0.03)  
dev.copy2pdf(file="master_scripts/figures/CI_sunshade.pdf")
dev.off()


#### treatment averages across season if Cibar and CI plotted against Narea

ci_cham<- summaryBy( Ci + ci_bar ~ chamber + leaf +temp, data=ci_bar4, FUN=mean, keep.names=TRUE)

leafN_nodrought <- leafN[leafN$drydown != "drought",]
N_cham <- summaryBy( leafN_area ~ chamber + leaf +temp, data=leafN_nodrought, FUN=mean, keep.names=TRUE)

canopy <- merge(ci_cham, N_cham)

##simple models
cibar_N<- lm(ci_bar~ leafN_area, data=canopy)
ci_N <- lm(Ci~ leafN_area, data=canopy )
summary(cibar_N)
summary(ci_N)




##
windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
plot(ci_bar ~leafN_area ,data=canopy, col=temp, pch=16, ylim=c(100, 350), xlim=c(1.25,4),subset=leaf=="sun", 
     ylab="", xlab= narealab, cex=1.25)
    points(ci_bar ~leafN_area ,data=canopy, col=temp, pch=1, subset=leaf=="shade", ylab="", xlab="", cex=1.25)
    points(Ci ~leafN_area ,data=canopy, col=temp, pch=17, cex=1.25, subset=leaf=="sun", ylab="", xlab= "")
    points(Ci ~leafN_area ,data=canopy, col=temp, pch=2,  cex=1.25, subset=leaf=="shade", ylab="", xlab="")

ablineclip(cibar_N, lty=1, x1=min(canopy$leafN_area), x2=max(canopy$leafN_area))
ablineclip(ci_N, lty=2, x1=min(canopy$leafN_area), x2=max(canopy$leafN_area))

title(ylab=cilab, mgp=ypos)

legend("bottomleft", leglab2 ,pch=pch4,   col=col4, pt.cex=.8,inset = 0.03, title= expression(bar(Ci)), bty="n")  
legend("topright", leglab2 ,pch=pchtri, col=col4, pt.cex=.8,inset = 0.03, title= "Ci", bty="n")  
dev.copy2pdf(file="master_scripts/figures/Ci_nitro.pdf")
dev.off()


