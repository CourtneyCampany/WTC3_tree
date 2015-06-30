#simulated aci and acc curves with chosen ci point

source("functions and packages/packages.R")
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
gm_agg <- summaryBy(Photo+Cond+Ci+Cc+PARi ~ id+leaf +light+temp+leaflight, 
                    data=gmes, FUN=mean, keep.names=TRUE)

shade <- gm_agg[gm_agg$leaflight=="shade-low",]
sun <- gm_agg[gm_agg$leaflight=="sun-high",]
fleck <- gm_agg[gm_agg$leaflight=="shade-high",]




#read in jmax vcmax
jvc<- read.csv("calculated_data/aci_sunsha.csv")


####use plant ecophys to predict aci curve--------------------------------------------------------------------------

#simulate ACi curves for each leaf, sun and shade at saturating light then simulated shade
sun_sim <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[2,2], Jmax=jvc[2,3],PPFD=1800)

fleck_sim <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[1,2], Jmax=jvc[1,3], PPFD=1800)

sha_sim <- Aci(Ci=seq(50,2000,length=101), Vcmax=jvc[1,2], Jmax=jvc[1,3], PPFD=mean(shade$PARi))

#simulate ci at 300 for different light scenarios
sun300 <- Photosyn( Vcmax=jvc[2,2], Jmax=jvc[2,3],PPFD=1800)
fleck300 <- Photosyn( Vcmax=jvc[1,2], Jmax=jvc[1,3], PPFD=1800)
sha300 <- Photosyn( Vcmax=jvc[1,2], Jmax=jvc[1,3], PPFD=mean(shade$PARi))



###testing aci fit to data---------------------------------------------------------------------------------------
par(mar=c(4,4,1,1), mgp=c(2.5, 1, 0))
plot(Photo~Ci, data=gm_agg, type='n', xlim=c(0, 400), ylim=c(0, 30),xlab=cilab, ylab=satlab)
#plot aci curves
points(sun_sim$Ci, sun_sim$ALEAF, col=suncol2, type="l", lwd=2)
points(fleck_sim$Ci, fleck_sim$ALEAF, col=lightscol2,  type="l", lwd=2)
points(sha_sim$Ci, sha_sim$ALEAF, col=shacol2,  type="l", lwd=2)
#plot ci point = 300
points(ALEAF~Ci, data=sun300, pch=16, col=suncol2, cex=1.5)
points(ALEAF~Ci, data=fleck300, pch=16, col=lightscol2, cex=1.5)
points(ALEAF~Ci, data=sha300, pch=16, col=shacol2, cex=1.5)

legend("topleft", c("Sun", "Shade", "Shade-low"), pch=c(16,16,16), lty=c(1,1,1),  
       col=c(suncol2, lightscol2, shacol2), inset = 0.01, bty="n")


