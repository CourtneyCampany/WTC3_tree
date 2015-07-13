# source("functions and packages/packages.R")
# source("functions and packages/functions.R")
# source("master_scripts/plot_objects.R")

#read in parameters from aci curves with gm included
coefs <- read.csv("calculated_data/aci_gm_param.csv")

#####redo aci curves with gmes 
gm <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <-summaryBy(gm ~ leaflight+temp, data=gm, FUN=mean, keep.names=TRUE)


####model ACC with above parameters 

ci_seq <- seq(50, 1000, length=101)

acisunat<- Aci(ci_seq, Vcmax=coefs[3,3], Jmax=coefs[3,4], Rd=coefs[3,5], gmeso=gm_agg[5,3])
acisunet<- Aci(ci_seq, Vcmax=coefs[4,3], Jmax=coefs[4,4], Rd=coefs[4,5], gmeso=gm_agg[6,3])
acishaat<- Aci(ci_seq, Vcmax=coefs[1,3], Jmax=coefs[1,4], Rd=coefs[1,5], gmeso=gm_agg[3,3])
acishaet<- Aci(ci_seq, Vcmax=coefs[2,3], Jmax=coefs[2,4], Rd=coefs[2,5], gmeso=gm_agg[4,3])

acisunat$Cc <- with(acisunat, Ci - ALEAF / gm_agg[5,3])
acisunet$Cc <- with(acisunet, Ci - ALEAF / gm_agg[6,3])
acishaat$Cc <- with(acishaat, Ci - ALEAF / gm_agg[3,3])
acishaet$Cc <- with(acishaet, Ci - ALEAF / gm_agg[4,3])

###plot of model ACC curves
#windows(8,6)
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(ALEAF~Cc, data=acisunat, pch=16, col=suncol2, type='l',lwd=3,ylab=satlab, xlab=cclab)
points(ALEAF~Cc, data=acisunet, pch=17, col=suncol2,type='l',lwd=3, lty=2)
points(ALEAF~Cc, data=acishaat, pch=16, col=lightscol2,type='l',lwd=3)
points(ALEAF~Cc, data=acishaet, pch=16, col=lightscol2,type='l',lwd=3, lty=2)
legend("topleft", c("Sun-AT", "Sun-ET", "Shade-AT", "Shade-ET"), lty=c(1,2, 1,2),lwd=2,
       col=c(suncol2, suncol2,lightscol2,lightscol2),inset = 0.01, bty='n',cex=.8)

# dev.copy2pdf(file="master_scripts/paper_figures/Acc_model.pdf")
# dev.off()