source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("master_scripts/plot_objects.R")

### investigate the relative contributions to the observed leaf delta c13 discrimination

discrim <- read.csv("calculated_data/discrim_wellwatered.csv")


##look at the mean discrmination parameters by treatment

##first mean of spot measurements
###get average by id
discrim_agg <- summaryBy(gm+ rm_resist+ DiminusDo + delta_i + delta_gm + delta_e + delta_f ~ chamber+id+leaf +light+temp+leaflight+Month, 
                         data=discrim, FUN=mean, keep.names=TRUE)

##then treatments + Month
discrim2 <- summaryBy(delta_i + delta_gm + delta_e + delta_f ~ temp+leaflight+Month, data=discrim_agg, 
                      FUN=mean, keep.names=TRUE)

##then treatments 
discrim3 <- summaryBy(delta_i + delta_gm + delta_e + delta_f ~ temp+leaflight, data=discrim_agg, 
FUN=mean, keep.names=TRUE)

##then leaf type only
discrim4 <- summaryBy(delta_i + delta_gm + delta_e + delta_f ~ leaflight, data=discrim_agg,  FUN=mean, keep.names=TRUE)



###to examine bad data plot gm resistance vs Di-D0 intstead of gm
  palette(c(lightscol, shacol, suncol))
 plot(rm_resist ~ DiminusDo, data=discrim_agg, xlim=c(0, 12), ylim=c(0, 25), pch=c(16, 17)[discrim_agg$temp],col=leaflight)
 plot(gm~DiminusDo, data=discrim_agg, xlim=c(0, 12), ylim=c(0, .5),pch=c(16, 17)[discrim_agg$temp],col=leaflight)
 
 
###discrimination factors---------------------------------------------------------------------------------------
 
###here the difference between di and do is driven mostly by gm (~75%), and then photorespiratio and respiration 
 
##need to reshape data to show this....maybe percentange of di-do????????????? 
 
bar(diff, c(leaf_type, Month), parbar, col=c(shacol,suncol), xlab="", ylab=parlab, ylim=c(0, 2000), 
     half.errbar=FALSE, mar=c(4,4,1,1),las=1,cex.axis=.8, cex.lab=1, cex.names=1,mgp=c(2.5,1,0)) 
 
 