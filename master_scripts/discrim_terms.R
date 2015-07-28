source("functions and packages/functions.R")
source("functions and packages/packages.R")

library(visreg)
library(multcomp)
library(nlme)

### investigate the relative contributions to the observed leaf delta c13 discrimination

discrim <- read.csv("calculated_data/discrim_wellwatered.csv")


##look at the mean discrmination parameters by treatment

##first mean of spot measurements
###get average by id
discrim_agg <- summaryBy(gm+ rm_resist+ DiminusDo + delta_i + delta_gm + delta_e + delta_f ~ chamber+id+leaf +light+temp+leaflight+Month, 
                         data=discrim, FUN=mean, keep.names=TRUE)
discrim_agg$gm_perc <- with(discrim_agg, delta_gm/(delta_gm+delta_e+delta_f))
discrim_agg$tukeyid <- as.factor(paste(discrim_agg$leaf, discrim_agg$temp, sep="-"))

##then treatments + Month
discrim2 <- summaryBy(delta_i + delta_gm + delta_e + delta_f+gm_perc ~ temp+leaflight+Month, data=discrim_agg, 
                      FUN=mean, keep.names=TRUE)

##then treatments 
discrim3 <- summaryBy(gm_perc ~ temp+leaflight, data=discrim_agg, FUN=c(mean,se))

##then leaf type only
discrim4 <- summaryBy(gm_perc ~ leaflight, data=discrim_agg,  FUN=c(mean,se))

gmcontr <- with(discrim_agg, mean(gm_perc))
gm_se <- with(discrim_agg, se(gm_perc))



gmperc_mod <- lme(gm_perc ~ tukeyid ,random=~1|chamber, data=discrim_agg)
summary(gmperc_mod)
anova(gmperc_mod)
visreg(gmperc_mod)
confint(gmperc_mod)

mod <- glm(gm_perc ~ tukeyid, family="binomial", data=discrim_agg)


###to examine bad data plot gm resistance vs Di-D0 intstead of gm
  palette(c(lightscol, shacol, suncol))
 plot(rm_resist ~ DiminusDo, data=discrim_agg, xlim=c(0, 12), ylim=c(0, 25), pch=c(16, 17)[discrim_agg$temp],col=leaflight)
 plot(gm~DiminusDo, data=discrim_agg, xlim=c(0, 12), ylim=c(0, .5),pch=c(16, 17)[discrim_agg$temp],col=leaflight)
 
 
###discrimination factors---------------------------------------------------------------------------------------
 
###here the difference between di and do is driven mostly by gm (~75%), and then photorespiratio and respiration 
 
##need to reshape data to show this....maybe percentange of di-do????????????? 
 
bar(diff, c(leaf_type, Month), parbar, col=c(shacol,suncol), xlab="", ylab=parlab, ylim=c(0, 2000), 
     half.errbar=FALSE, mar=c(4,4,1,1),las=1,cex.axis=.8, cex.lab=1, cex.names=1,mgp=c(2.5,1,0)) 
 
 