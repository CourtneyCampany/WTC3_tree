# source("functions and packages/functions.R")
# source("functions and packages/packages.R")
# source("master_scripts/plot_objects.R")

 par <- read.csv("raw data/par.csv")
 
 treatments <- read.csv("raw data/temp_trt.csv")

#format function
par<- parformat(par)

par_leaf <- subset(par, ID !="shade-high")
par_leaf2 <- par_leaf[par_leaf$drydown == "control",]
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")

#data format for bar
parbar <- par_leaf2[ , c(2:3,5)]
parbar$Month <- factor(parbar$Month, levels = Morder)
levels(parbar$Month)
parbar$leaf_type <- gsub("s", "S", parbar$leaf_type)

par_agg <- summaryBy(par ~ leaf_type, data=parbar, FUN=mean)

###Stats---------------------------------------------------------------------------------------------------------
# library(visreg)
# library(multcomp)
# library(nlme)
# 
# #examite data with boxplots, then remove any outliers
# boxplot(par~temp, data=par_leaf2[par_leaf2$leaf_type =="sun",])
# boxplot(par~temp, data=par_leaf2[par_leaf2$leaf_type =="shade",])
# 
# 
# ##PAR significantly different between leaves not by temperature treatment
# par_mod <- lme(par ~ leaf_type, random=~1|chamber, data=par_leaf2)
# summary(par_mod)
# anova(par_mod)
# 
# par_mod2 <- lme(par ~ leaf_type*temp, random=~1|chamber, data=par_leaf2)
# anova(par_mod2)
# summary(par_mod2)

###PLOTTING------------------------------------------------------------------------------------------------
 #windows(6,6)

bar(par, c(leaf_type, Month), parbar, col=c(shacol,suncol), xlab="", ylab=parlab, ylim=c(0, 2000), 
      half.errbar=FALSE, mar=c(4,4,1,1),las=1,cex.axis=.8, cex.lab=1, cex.names=1,mgp=c(2.5,1,0))
#    dev.copy2pdf(file="master_scripts/paper_figures/ppfd.pdf")
#    dev.off()


###for png
# png(filename = "makepngs/ppfd.png", width = 11, height = 8.5, units = "in", res= 400)
# bar(par, c(leaf_type, Month), parbar, col=c(shacol,suncol), xlab="", ylab=parlab, ylim=c(0, 2000), 
#     half.errbar=FALSE, mar=c(5,6,1,1),las=1,cex.axis=1.5, cex.lab=2, cex.names=2,mgp=c(3.5,1,0))
# dev.off()


 

