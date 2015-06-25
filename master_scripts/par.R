# source("functions and packages/functions.R")
# source("functions and packages/packages.R")
# source("master_scripts/plot_objects.R")
# 
# par <- read.csv("raw data/par.csv")
# 
# treatments <- read.csv("raw data/temp_trt.csv")

#format function
par<- parformat(par)

par_leaf <- subset(par, ID !="shade-high")
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")

#data format for bar
parbar <- par_leaf[ , c(2:3,5)]
parbar$month <- factor(parbar$month, levels = Morder)
levels(parbar$month)
parbar$leaf_type <- gsub("s", "S", parbar$leaf_type)

par_agg <- summaryBy(par ~ leaf_type, data=parbar, FUN=mean)

###Stats---------------------------------------------------------------------------------------------------------
# library(visreg)
# library(multcomp)
# library(nlme)

#examite data with boxplots, then remove any outliers
# boxplot(par~temp, data=par_leaf[par_leaf$leaf_type =="sun",])
# boxplot(par~temp, data=par_leaf[par_leaf$leaf_type =="shade",])


###lma not different between leaf types or temp treatment.
# par_leaf <- lme(par ~ leaf_type, random=~1|chamber, data=par_leaf)
# summary(par_leaf)
# anova(par_leaf)


###PLOTTING------------------------------------------------------------------------------------------------

#windows(7,5)
bar(par, c(leaf_type, month), parbar, col=c(shacol,suncol), xlab="", ylab=parlab, ylim=c(0, 2000), 
      half.errbar=FALSE, mar=c(5,5,2,2))
#title(ylab=parlab, mgp=ypos)
# dev.copy2pdf(file="master_scripts/figures/ppfd.pdf")
# dev.off()


###for png
# png(filename = "markdown/ppfd.png", width = 11, height = 8.5, units = "in", res= 400)
# 
# bar(par, c(leaf_type, month), parbar, col=c("yellowgreen", "green4"), xlab="",  ylim=c(0, 2000), 
#     half.errbar=FALSE, mar=c(5,7,2,2), ylab=parlab, cex.axis=1.75, cex.lab = 2, cex.names=2,
#     legend = FALSE)
# legend("topleft", c("Shade", "Sun"), pch=22,inset = 0.01, pt.bg=c("yellowgreen", "green4"),
#        bty='n', cex=2)
# 
# dev.off()


 

