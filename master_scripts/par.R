source("functions and packages/functions.R")
source("functions and packages/packages.R")
source("master_scripts/plot_objects.R")

par <- read.csv("raw data/par.csv")
treatments <- read.csv("raw data/chamber_trt.csv")

#format function
par<- parformat(par)

par_leaf <- subset(par, ID !="shade-high")
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")

#data format for bar
parbar <- subset(par_leaf, select = c("par", "month", "leaf_type"))
parbar$month <- factor(parbar$month, levels = Morder)
levels(parbar$month)
parbar$leaf_type <- gsub("s", "S", parbar$leaf_type)


par_agg <- summaryBy(par ~ leaf_type, data=parbar, FUN=mean)


windows(7,5)
bar(par, c(leaf_type, month), parbar, col=c("yellowgreen", "green4"), xlab="", ylab="", ylim=c(0, 2000), 
      half.errbar=FALSE)
title(ylab=parlab, mgp=ypos)
dev.copy2pdf(file="master_scripts/figures/ppfd.pdf")
dev.off()


###for png
png(filename = "makepngs/ppfd.png", width = 11, height = 8.5, units = "in", res= 400)

bar(par, c(leaf_type, month), parbar, col=c("yellowgreen", "green4"), xlab="",  ylim=c(0, 2000), 
    half.errbar=FALSE, mar=c(5,7,2,2), ylab=parlab, cex.axis=1.75, cex.lab = 2, cex.names=2,
    legend = FALSE)
legend("topleft", c("Shade", "Sun"), pch=22,inset = 0.01, pt.bg=c("yellowgreen", "green4"),
       bty='n', cex=2)

dev.off()


 

