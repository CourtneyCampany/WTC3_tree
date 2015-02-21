source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

###read and format data-----------------------------------------------------------------------------
#plot summary
treatments <- read.csv("raw data/temp_trt.csv")

#par 
leaf_par <- read.csv("raw data/par.csv")
  leaf_par <- parformat(leaf_par)
  leaf_par$chamber <- as.factor(leaf_par$chamber)
  names(leaf_par)[c(2:4, 7)] <- c("Month","leaf", "light", "leaflight")
  #no shade high and no drought
  leaf_par2 <- leaf_par[leaf_par$leaflight != "shade-high",]
  leaf_par3 <- leaf_par2[leaf_par2$drydown != "drought",]

#nitro
leafN <- read.csv("calculated_data/leaf_chemistry.csv")
  #no drought
  leafN2 <- leafN[leafN$drydown != "drought",]

#leafK
leafk <- read.csv("calculated_data/leafK_nodrought.csv")


####merge leaf N with PAR---------------------------------------------------------------------------------


par_nitro <- merge(leaf_par3, leafN2[,c(1:3, 8:9, 12:13)])

test <- dlply(par_nitro, .(chamber, Month))

leaf_sp <- ddply(par_nitro, .(chamber, Month), function(x) rbind.fill(
  data.frame(par_diff = x$par[1]/x$par[2], n_diff= x$leafN_area[1]/x$leafN_area[2], temp=x$temp[1])))


####merge LeafK with Par
par_K<- merge(leaf_par3, leafk[,c(1:3, 7:8, 12)])
leaf_sp2 <- ddply(par_K, .(chamber, Month), function(x) rbind.fill(
  data.frame(par_diff = x$par[1]/x$par[2], k_diff= x$leafK[1]/x$leafK[2], temp=x$temp[1])))


##plot didtribution of N and Leaf K as a function of relative PPFD on shade and sun leaves

##nitro
plot(n_diff~par_diff,data=leaf_sp, col=cols, pch=16, cex=1.5, ylim=c(0,1.2), xlim=c(0, 1.2),
     xlab=relparlab, ylab="")
title(ylab=relnitrolab, mgp=ypos)
abline(0,1, lty=2)

legend("bottomright", templab, pch=16, col=cols,pt.cex=1.5,inset = 0.03)  

##leafK
plot(k_diff~par_diff,data=leaf_sp2, col=cols, pch=16, cex=1.5, ylim=c(0,4), xlim=c(0, 4),
     xlab=relparlab, ylab="")
title(ylab=relklab, mgp=ypos)
abline(0,1, lty=2)

legend("bottomright", templab, pch=16, col=cols, pt.cex=1.5, inset = 0.03)
