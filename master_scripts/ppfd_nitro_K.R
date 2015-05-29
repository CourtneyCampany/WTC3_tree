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
leafk$leaf <- gsub("s", "S", leafk$leaf)


####merge leaf N with PAR---------------------------------------------------------------------------------
par_nitro <- merge(leaf_par3, leafN2[,c(1:3, 8:9, 12:13)])

test <- dlply(par_nitro, .(chamber, Month))

leaf_sp <- ddply(par_nitro, .(chamber, Month), function(x) rbind.fill(
  data.frame(par_diff = x$par[1]/x$par[2], n_diff= x$leafN_area[1]/x$leafN_area[2], temp=x$temp[1])))


####merge LeafK with Par
par_K<- merge(leaf_par3, leafk[,c(1:3, 7:8, 12)])
leaf_sp2 <- ddply(par_K, .(chamber, Month), function(x) rbind.fill(
  data.frame(par_diff = x$par[1]/x$par[2], k_diff= x$leafK[1]/x$leafK[2], temp=x$temp[1])))

####merge LeafK with nitro
nitro_K<- merge(leafN2, leafk[,c(1:3, 7:8, 12)])
leaf_sp3 <- ddply(nitro_K, .(chamber, Month), function(x) rbind.fill(
  data.frame(n_diff= x$leafN_area[1]/x$leafN_area[2], k_diff= x$leafK[1]/x$leafK[2], temp=x$temp[1])))

####means testing

Nshade <- leafN[leafN$leaf == "shade",]
Nsun<- leafN[leafN$leaf == "sun",]
Kshade <- leafk[leafk$leaf == "shade",]
Kshade <- droplevels(Kshade)
Ksun<- leafk[leafk$leaf == "sun",]


##simple linear models of sun vs shade leaf nitro and k
N_lm<- lm(Nshade$leafN_area~ Nsun$leafN_area)
summary(N_lm)
Namb_lm <- lm(Nshade[Nshade$temp =="ambient", "leafN_area"] ~Nsun[Nsun$temp =="ambient", "leafN_area"])
summary(Namb_lm)
Nelev_lm<- lm(Nshade[Nshade$temp =="elevated", "leafN_area"] ~Nsun[Nsun$temp =="elevated", "leafN_area"])
summary(Nelev_lm)


K_lm<- lm(Kshade$leafK~ Ksun$leafK)
summary(K_lm)
Kamb_lm <- lm(Kshade[Kshade$temp =="ambient", "leafK"] ~Ksun[Ksun$temp =="ambient", "leafK"])
summary(Kamb_lm)
Kelev_lm<- lm(Kshade[Kshade$temp =="elevated", "leafK"] ~Ksun[Ksun$temp =="elevated", "leafK"])
summary(Kelev_lm)

####plotting----------------------------------------------------------------------------------
palette(c("blue", "red"))
leafK_lab <- expression(Leaf~K~~(mmol~H[2]*O~m^-2~s^-1~MPa^-1))

bar(leafK, leaf, leafk, col=c("yellowgreen", "green4"), ylim=c(0, 4),xlab="", 
    ylab=leafK_lab,half.errbar=FALSE, mar=c(5,5,2,2), 
    cex.axis=1.25, cex.label= 1.5, cex.names=1.5)




###nitro area sun v shade
plot(Nshade$leafN_area ~ Nsun$leafN_area , ylim=c(0,5), xlim=c(0, 5), xlab=nsunlab, ylab="", 
      cex=1.25,col=Nsun$temp)
  title(ylab=nshadelab, mgp=ypos)
  abline(0,1, lty=2)
  ablineclip(Namb_lm, lty=2, lwd=1.5,x1=min(Nsun$leafN_area), x2=max(Nsun$leafN_area), col="blue")
  ablineclip(Nelev_lm, lty=2, lwd=1.5,x1=min(Nsun$leafN_area), x2=max(Nsun$leafN_area), col="red")
dev.copy2pdf(file= "master_scripts/figures/leafnitro_sunsha.pdf")
dev.off()

plot(Kshade$leafK ~ Ksun$leafK, pch=16, ylim=c(0,6), xlim=c(0, 6), ylab="", col=Ksun$temp,
     xlab=ksunlab ,cex=1.25)
  title(ylab=kshadelab, mgp=ypos)
  abline(0,1, lty=2)
  ablineclip(Kamb_lm, lty=2, x1=min(Ksun$leafK), x2=max(Ksun$leafK), col="blue")
  ablineclip(Kelev_lm, lty=2, x1=min(Ksun$leafK), x2=max(Ksun$leafK), col="red")
dev.copy2pdf(file= "master_scripts/figures/leafK_sunsha.pdf")
dev.off()

##plot didtribution of N and Leaf K as a function of relative PPFD on shade and sun leaves

##nitro
windows(7,5)

plot(n_diff~par_diff,data=leaf_sp, col=cols, pch=16, cex=1.5, ylim=c(0,1.2), xlim=c(0, 1.2),
     xlab=relparlab, ylab="")
title(ylab=relnitrolab, mgp=ypos)
abline(0,1, lty=2)

legend("bottomright", templab, pch=16, col=cols,pt.cex=1.5,inset = 0.03)  

dev.copy2pdf(file= "master_scripts/figures/relativenitro.pdf")
dev.off()

##leafK
windows(7,5)
plot(k_diff~par_diff,data=leaf_sp2, col=cols, pch=16, cex=1.5, ylim=c(0,4), xlim=c(0, 4),
     xlab=relparlab, ylab="")
title(ylab=relklab, mgp=ypos)
abline(0,1, lty=2)

legend("bottomright", templab, pch=16, col=cols, pt.cex=1.5, inset = 0.03)


dev.copy2pdf(file= "master_scripts/figures/relativeK.pdf")
dev.off()



#nitro_K
plot(n_diff~k_diff,data=leaf_sp3, col=cols, pch=16, cex=1.5, ylim=c(0,4), xlim=c(0, 4),
     xlab=relklab, ylab="")
title(ylab=relnitrolab, mgp=ypos)
abline(0,1, lty=2)

legend("bottomright", templab, pch=16, col=cols, pt.cex=1.5, inset = 0.03)

dev.copy2pdf(file= "master_scripts/figures/KN_sunshade.pdf")
dev.off()

