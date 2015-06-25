source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")


#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
Ci_bar <- read.csv("calculated_data/Ci_bar.csv")
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###calculate g1 with all gmes data, average of spot measurements by id 
gs_dat <- summaryBy(Photo+Cond+VpdL+CO2R + Trmmol ~ chamber+id+leaf +light+temp+leaflight+Month, 
                    data=gmes, FUN=mean, keep.names=TRUE)

gs_dat$ite <- with(gs_dat, Photo/Trmmol)

names(gs_dat)[8:11] <-c("A", "gs", "D", "Ca") 

gs_dat$id2 <- as.factor(paste(gs_dat$leaflight, gs_dat$temp, sep="-"))
gs_dat$id3 <- as.factor(paste(gs_dat$leaflight, gs_dat$temp, gs_dat$Month,sep="-"))
gs_dat$id4 <- as.factor(paste(gs_dat$leaflight, gs_dat$temp, gs_dat$chamber,sep="-"))


#use nls list for g1 for optimal gs model---------------------------------------------------------------
library(nlme)
nlsfits <- nlsList(gs ~  1.6*(1+g1/sqrt(D))*(A/Ca) |id2,
                   start=list(g1=8),data=gs_dat)


g1_dat <- data.frame(coef(nlsfits))
g1_dat$id <- as.character(rownames(g1_dat))
row.names(g1_dat) <- NULL
names(g1_dat)[1] <- "g1"

write.csv(g1_dat, "calculated_data/g1_leaf.csv", row.names=FALSE)


#use nls for g1 ite model-----------------------------------------------------------------------
g1fits <- nlsList(ite ~ (Ca*102.3) / (1.6*(g1*sqrt(D)+D))/1000 |id2,
                  start=list(g1=2), data=gs_dat)



g1_ite <- data.frame(coef(g1fits))
g1_ite$id <- as.character(rownames(g1_ite))
row.names(g1_ite) <- NULL
names(g1_ite)[1] <- "g1"

write.csv(g1_ite, "calculated_data/g1_ite.csv", row.names=FALSE)

###sun leaves have low g1, thus high lambda meaning they are more conservative than shade leaves

# Figure
cols <- c("darkorange","forestgreen")
with(gs_dat, plot(D, ite, pch=19, col=cols[leaf]))
p <- coef(g1fits)[[1]]
f <- function(D, g1)(400*102.3) / (1.6*(g1*sqrt(D)+D))/1000
for(i in 1:6)curve(f(x, p[i]), add=T, col=c(rep(cols[1],4),rep(cols[2],2))[i])

