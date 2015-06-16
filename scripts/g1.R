source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")


#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------

Ci_bar <- read.csv("calculated_data/Ci_bar.csv")

gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
##calculate CC
gmes$Cc<- with(gmes, Ci-Photo/gm)
gmes$cc_ci<- with(gmes, Ci/Cc)



##remove shade-high
 gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
# gm_sunsha <- droplevels(gm_sunsha)
# gm_sunsha$id <- gsub("-high", "", gm_sunsha$id)
# gm_sunsha$id <- gsub("-low", "", gm_sunsha$id)

##merge
gm_c13 <- merge(gm_sunsha, Ci_bar[, c(2,8)], by="id")


###calculate g1 with all gmes data, average of spot measurements by id 
(gs_dat <- summaryBy(Photo+Cond+VpdL+CO2R ~ chamber+id+leaf +light+temp+leaflight+Month, 
                    data=gmes, FUN=mean, keep.names=TRUE))

names(gs_dat)[8:11] <-c("A", "gs", "D", "Ca") 

gs_dat$id2 <- as.factor(paste(gs_dat$leaflight, gs_dat$temp, sep="-"))
gs_dat$id3 <- as.factor(paste(gs_dat$leaflight, gs_dat$temp, gs_dat$Month,sep="-"))
gs_dat$id4 <- as.factor(paste(gs_dat$leaflight, gs_dat$temp, gs_dat$chamber,sep="-"))


#use nls list instead of  loop
library(nlme)
nlsfits <- nlsList(gs ~  1.6*(1+g1/sqrt(D))*(A/Ca) |id2,
                   start=list(g1=8),data=gs_dat)


g1_dat <- data.frame(coef(nlsfits))
g1_dat$id <- as.character(rownames(g1_dat))
row.names(g1_dat) <- NULL
names(g1_dat)[1] <- "g1"

###sun leaves have low g1, thus high lambda meaning they are more conservative than shade leaves



