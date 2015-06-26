source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

library(mgcv)
library(lme4)
library(lmerTest)


#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ CTleaf+gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
  gm_agg$leaf <- gsub("s", "S", gm_agg$leaf)
  gm_agg$light <- gsub("high", "Sun-light", gm_agg$light)
  gm_agg$light <- gsub("low", "Shade-light", gm_agg$light)

##remove shade-high
gm_sunsha <- gm_agg[gm_agg$leaflight != "shade-high",]
  gm_sunsha <- droplevels(gm_sunsha)

##sun-shade dataframes
sundat <- gm_sunsha[gm_sunsha$leaflight =="sun-high",]
shadat <- gm_sunsha[gm_sunsha$leaflight =="shade-low",]

##sunfleck dfr
gm_fleck<- gm_agg[gm_agg$leaflight == "shade-high",]
gm_fleck <- droplevels(gm_fleck)

###dfrs with variables for testing
agm_sun <- sundat[, c("Photo", "gm", "chamber")]
agm_sha <- shadat[, c("Photo", "gm", "chamber")]
agm_fleck <- gm_fleck[,  c("Photo", "gm", "chamber")]

gmt_sun <- sundat[, c("CTleaf", "gm", "chamber")]
gmt_sha <- shadat[, c("CTleaf", "gm", "chamber")]
gmt_fleck <- gm_fleck[, c("CTleaf", "gm", "chamber")]


####CI for gm vs temperature and photosynthesis using bootstrapping-------------------------------------------------

#1: gm vs leaf Temp

Tgm_sun_lm <- lmer(gm~CTleaf + (1|chamber), data=gmt_sun)

boot_tgmsun <- bootMer(Tgm_sun_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
gmt_sun$lcl <- apply(boot_tgmsun$t, 2, quantile, 0.025)
gmt_sun$ucl <- apply(boot_tgmsun$t, 2, quantile, 0.975)
gmt_sun$pred <- predict(Tgm_sun_lm, re.form=NA)
gmt_sun <- arrange(gmt_sun, CTleaf) 

Tgm_sha_lm <- lmer(gm~CTleaf+ (1|chamber), data=gmt_sha)

boot_tgmsha <- bootMer(Tgm_sha_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
gmt_sha$lcl <- apply(boot_tgmsha$t, 2, quantile, 0.025)
gmt_sha$ucl <- apply(boot_tgmsha$t, 2, quantile, 0.975)
gmt_sha$pred <- predict(Tgm_sha_lm, re.form=NA)
gmt_sha <- arrange(gmt_sha, CTleaf) 

#2: Photosynthesis vs gm

Agm_sun_lm <- lmer(Photo ~ gm + (1|chamber), data=agm_sun)

boot_agmsun <- bootMer(Agm_sun_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
agm_sun$lcl <- apply(boot_agmsun$t, 2, quantile, 0.025)
agm_sun$ucl <- apply(boot_agmsun$t, 2, quantile, 0.975)
agm_sun$pred <- predict(Agm_sun_lm, re.form=NA)
agm_sun <- arrange(agm_sun, gm)

Agm_sha_lm <- lmer(Photo ~ gm + (1|chamber), data=agm_sha)

boot_agmsha <- bootMer(Agm_sha_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
agm_sha$lcl <- apply(boot_agmsha$t, 2, quantile, 0.025)
agm_sha$ucl <- apply(boot_agmsha$t, 2, quantile, 0.975)
agm_sha$pred <- predict(Agm_sha_lm, re.form=NA)
agm_sha <- arrange(agm_sha, gm)

Agm_fleck_lm <- lmer(Photo ~ gm + (1|chamber), data=agm_fleck)

boot_agmfleck <- bootMer(Agm_fleck_lm, FUN=function(x)predict(x, re.form=NA),nsim=999)
agm_fleck$lcl <- apply(boot_agmfleck$t, 2, quantile, 0.025)
agm_fleck$ucl <- apply(boot_agmfleck$t, 2, quantile, 0.975)
agm_fleck$pred <- predict(Agm_fleck_lm, re.form=NA)
agm_fleck <- arrange(agm_fleck, gm)

#agm
write.csv(agm_sun, "master_scripts/bootstrap_results/agm_sun.csv", row.names=FALSE)
write.csv(agm_sha, "master_scripts/bootstrap_results/agm_sha.csv", row.names=FALSE) 
write.csv(agm_fleck, "master_scripts/bootstrap_results/agm_fleck.csv", row.names=FALSE) 

write.csv(gmt_sun, "master_scripts/bootstrap_results/gmt_sun", row.names=FALSE)
write.csv(gmt_sha, "master_scripts/bootstrap_results/gmt_sha", row.names=FALSE) 



