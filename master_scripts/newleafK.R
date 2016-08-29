source("functions and packages/functions.R")
source("functions and packages/packages.R")

treatments <- read.csv("raw data/temp_trt.csv")

#read and format leaf water potential---------------------------------------------------------------
leafdata <- read.csv("raw data/leaf_data.csv")
  #water potential only
  wp <- leafdata[, c(1, 3:6)]
  #reshape data
  wp_day <- (cast(wp, Month+ chamber + leaf ~ wp_type, value = "water_potential"))

  #add treatments------------------------------------------------------------------------------------------------------
  wptrt_func <- function(x){
    x <- merge(x, treatments)
    x$drydown <- ifelse(x$Month %in% c("Mar", "Apr") & x$chamber %in%c("ch01", "ch03", "ch04", "ch06", "ch08", "ch11"), 
                        "drought", "control")
    return(x)
  }

WP <- wptrt_func(wp_day)
  #convert to millipascals
  WP$pre_mp <- with(WP, ((pre/10)*-1))
  WP$mid_mp <- with(WP, ((mid/10)*-1))


#read in licor data and extract transpiration-----------------------------------------------------------------------
##using raw files, gmes well watered dfr has a few removed gmes outliers, so using complete raw data here:
  
licor_raw <- read.csv("raw data/gm_licor.csv") #this is base raw data
  transp <- licor_raw[,c("campaign", "chamber", "leaf", "light", "Trmmol", "Cond")]
  transp <- chlab_func(transp)
  transp <- add_Month(transp)
  transp <- wptrt_func(transp)
  
  #means by campaign
  transp_agg <- summaryBy(Trmmol+Cond ~ Month +chamber+leaf+light+drydown, data= transp, FUN=c(mean), keep.names=TRUE)
    transp_agg$leaflight <- paste(transp_agg$leaf, transp_agg$light, sep="-")
###remove drought treatment from raw licor data
E_dat <- transp_agg[transp_agg$drydown == "control",]
  E_dat <- droplevels(E_dat)
  
##prelim plot  
  E_dat2 <- subset(E_dat,  chamber != "ch10" | Month != "Mar")
  plot(E_dat2[E_dat2$leaflight=="shade-high","Trmmol"] ~ E_dat2[E_dat2$leaflight=="shade-low","Trmmol"], data=E_dat2,
       xlab= "Trmmol Shade-Low", ylab = "Trmmol Shade-High", xlim=c(0,8), ylim=c(0,8))
  abline(0,1)

  ##seperate files by leaflight of shade leaves:
E_norm <- subset(E_dat, transp_agg$leaflight !=  "shade-high")
  E_norm$Month <- as.factor(E_norm$Month)
  E_norm$chamber<- as.factor(E_norm$chamber)
  E_norm$leaf<- as.factor(E_norm$leaf)

E_fleck <- subset(E_dat, transp_agg$leaflight !=  "shade-low")
  E_fleck$Month <- as.factor(E_fleck$Month)
  E_fleck$chamber<- as.factor(E_fleck$chamber)
  E_fleck$leaf<- as.factor(E_fleck$leaf)
  
#merge each of these files with water potential data and calculate leaf K-------------------------------------------------
leafK_norm <- merge(E_norm, WP)
  leafK_norm$wpdiff <- with(leafK_norm, abs(mid_mp - pre_mp))
  leafK_norm$leafK <- with(leafK_norm, Trmmol/wpdiff)
  #missing data for march ch10 shade, so remove sun leaf
  leafK_norm2 <- subset(leafK_norm,  chamber != "ch10" | Month != "Mar")
  
leafK_fleck <- merge(E_fleck ,WP)
  leafK_fleck$wpdiff <- with(leafK_fleck, abs(mid_mp - pre_mp))
  leafK_fleck$leafK <- with(leafK_fleck, Trmmol/wpdiff)
  
# write.csv(leafK_norm, "calculated_data/leafK_nodrought.csv", row.names = FALSE)
# write.csv(leafK_fleck,"calculated_data/leafK_nodrought_highlight.csv", row.names = FALSE)  

#prelim plotting---------------------------------------------------------------------------------------------------------
ypos <- c(2.5,1,0)
leafK_lab <- expression(Leaf-K~~(mmol~H[2]*O~m^-2~s^-1~MPa^-1))

#well watered trees
bar(leafK, c(leaf, Month), leafK_norm, col=c("yellowgreen", "green4"),ylim=c(0, 4),half.errbar=FALSE,
    xlab="",ylab="")
title(ylab=leafK_lab, mgp=ypos)

bar(leafK, c(leaf, Month), leafK_fleck, col=c("yellowgreen", "green4"),ylim=c(0, 4),half.errbar=FALSE,
    xlab="",ylab="")
title(ylab=leafK_lab, mgp=ypos)

#Stats  -----------------------------------------------------------------------------------------------------------------
#examite data with boxplots, lool remove any outliers
boxplot(leafK~temp+leaf, data=leafK_norm)
boxplot(leafK~temp+leaf, data=leafK_fleck)

library(visreg)
library(multcomp)
library(nlme)


##1. lowlight shade K
leafK_norm$tukeyid <- as.factor(paste(leafK_norm$leaf, leafK_norm$temp, sep="-"))

  ###temp trt?  NO
  knorm_temp <- lme(leafK ~ temp ,random=~1|chamber, data=leafK_norm)
  summary(knorm_temp)
  anova(knorm_temp)


##2. highlight shade K
leafK_fleck$tukeyid <- as.factor(paste(leafK_fleck$leaf, leafK_fleck$temp, sep="-"))

  ###temp trt?  NO
  kfleck_temp <- lme(leafK ~ temp ,random=~1|chamber, data=leafK_fleck)
  summary(kfleck_temp)
  anova(kfleck_temp)


##3. Full model on high light leaves for manuscript
k_leaf <- lme(leafK~ tukeyid, random=~1|chamber, data=leafK_fleck)
summary(k_leaf)
anova(k_leaf)
visreg(k_leaf)

tukey_k<- glht(k_leaf, linfct = mcp(tukeyid = "Tukey"))
k_siglets<- cld(tukey_k)
k_siglets2 <- k_siglets$mcletters$Letters

write.csv(k_siglets2, "master_scripts/sigletters/slr_k.csv", row.names=FALSE)

