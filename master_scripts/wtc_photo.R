source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
library(doBy)
library(scales)

photo <- read.csv("raw data/gm_licor.csv")

treatments <- read.csv("raw data/temp_trt.csv")


photo2 <- chooseidfunc(photo, c("campaign" , "chamber",  "leaf",  "light"))
  photo2$leaflight <- as.factor(paste(photo2$leaf,photo2$light, sep="-"))

#remove lights on treatment
wtc_photo <- photo2[photo2$leaflight == "sun-high" | photo2$leaflight == "shade-low", c(2:5,7, 12:16, 25,64:65) ]
  #add months and treatments
  wtc_photo$campaign <- as.factor(wtc_photo$campaign)
  wtc_photo <- chlab_func(wtc_photo)
  wtc_photo <- add_Month(wtc_photo)
  wtc_photo <- addtrt_func(wtc_photo)
  wtc_photo <- droplevels(wtc_photo)

write.csv(wtc_photo, "calculated_data/gasexchange_basic.csv", row.names=FALSE)

#   #order my month over 2013-14
#   Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")
#   wtc_photo$Month <- factor(wtc_photo$Month, levels = Morder)

photo_agg <- summaryBy(Photo+Cond+Ci+Trmmol+VpdL ~ leaf+campaign+temp, data=wtc_photo, FUN=c(mean))
photo_agg$campaign <- as.numeric(photo_agg$campaign)

test <- wtc_photo[wtc_photo$leaf=="sun",] 
test2 <- wtc_photo[wtc_photo$leaf=="shade",]

###plotting-----------------------------------------------------------------------------------

##plots leaf traits across campaigns

shadecols <- alpha(cols, .5)
Morder <- c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr")


plot(Photo.mean ~ campaign, data=photo_agg, subset=leaf=="sun", pch=16, col=cols, cex=1.5, ylim=c(0, 20), 
     ylab=photolab, xlab="",axes=FALSE)
points(Photo.mean ~ campaign, data=photo_agg, subset=leaf=="shade", pch=21, bg=shadecols, col="grey30",cex=1.5, ylim=c(0, 20))
box()
axis(1, labels = Morder, at= c(1,2,3,4,5,6))
axis(2, labels=TRUE)



