source("functions and packages/functions.R")
library(doBy)
library(stringr)

photo <- read.csv("raw data/gm_licor.csv")

treatments <- read.csv("raw data/temp_trt.csv")


photo2 <- chooseidfunc(photo, c("campaign" , "chamber",  "leaf",  "light"))
  photo2$leaflight <- paste(photo2$leaf,photo2$light, sep="-")

#remove lights on treatment
wtc_photo <- photo2[photo2$leaflight == "sun-high" | photo2$leaflight == "shade-low", c(2:5,11:15, 63) ]
  #add months and treatments
  wtc_photo <- add_Month(wtc_photo)
  wtc_photo <- addtrt_func(wtc_photo)
  wtc_photo$leaf <- as.factor(wtc_photo$leaf)
droplevels(wtc_photo$leaf)

photo_agg <- summaryBy(Photo+Cond+Ci+Trmmol+VpdL ~ leaf+campaign+temp, data=wtc_photo, FUN=c(mean))




plot(Photo ~ campaign, data=wtc_photo, pch=16, col=as.factor(leaf))
