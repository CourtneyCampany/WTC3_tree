source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

###read data and format data-----------------------------------------------------------------------------
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


####merge leaf N with PAR---------------------------------------------------------------------------------


par_nitro <- merge(leaf_par3, leafN2[,c(1:3, 8:9, 12:13)])


leaf_sp <- ddply(par_nitro, .(chamber, Month), function(x) rbind.fill(
  data.frame(par_diff = x$par[2]- x$par[1], n_diff= x$leafN_area[2]- x$leafN_area[1], temp=x$temp[1])))