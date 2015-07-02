source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")
require(lme4)
require(lmerTest)
library(LMERConvenienceFunctions)

##Stats on c13 and leaf N-------------------------------

#read data
treatments <- read.csv("raw data/temp_trt.csv")

leaf_chem <- read.csv("raw data/leaf_chem.csv")
leaf_mass <- read.csv("raw data/leaf_data.csv")

#get vcmax per chamber 
aciparam <- read.csv("calculated_data/aciparameters.csv")

#calculate nitrogen content in leaves
canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
#add treatments  
canopy_chem <- addtrt_func(canopy_chem)
canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
canopy_chem<- add_campaign(canopy_chem)

chem_mod <- lm(c13 ~ leafN_area, data=canopy_chem)
summary(chem_mod)

chem_mod2 <- lmer(c13 ~ leafN_area + (1|chamber),data=canopy_chem)
anova(chem_mod2)
summary(chem_mod2)
mcp.fnc(chem_mod2)
source("functions and packages/r2glmm.R")
rsquared.glmm(chem_mod2)

