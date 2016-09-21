
options(repos="http://cran.rstudio.com/")

r <- require(pacman)
if(!r)install.packages("pacman")
#pacman:::p_set_cranrepo()
pacman::p_load(tibble,broom,reporttools,lubridate, doBy, plyr,plotrix, wesanderson, plantecophys, 
               lme4,scales,reshape, pixiedust)



source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")

##read table ready dataframes
resourcetab <- read.csv("master_scripts/resource_table.csv", stringsAsFactors = FALSE)
getab <- read.csv("master_scripts/ge_table.csv", stringsAsFactors = FALSE)
pval <- c(0.781, 0.001, 0.001, 0.001, 0.3486, 0.6385, 0.001)
pval2 <- c(0.001,	0.001,	0.001,	0.001,	0.001,	0.005,	0.001,	0.001)

