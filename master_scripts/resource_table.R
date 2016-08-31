source("functions and packages/packages.R")
source("functions and packages/functions.R")
treatments <- read.csv("raw data/temp_trt.csv") 

##sun shade resource data table
##means/se by sun/shade and temperature treatment

##variables include:  WP*2, K, Narea, 13C, Vcmax, Jmax, lma

####aci parameters--------------------------------------------------------------------------------------------------
# aciparam <- read.csv("calculated_data/aciparameters.csv")

##use chorloplastic parameters instead
acigmparam <- read.csv("calculated_data/aciparameters_gm.csv")

aci_agg <- summaryBy(Vcmax+Jmax ~ leaf+temp, data=acigmparam, FUN=c(mean, se))


####Narea/LMA/13c-----------------------------------------------------------------------------------------------
leaf_chem <- read.csv("raw data/leaf_chem.csv")
leaf_mass <- read.csv("raw data/leaf_data.csv")

#calculate nitrogen content in leaves
canopy_chem <- merge(leaf_mass[leaf_mass$wp_type=="mid",c(1, 3:4, 7:8)], leaf_chem[, 4:8], by=c("Month", "chamber", "leaf"))
  #add treatments  
  canopy_chem <- addtrt_func(canopy_chem)
  canopy_chem$leafN <- with(canopy_chem, leaf_mass *(n_perc/100))
  canopy_chem$lma <- with(canopy_chem, (leaf_mass/leaf_area)*10000) #g/m2
  canopy_chem$leafN_area <- with(canopy_chem, lma *(n_perc/100))
  canopy_chem<- add_campaign(canopy_chem)

canopy_chem2 <- canopy_chem[canopy_chem$drydown=="control",]

canopy_agg <- summaryBy(lma+leafN_area+ c13 ~ leaf + temp, data=canopy_chem2, FUN=c(mean, se))

resourcetab <- merge(aci_agg, canopy_agg)

#### leaf K and water potential
waterdat <- read.csv("calculated_data/leafK_nodrought_highlight.csv")
water_agg <- summaryBy(pre_mp+mid_mp+leafK ~ leaf+temp, data=waterdat, FUN=c(mean,se))

resourcetab <- merge(resourcetab, water_agg)

###Amax
amax <- read.csv("calculated_data/amax.csv")
resourcetab <- merge(resourcetab, amax)


####order, split into mean/se, fix rounding, and paste mean(se)

#order variables
leaf_table <- resourcetab[, c(1:2,7,19,8,15,13,14,9,10,20,11,18,16,17,12)]
leaf_table <- leaf_table[c(3:4, 1:2),]

leaf_vars <- leaf_table[, 1:2]
  leaf_vars$leaf <- gsub("s", "S", leaf_vars$leaf)
  leaf_vars$temp <- gsub("ambient", "Ambient", leaf_vars$temp)
  leaf_vars$temp <- gsub("elevated", "Elevated", leaf_vars$temp)

leaf_vars2 <-data.frame(Leaf = c("Sun", "", "Shade", ""), Temperature = c("AT", "ET", "AT","ET"))  
  
##split means and SE
leaf_means <- leaf_table[, 3:9]
leaf_se <- leaf_table[, 10:16]

###paste and round means and se together
v1 <- data.frame(paste0(sprintf("%2.1f", round(leaf_means[,1], 1)), " (", sprintf("%2.1f", round(leaf_se[,1],1)),")"))
v2 <- data.frame(paste0(sprintf("%2.1f", round(leaf_means[,2], 3)), " (", sprintf("%2.1f", round(leaf_se[,2],3)),")"))
v3 <- data.frame(paste0(sprintf("%1.2f", round(leaf_means[,3], 3)), " (", sprintf("%1.2f", round(leaf_se[,3],3)),")"))
v4 <- data.frame(paste0(sprintf("%1.2f", round(leaf_means[,4], 2)), " (", sprintf("%1.2f", round(leaf_se[,4],2)),")"))
v5 <- data.frame(paste0(sprintf("%1.2f",round(leaf_means[,5], 2)), " (", sprintf("%1.2f", round(leaf_se[,5],2)),")"))
v6 <- data.frame(paste0(sprintf("%2.2f",round(leaf_means[,6],2)), " (", sprintf("%1.2f",round(leaf_se[,6],2)),")"))
v7 <- data.frame(paste0(sprintf("%3.1f",round(leaf_means[,7], 1)), " (", sprintf("%1.2f",round(leaf_se[,7],2)),")"))

leaf_table2 <- cbind(leaf_vars2, v1)
leaf_table2 <- cbind(leaf_table2, v2)
leaf_table2 <- cbind(leaf_table2, v3)
leaf_table2 <- cbind(leaf_table2, v4)
leaf_table2 <- cbind(leaf_table2, v5)
leaf_table2 <- cbind(leaf_table2, v6)
leaf_table2 <- cbind(leaf_table2, v7)

###add sigletters (order of sig letters is shade AT, ET then sun AT:ET)-----------------------------------------------------

sigletter_files <- list.files(path = "master_scripts/sigletters/", pattern="slr", full.names = TRUE)
##make names of list with file names minus extension
sigletter_vars <- gsub("master_scripts/sigletters/", "", sigletter_files)
sigletter_vars <- gsub(".csv", "", sigletter_vars)

sigletter_list <- lapply(sigletter_files, function(x) read.csv(x))
##add names to list
names(sigletter_list) <- sigletter_vars

##add treatments
sigvol <- data.frame(leaf=c("shade", "shade", "sun", "sun"), temp=c("ambient", "elevated", "ambient", "elevated"))
siglet <- lapply(sigletter_list, function(x) cbind(x, sigvol))
siglet2 <- lapply(siglet, function(x) as.data.frame(x))

###do I need to match the sigletters order with that of datatable?
siglet3 <- list()
for(i in 1:7) {
  siglet3[[i]] <- siglet2[[i]][c(3,4,1,2),] 
}

###add sigletters to table--------------------------------------------------------------------------------------------------

leaf_table2[[3]] <- paste(leaf_table2[[3]], siglet3[[4]][,1])
leaf_table2[[4]] <- paste(leaf_table2[[4]], siglet3[[1]][,1])
leaf_table2[[5]] <- paste(leaf_table2[[5]], siglet3[[6]][,1])
leaf_table2[[6]] <- paste(leaf_table2[[6]], siglet3[[3]][,1])
leaf_table2[[7]] <- paste(leaf_table2[[7]], siglet3[[7]][,1])
leaf_table2[[8]] <- paste(leaf_table2[[8]], siglet3[[5]][,1])
leaf_table2[[9]] <- paste(leaf_table2[[9]], siglet3[[2]][,1])

write.csv(leaf_table2, "master_scripts/resource_table.csv", row.names=FALSE)





