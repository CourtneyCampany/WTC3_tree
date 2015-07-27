source("functions and packages/functions.R")
library(doBy)

gasexchange  <- read.csv("calculated_data/gmes_wtc.csv")

###leaf data
ge_agg <- summaryBy(Photo+Cond+Ci+Trmmol+gm+VpdL ~ chamber+id+leaf +light+temp+leaflight+Month+drydown, 
                    data=gasexchange, FUN=mean, keep.names=TRUE)

ge_overall <- summaryBy(Photo+Cond+Ci+Trmmol+gm+VpdL ~ leaf +temp+leaflight+drydown, 
                        data=gasexchange, FUN=c(mean, se))


###split by drought and then rearrange
ge_wet <- ge_overall[ge_overall$drydown == "control",]
ge_wet <- droplevels(ge_wet)

ge_dry <- ge_overall[ge_overall$drydown == "drought",]


###add ITE------------------------------------------------------------------------------------------------------------------- 
ITE <- read.csv("calculated_data/ITE.csv")  
ite_agg <- summaryBy(iWUE ~ leaf+temp+leaflight, data=ITE, FUN=c(mean, se))

ge_wet <- merge(ge_wet, ite_agg, by=c("leaf", "leaflight", "temp"))                     

###format datatable ------------------------------------------------------------------------------------------------------                   
ge_table <- ge_wet[ c(5,6,3,4,1,2),]

ge_vars <- ge_table[, 1:3]
  ge_vars$leaflight <- gsub("sun-", "", ge_vars$leaflight)
  ge_vars$leaflight <- gsub("shade-", "", ge_vars$leaflight)
  ge_vars$leaflight <- gsub("high", "High", ge_vars$leaflight)
  ge_vars$leaflight <- gsub("l", "L", ge_vars$leaflight)
  ge_vars$leaf <- gsub("s", "S", ge_vars$leaf)
  ge_vars$temp <- gsub("ambient", "Ambient", ge_vars$temp)
  ge_vars$temp <- gsub("elevated", "Elevated", ge_vars$temp)
  
##no Ci/Cc for now
##order: A, gs, gm, ite, e, vpd  
ge_means <- ge_table[, c(5,6,9,17,8,10)]
ge_se <- ge_table[, c(11,12,15,18,14,16)]


###paste and round means and se together
ge1 <- data.frame(paste0(sprintf("%2.1f", round(ge_means[,1], 1)), " (", sprintf("%2.1f", round(ge_se[,1],1)),")"))
ge2 <- data.frame(paste0(sprintf("%1.3f", round(ge_means[,2], 3)), " (", sprintf("%1.3f", round(ge_se[,2],3)),")"))
ge3 <- data.frame(paste0(sprintf("%1.3f", round(ge_means[,3], 3)), " (", sprintf("%1.3f", round(ge_se[,3],3)),")"))
ge4 <- data.frame(paste0(sprintf("%1.2f", round(ge_means[,4], 2)), " (", sprintf("%1.2f", round(ge_se[,4],2)),")"))
ge5 <- data.frame(paste0(sprintf("%1.2f",round(ge_means[,5], 2)), " (", round(ge_se[,5],2),")"))
ge6 <- data.frame(paste0(sprintf("%1.2f",round(ge_means[,6], 1)), " (", sprintf("%1.2f",round(ge_se[,6],2)),")"))


ge_table3 <- cbind(ge_vars, ge1)
  ge_table3 <- cbind(ge_table3, ge2)
  ge_table3 <- cbind(ge_table3, ge3)
  ge_table3 <- cbind(ge_table3, ge4)
  ge_table3 <- cbind(ge_table3, ge5)
  ge_table3 <- cbind(ge_table3, ge6)
  
###add sigletters
  
sigletter_files <- list.files(path = "master_scripts/sigletters/", pattern="lightson", full.names = TRUE)
##make names of list with file names minus extension
sigletter_vars <- gsub("master_scripts/sigletters/", "", sigletter_files)
sigletter_vars <- gsub(".csv", "", sigletter_vars)
  
sigletter_list <- lapply(sigletter_files, function(x) read.csv(x))
  ##add names to list
  names(sigletter_list) <- sigletter_vars
  
##add treatments
sigvol <- data.frame(leaf=c("shade", "shade", "shade", "shade", "sun", "sun"), 
                     light=c("high", "low", "high", "low", "high", "low"),
                     temp=c("ambient", "elevated", "ambient", "elevated", "ambient", "elevated"))
siglet <- lapply(sigletter_list, function(x) cbind(x, sigvol))
siglet2 <- lapply(siglet, function(x) as.data.frame(x))
  
###match the sigletters order with that of datatable variables
siglet3 <- list()
  for(i in 1:6) {
    siglet3[[i]] <- siglet2[[i]][c(5,6,3,4,1,2),] 
  }
  

###add sigletters to table--------------------------------------------------------------------------------------------------
ge_table3[[4]] <- paste(ge_table3[[4]], siglet3[[1]][,1])
ge_table3[[5]] <- paste(ge_table3[[5]], siglet3[[4]][,1])
ge_table3[[6]] <- paste(ge_table3[[6]], siglet3[[3]][,1])
ge_table3[[7]] <- paste(ge_table3[[7]], siglet3[[5]][,1])
ge_table3[[8]] <- paste(ge_table3[[8]], siglet3[[2]][,1])
ge_table3[[9]] <- paste(ge_table3[[9]], siglet3[[6]][,1])

write.csv(ge_table3, "master_scripts/ge_table.csv", row.names=FALSE)
  

  
  