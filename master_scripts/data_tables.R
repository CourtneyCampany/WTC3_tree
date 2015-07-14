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


ge_table <- ge_wet[ c(6,5,2,4,1,3),]

ge_vars <- ge_table[, 1:3]
  ge_vars$leaflight <- gsub("sun-", "", ge_vars$leaflight)
  ge_vars$leaflight <- gsub("shade-", "", ge_vars$leaflight)
  ge_vars$leaflight <- gsub("high", "High", ge_vars$leaflight)
  ge_vars$leaflight <- gsub("l", "L", ge_vars$leaflight)
  ge_vars$leaf <- gsub("s", "S", ge_vars$leaf)
  ge_vars$temp <- gsub("ambient", "Ambient", ge_vars$temp)
  ge_vars$temp <- gsub("elevated", "Elevated", ge_vars$temp)
  
ge_means <- ge_table[, c(5,6,9,7,8, 10)]
ge_se <- ge_table[, c(11,12,15,13,14,16)]

###paste and round means and se together
ge1 <- data.frame(paste0(sprintf("%2.1f", round(ge_means[,1], 1)), " (", sprintf("%2.1f", round(ge_se[,1],1)),")"))
ge2 <- data.frame(paste0(sprintf("%1.3f", round(ge_means[,2], 3)), " (", sprintf("%1.3f", round(ge_se[,2],3)),")"))
ge3 <- data.frame(paste0(sprintf("%1.3f", round(ge_means[,3], 3)), " (", sprintf("%1.3f", round(ge_se[,3],3)),")"))
ge4 <- data.frame(paste0(sprintf("%3.1f", round(ge_means[,4], 1)), " (", sprintf("%1.1f", round(ge_se[,4],1)),")"))
ge5 <- data.frame(paste0(sprintf("%1.2f",round(ge_means[,5], 2)), " (", round(ge_se[,5],2),")"))
ge6 <- data.frame(paste0(sprintf("%1.2f",round(ge_means[,6], 1)), " (", sprintf("%1.2f",round(ge_se[,6],2)),")"))


ge_table3 <- cbind(ge_vars, ge1)
  ge_table3 <- cbind(ge_table3, ge2)
  ge_table3 <- cbind(ge_table3, ge3)
  ge_table3 <- cbind(ge_table3, ge4)
  ge_table3 <- cbind(ge_table3, ge5)
  ge_table3 <- cbind(ge_table3, ge6)
  
###add sigletters

write.csv(ge_table3, "master_scripts/ge_table.csv", row.names=FALSE)
  

  
  