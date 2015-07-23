library(devtools)
library(HIEv)
library(doBy)

setToken("u2xEk2wTte3AsdBxGTr5")

#Search HIEv for the ROS weather station data during the pot experiment
wtc_search <- searchHIEv(filename="WTC", startDate="2013-08-01", endDate="2014-06-01")

ch01 <- downloadTOA5(filename="WTC01_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch02 <- downloadTOA5(filename="WTC02_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch03 <- downloadTOA5(filename="WTC03_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch04 <- downloadTOA5(filename="WTC04_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch05 <- downloadTOA5(filename="WTC05_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch06 <- downloadTOA5(filename="WTC06_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch07 <- downloadTOA5(filename="WTC07_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch08 <- downloadTOA5(filename="WTC08_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch09 <- downloadTOA5(filename="WTC09_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch10 <- downloadTOA5(filename="WTC10_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch11 <- downloadTOA5(filename="WTC11_Table2", startDate = "2013-09-01",endDate="2014-06-01")
ch12 <- downloadTOA5(filename="WTC12_Table2", startDate = "2013-09-01",endDate="2014-06-01")

ch_list <- list(ch01, ch01, ch03, ch04, ch05, ch06, ch07, ch08, ch09, ch10, ch11, ch12)

test<- ch01[complete.cases(ch01),]
row.names(test) <- NULL

test2 < test[, c("Date", "PPFD_Avg")]

test_agg <- summaryBy(PPFD_Avg ~ Date, data=ch01,FUN=test)

names(test)[4] <- "Temp"

met_fun <- function(x) {
  names(x)[4] <- "Temperature"
   x$ppfd15_mol <- x$PPFD_Avg/1000000
   x$par15_mol_s <- x$ppfd15_mol*60
  y <- x[, c("DateTime", "PPFD_Avg", "Temperature", "par15_mol_s","Date")]
  return(y)
  }

ch_met <- lapply(ch_list,  met_fun)

###get total daily par for each chamber and then save as a dataframe

daypar <- lapply(ch_met, function(x) aggregate(par15_mol_s ~ Date, data=x,FUN=sum))

with(daypar[[4]], plot(Date, par15_mol_s, type = "l"))

###air temp

airvars_min <- lapply(ch_met, function(x) aggregate(Temperature~Date, data=x, FUN=min))
airvars_max <- lapply(ch_met, function(x) aggregate(Temperature~Date, data=x, FUN=max))
airvars_mean <- lapply(ch_met, function(x) aggregate(Temperature~Date, data=x, FUN=mean))

with(airvars_min[[2]], plot(Date, Temperature, type = "l", col="red"))   


