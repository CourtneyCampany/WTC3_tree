library(plyr)
library(devtools)
library(HIEv)
library(doBy)


setToken("u2xEk2wTte3AsdBxGTr5")

#Search HIEv for the ROS weather station data during the pot experiment
wtc_search <- searchHIEv(filename="wTCMET")
wtc_search2 <- searchHIEv(filename="OUTMET")

met_names <- 

###download both sets met files (PPFD oustide chamber and AIR and )
oct_met <- downloadCSV(filename="WTC_TEMP_CM_WTCMET_20131001")
nov_met <- downloadCSV(filename="WTC_TEMP_CM_WTCMET_20131101")
dec_met <- downloadCSV(filename="WTC_TEMP_CM_WTCMET_20131201")
jan_met <- downloadCSV(filename="WTC_TEMP_CM_WTCMET_20140101")
feb_met <- downloadCSV(filename="WTC_TEMP_CM_WTCMET_20140201")
mar_met <- downloadCSV(filename="WTC_TEMP_CM_WTCMET_20140301")
apr_met <- downloadCSV(filename="WTC_TEMP_CM_WTCMET_20140401")

oct_met2 <- downloadCSV(filename="WTC_TEMP_CM_OUTMET_20131001")
nov_met2 <- downloadCSV(filename="WTC_TEMP_CM_OUTMET_20131101")
dec_met2 <- downloadCSV(filename="WTC_TEMP_CM_OUTMET_20131201")
jan_met2 <- downloadCSV(filename="WTC_TEMP_CM_OUTMET_20140101")
feb_met2 <- downloadCSV(filename="WTC_TEMP_CM_OUTMET_20140201")
mar_met2 <- downloadCSV(filename="WTC_TEMP_CM_OUTMET_20140301")
apr_met2 <- downloadCSV(filename="WTC_TEMP_CM_OUTMET_20140401")


chamber_met <- list(oct_met, nov_met, dec_met, jan_met, feb_met, mar_met, apr_met)
outside_met <- list(oct_met2, nov_met2, dec_met2, jan_met2, feb_met2, mar_met2, apr_met2)

##function to keep datetime, Tair, RH----------------------------------------------------------------------------------

vars_func <- function(x) {
  
    dat<- x[ , c("chamber", "RH_al","DateTime", "Tair_al", "PPFD_Avg")]
    
    dat$PPFD_Avg <- ifelse(dat$PPFD_Avg < 0, 0, dat$PPFD_Avg)
    dat$ppfd_mol <- dat$PPFD_Avg/1000000
    dat$PPFD15_mol_s <- dat$ppfd_mol*15*60
    
    dat$Timestamp <- ymd_hms(dat$DateTime)
    dat$Date <- as.Date(dat$Timestamp)
    
    dat2 <- dat[, c("chamber", "DateTime", "RH_al", "Tair_al", "PPFD15_mol_s", "Date")]
   return(dat2)
  }
  
chams_met <- lapply(chamber_met, vars_func)  
chams_met2 <- rbind.fill(chams_met)

library(plantecophys)
##use ecophys to conver rh to vpd
chams_met2$VPD <- RHtoVPD(chams_met2$RH_al, chams_met2$Tair_al)

###function to keep PPFD-----------------------------------------------------------------------------------------------

ppfd_fun <- function(x) {
  
  dat<- x[ , c("DateTime", "PAR")]
  
  dat$PAR <- ifelse(dat$PAR < 0, 0, dat$PAR)
  dat$ppfd_mol <- dat$PAR/1000000
  dat$PPFD15_mol_s <- dat$ppfd_mol*15*60
  
  dat$Timestamp <- ymd_hms(dat$DateTime)
  dat$Date <- as.Date(dat$Timestamp)
  
  dat2 <- dat[,c("DateTime", "Date", "PPFD15_mol_s")]
  return(dat2)
}

outside_ppfd <- lapply(outside_met, ppfd_fun)  
chams_ppfd2 <- rbind.fill(outside_ppfd)


###get total daily par for each chamber and outside chamber and then save as a dataframe

PPFD_outside <- summaryBy(PPFD15_mol_s~Date, data=chams_ppfd2, FUN=sum, keep.names=TRUE)
names(PPFD_outside)[2] <- "PPFD_day"

PPFD_chamber <- summaryBy(PPFD15_mol_s~Date+chamber, data=chams_met2, FUN=sum, keep.names=TRUE)
names(PPFD_chamber)[3] <- "PPFD_day"

#with(PPFD_outside, plot(Date, PPFD15_mol_s, type = "l"))
with(PPFD_chamber, plot(Date, PPFD15_mol_s, col=chamber))

write.csv(PPFD_outside, "calculated_data/PPFD_outside.csv", row.names=FALSE)
write.csv(PPFD_chamber, "calculated_data/PPFD_chamber.csv", row.names=FALSE)


###air temp, and VPD inside chamber
met_chamber <- summaryBy(VPD+Tair_al~Date+chamber, data=chams_met2, FUN=c(min, max, mean))

write.csv(met_chamber, "calculated_data/met_chamber.csv", row.names=FALSE)


with(met_chamber, plot(Date, Tair_al.max, type = "l", col="red"))   
with(met_chamber, points(Date, Tair_al.min, type = "l", col="blue")) 


