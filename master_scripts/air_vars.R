 source("functions and packages/packages.R")
 source("functions and packages/functions.R")
 source("master_scripts/plot_objects.R")
library(lubridate)
 
####convert 1min PPFD and temperature readings into plot for manuscript 
 
treatments <- read.csv("raw data/temp_trt.csv")

### load data that has been downloaded into met data folder
met_names<- list.files(path="wtc_met/",pattern="Table2",full.names=TRUE)

met_names2 <- gsub("wtc_met/", "", met_names)
met_names2 <- gsub(".dat", "", met_names2)
met_names2 <- gsub("_Table2", "", met_names2)
met_names2 <- gsub("WTC", "ch", met_names2)

cham <- substr(met_names2, 0,4)

met_files <- llply(list.files(path="wtc_met/",pattern="Table2",full.names=TRUE),function(filename) {
  dat=read.csv(filename, header=TRUE)
})


#actual file names
met_data <- setNames(met_files, met_names2)

met_fun <- function(x) {
dat<- x[ -c(1:4), c(1,3:4)]
 names(dat)[1:3] <- c("Timestamp", "PPFD", "Temperature")
 dat$PPFD <- as.numeric(as.character(dat$PPFD))
 dat$Temperature <- as.numeric(as.character(dat$Temperature))
 dat$PPFD <- ifelse(dat$PPFD < 0, 0, dat$PPFD)
 dat$ppfd_mol <- dat$PPFD/1000000
 dat$ppfd_mol_min <- dat$ppfd_mol*60
 
 dat$Timestamp <- ymd_hms(dat$Timestamp)
 dat$Date <- as.Date(dat$Timestamp)
 
 dat2 <- dat[, c(1, 3, 5:6)]
return(dat2)
}

# test <- met_data[[5]]
# test2 <- met_fun(test)

met_data <- lapply(met_data,  met_fun)

###add chamber
for (i in 1:134){
  met_data[[i]]$chamber <- cham[i]
}

##make one big dataframe and add treatments
met_data_all <- rbind.fill(met_data)
met_data_all <- merge(met_data_all, treatments)

###summary of PPFD and temp data 

PPFD_day <- summaryBy(ppfd_mol_min~Date+chamber+temp, data=met_data_all, FUN=sum, keep.names=TRUE)
PPFD_day$chamber <- as.factor(PPFD_day$chamber)

temp_day <- summaryBy(Temperature~Date+chamber+temp, data=met_data_all, FUN=c(mean, max, min))
temp_day$chamber <- as.factor(temp_day$chamber)

###PPFD PLOTs------------------------------------------------------------------------------------------------------------

plot(ppfd_mol_min ~Date, data=PPFD_day, col=chamber)

clean_PPFD <- PPFD_day[PPFD_day$chamber != "ch03",]

plot(ppfd_mol_min ~Date, data=clean_PPFD, col=chamber, pch=16)

with(clean_PPFD[clean_PPFD$chamber == "ch01",], plot(Date, ppfd_mol_min, type = "l", col="red"))
  points(ppfd_mol_min~Date, type = "l", col="blue", data=clean_PPFD[clean_PPFD$chamber == "ch02",])


##plotting of PPFD
palette(c("black", "red"))
plot(ppfd_mol_min ~Date, data=PPFD_day[PPFD_day$chamber!= "ch03",], col=temp, pch=16)

plot(ppfd_mol_min ~Date, data=clean_PPFD, type='n')
palette(rainbow(12))
for(i in 1:length(unique(clean_PPFD$chamber))){
  points(ppfd_mol_min ~Date, data=clean_PPFD, col=palette(), type="l")
}

###TEMP PLOTS------------------------------------------------------------------------------------------------------------

palette(c("black", "red"))
with(temp_day, plot(Date, Temperature.max, col=temp, ylim=c(-5, 50)))

points(Temperature.min ~Date, data=temp_day, col=temp)

