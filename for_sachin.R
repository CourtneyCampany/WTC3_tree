dat <- read.csv("sachin.csv")

str(dat)
##from this you can see that Date is a factor so we need to change it a date object
## we can use the lubridate packake to accomplish this very easy
##you should also learn as.Date as.POSIXct but this is way better

library(lubridate)

##first look at the format of the actual date data and determine where things are (month, day, year hour, minute, second)

head(dat)

##then turn Date into a Date object using lubridate
dat$Date <- dmy_hms(dat$Date,  tz="UTC")

##check to see if it worked
str(dat)
##very important to notice that R took your Date (mdy_hms) and converted it into the standard format for R which is ymd_hms (POSIXct)

###i use as.Date to extract YMD as the numer of days since the origin ("1970-01-01") for later use
dat$Date2 <- as.numeric(as.Date(dat$Date, format = "%Y/%m/%d", tz="UTC"))

##lets make a new column that we will use for plotting
##we will eventually want to plot data by hour so lets work for that

##extract the day of the year (1-365)
dat$day <- yday(dat$Date)
##then hour
dat$hour <- hour(dat$Date)

##paste these two together as your 
dat$unique_time <- with(dat, paste(day, hour, sep = "-"))


####means by hour-------------------------------------------------------------------------------------------------------------

##then take the mean by your unique variable
library(doBy)
dat_agg <- summaryBy(Date2 + hour + Temp ~ unique_time, data=dat, FUN=c(mean), keep.names = TRUE)

##now lets convert that date as a number back to Date
dat_agg$calendar <- as.Date(dat_agg$Date2,  origin = "1970-01-01")

dat_agg$calendar2 <- with(dat_agg, paste(calendar, hour, sep=" "))
str(dat_agg)

dat_agg$calendar2 <- ymd_h(dat_agg$calendar2)

####plot----------------------------------------------------------------------------------------------------------------------

###order the Date column (calendar2) so that the line argument works properly
dat_agg <- dat_agg[order(dat_agg$calendar2),]

plot(Temp~calendar2, data=dat_agg)

plot(Temp~calendar2, data=dat_agg, type="l")





