###function to create a unique sample id from user specific variable column names-----------------------------------

chooseidfunc <- function(dfr, varnames, sep="-"){
  dfr$id <- as.factor(apply(dfr[,varnames], 1, function(x)paste(x, collapse=sep)))
  return(dfr)
}

####licor formating function------------------------------------------------------------------------------------------
licorformat_func <- function(x){
  
  licorfirst<- which(colnames(x) == "Obs")
  licorlast<- which(colnames(x) == "AHs.Cs")
  
  x$calendar <- strptime(x$Date, format = "%d/%m/%Y", tz="UTC")
  x$calendar <- as.character(x$Date)
  x$clock <- as.character(x$HHMMSS)
  x$Date2 <- paste(x$calendar, x$clock, sep=" ")
  x$datetime<- strptime(x$Date2, format = "%d/%m/%Y  %H:%M:%S",  tz="UTC")
  dtcol <- which(colnames(x) == "datetime")
  licorcol <- which(colnames(x) == "licor")
  idcol <- which(colnames(x) == "id")
  
  #subset dataset to keep licor values, id, and datetime
  dfr <- x[,c(licorfirst:licorlast, licorcol, idcol, dtcol)]
  dfr$O2 <- 21
  #sort the dfr to put o2 in the right place
  idcol2 <- which(colnames(dfr) == "id") 
  dtcol2 <- which(colnames(dfr) == "datetime")
  ocol <- which(colnames(dfr) == "O2")
  licorcol2 <- which(colnames(dfr) == "licor")
  licorfirst2<- which(colnames(dfr) == "Obs")
  licorlast2<- which(colnames(dfr) == "AHs.Cs")
  beforeO<- which(colnames(dfr) == "FTime")
  afterO<- which(colnames(dfr) == "EBal.")
  
  dfr <- dfr[, c(licorfirst2:beforeO,ocol, afterO:licorlast2, licorcol2, idcol2, dtcol2 )]
  return(dfr)
}

####function to return unique max and min dates for each tree id----------------------------------------------------

timerange_func <- function(x, dfr){
  
  x_sp <- split(x, x$id)
  dfr<- lapply(x_sp, function(x){
    max.dt <- max(x$datetime)
    min.dt <- min(x$datetime)
    id<- unique(x$id)
    newdfr <- data.frame(min=min.dt, max=max.dt,id=id)
    
  })
  #below needs package 'data.table'
  times <- rbindlist(dfr)
  times2 <- as.data.frame(times)
}
#tdl formatting function-----------------------------------------------------------------------------------------------

tdlformat_func <- function(x){
  x$calendar <- strptime(x$TIMESTAMP, format = "%d/%m/%Y",  tz="UTC")
  x$calendar <- as.character(x$calendar)
  x$clock <- as.character(x$TIME)
  x$Date <- paste(x$calendar, x$clock, sep=" ")
  x$datetime <- strptime(x$Date,  format = "%Y-%m-%d  %H:%M:%S",  tz="UTC")
  
  dfr <- x[,c("SiteOutput", "CorrConcA_Avg","CorrConcB_Avg",  "Corrdel13C_Avg", "datetime", "licor")]
  
  return(dfr)
}

###xsi calulcation for csv with tdl data-------------------------------------------------------------------------------
xsicalc_func <- function(x){
  xsi_dfr <- x[x$SiteOutput != c(3,4),]
  #give unique to ref and sample lines using odd/even tdl line #s
  xsi_dfr$line_id <- ifelse(xsi_dfr$SiteOutput %% 2 == 1,"a", "b")
  xsi_dfr$CO2_total <- (xsi_dfr$CorrConcA_Avg+xsi_dfr$CorrConcB_Avg)/(1-0.00474)
  #seperate ref and sample lines for calculations
  xsi_a <- xsi_dfr[xsi_dfr$line_id=="a",]
  colnames(xsi_a)[(names(xsi_a) == "Corrdel13C_Avg")] <- "del13_ref"
  colnames(xsi_a)[(names(xsi_a) == "CO2_total")] <- "CO2_total_ref"
  
  xsi_b <- xsi_dfr[xsi_dfr$line_id=="b",]
  colnames(xsi_b)[(names(xsi_b) == "Corrdel13C_Avg")] <- "del13_samp"
  colnames(xsi_b)[(names(xsi_b) == "CO2_total")] <- "CO2_total_samp"
  
  #new dfr with xsi, deltadiff, DELTA, and timestamp for matching
  deltadiff<- xsi_b$del13_samp - xsi_a$del13_ref
  xsi <- xsi_b$CO2_total_samp/(xsi_a$CO2_total_ref - xsi_b$CO2_total_samp)
  
  xsi_calc <-data.frame(cbind(deltadiff, xsi))
  xsi_calc$DELTA <- (1000 * xsi_calc$xsi * xsi_calc$deltadiff)/(1000+xsi_b$del13_samp-(xsi_calc$xsi*xsi_calc$deltadiff))
  xsi_calc$timeavg <- xsi_a$datetime-((xsi_a$datetime - xsi_b$datetime)/2)
  xsi_calc$licor <- xsi_dfr$licor[1]
  
  return(xsi_calc)
}
