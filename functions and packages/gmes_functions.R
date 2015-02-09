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
