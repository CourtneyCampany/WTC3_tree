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
    licor=x$licor
    newdfr <- data.frame(min=min.dt, max=max.dt,id=id, licor=licor)
      })
    #below needs package 'data.table'
    times <- data.table::rbindlist(dfr)
    times2  <- unique(as.data.frame(times))
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
    deltadiff<- xsi_b$del13_sampxsi_a$del13_ref
    xsi <- xsi_b$CO2_total_samp/(xsi_a$CO2_total_refxsi_b$CO2_total_samp)
                
    xsi_calc <-data.frame(cbind(deltadiff, xsi))
        xsi_calc$DELTA <- (1000 * xsi_calc$xsi * xsi_calc$deltadiff)/(1000+xsi_b$del13_samp-(xsi_calc$xsi*xsi_calc$deltadiff))
        xsi_calc$timeavg <- xsi_a$datetime-((xsi_a$datetimexsi_b$datetime)/2)
        xsi_calc$licor <- xsi_dfr$licor[1]
                  
     return(xsi_calc)
      }

####gmes_func---------------------------------------------------------------------------------------------------------
gmesdata_func <- function(xsi_dfr, licor_dfr, times_dfr, licorrows=5, whichlicor="f2" ){
        
    ###subset licor_dfr by licor used
    licor_dfr2 <- licor_dfr[licor_dfr$licor == whichlicor,]
    licor_dfr2 <- droplevels(licor_dfr2)
    ###subset times_dfr by licor used
    times_dfr2 <- times_dfr[times_dfr$licor == whichlicor,] 
    times_dfr2 <- droplevels(times_dfr2)
            
    ###use times_dfr to add "sample id" to xsi dfr based datetime interval
    xsi_dfr$id <- "no_sample"
              
    for(i in 1:nrow(times_dfr)){
                    
    j <- which(xsi_dfr$timeavg >= times_dfr2$min[i] & xsi_dfr$timeavg <= times_dfr2$max[i])
    xsi_dfr$id[j] <- as.character(times_dfr2$id[i])
    }
              
    xsi_samples <- xsi_dfr[xsi_dfr$id != "no_sample",]
    #if(any(xsi_samples$xsi >20)) warning ("You have XSI values over the acceptable limit")
                  
    ###add closest matched licor time "datetime_match" to xsi_dfr with timematch func (min diff)
    ii<- sapply(1:nrow(xsi_samples), function(i)which.min(timematch(xsi_samples$timeavg[i], licor_dfr2$datetime)))
    xsi_samples$datetime_match <- licor_dfr2$datetime[ii]
                  
    thisrow <- as.vector(ii) 
    delr <- (licorrows-1)/2
                    
    licor_ids <- lapply(1:length(thisrow), function(k) {
    id_split <- licor_dfr2[(thisrow[k]-delr):(thisrow[k]+delr),]
    })
                      
    ##in each list if # of unique ids isgreater > 1 then null, null will disappear during rbind
    deleteDoubles <- function(l, varname="id"){
                            
    ii <- which(sapply(l, function(el)length(unique(el[,varname])) > 1))
    l[ii] <- NULL
    return(l)
    }
                        
    licor_ids2 <- deleteDoubles(licor_ids)
                          
    ###mean of each list, convert to dfr, cbind with xsi&DELTA from xsi_dfr (nrow must be equal)
    licor_ids_datefix <- llply(licor_ids2, function(x) c(x$datetime <- as.numeric(x$datetime), return(x)))
    licor_ids_agg <- llply(licor_ids_datefix,function(x) summaryBy(.~id+licor, data=x, FUN=mean, keep.names=TRUE))
    licor_agg_dtfix <- llply(licor_ids_agg, function(x) c(x$datetime <- as.POSIXct(x$datetime,
                                                                                                                                                                                             origin= '1970-01-01', tz="UTC"), return(x)))
                            
    licor_agg<- rbind.fill(licor_agg_dtfix)
                              
    ###now there is the possibility that there will be less licor values than xsi values, 
    ###must rematch closest time row and then add that rows xsi and delta to new dfr
                                
    dd<- sapply(1:nrow(licor_agg), function(i)which.min(timematch(licor_agg$datetime[i], xsi_samples$datetime_match)))
    licor_agg$xsi <- xsi_samples$xsi[dd]
    licor_agg$DELTA <- xsi_samples$DELTA[dd]
    return(licor_agg)
                                
    }

###time match function to find minimum difference between two sets of datetimes------------------------------------
timematch <- function(time1, time2)abs(time1-time2)
  
###gmes function------------------------------------------------------------------------------
gmcalc_func <- function(x, a=4.4, ab= 2.9, b=29, f=16.2,del_growth = -8 , delR=-5, 
                       k25r=0.728, k25g=38.89, Ea_r = 72.311, Ea_g = 20.437,Rgc=8.314472){

      e = delRdel_growth
            
      x$CiCa <- x$Ci/x$CO2R
      x$a_prime <- (ab*(x$CO2S-x$C2sfc)+a*(x$C2sfc-x$Ci))/(x$CO2S-x$Ci)
              
      x$Rd <- k25r * exp(Ea_r*((x$Tleaf+273.15)-298)/(298*Rgc*(x$Tleaf+273.15)))
      x$Gstar <- k25g * exp(Ea_g*((x$Tleaf+273.15)-298)/(298*Rgc*(x$Tleaf+273.15)))
                
      x$rd_term <- e*x$Rd*(x$Ci-x$Gstar)/((x$Photo+x$Rd)*x$CO2S)
      x$f_term <- f*x$Gstar/x$CO2S
                  
      x$TleafminusTair <- x$Tleafx$Tair
      x$TblockminusTair <- x$TBlkx$Tair
                    
      x$CO2Rdry <- x$CO2R/(1-x$H2OR/1000)
      x$CO2Sdry <- x$CO2S/(1-x$H2OS/1000)
                      
      x$t <- (1+x$a_prime/1000)*x$Trmmol/x$CndCO2/1000/2
      x$t2 <- 1/(1-x$t)
      x$t3 <- (1+x$t)/(1-x$t)
                        
      x$Di <- x$a_prime * x$t2+ (x$t3 * b-x$a_prime * x$t2) * x$CiCa
      x$DiminusDo <- x$Dix$DELTA
                          
      x$rd_term2 <- x$t3- x$rd_term
      x$f_term2 <- x$t3x$f_term
                            
      x$gm <- x$t3 * (b1.8x$Rd * e / (x$Rd+x$Photo)) * x$Photo/x$CO2S/(x$DiminusDox$rd_term2x$f_term2)
      x$gm_bar <- x$gm*100/x$Press
                              
      #different fractionation components as outputs--------------------------------------------------------------------
                              
      #fractionation where Ci=Cc in the absence ot respiratory fractionation
      x$delta_i <- (x$t2*x$a_prime)+(x$t2*((1+x$t)*b-x$a_prime)*x$CiCa)
                                
      #fractionation associated with the diffusion of CO2 from intercellular airspace to chloroplast 
      x$delta_gm = x$t3*(bx$a_prime(e*x$Rd)/(x$Photo+x$Rd)) * (x$Photo/(x$gm * x$CO2R))
                                  
      #most of the fractionation associated with respiration
      x$delta_e <- x$t3 * (((e * x$Rd)/((x$Photo + x$Rd) * x$CO2R))*(x$Cix$Gstar))
                                    
      #fractionation associated with photorespiration
      x$delta_f <- x$t3 * (f * (x$Gstar/x$CO2R))
                                      
      return(x)
      }

###extract 13C of refernce gas from cylinder to use as delR in gmes equation----------------------------------------------------
      
tank13 <- function(x){ #run on tdl formatted lists
      #remove reference gases
      dat <- x[x$SiteOutput != 3 & x$SiteOutput != 4, c("SiteOutput","Corrdel13C_Avg")]
      #subset odd gas lines as they are the first reference line from cylinder
      is.odd <- function(v) v %% 2 != 0
      dat2 <- dat[which(is.odd(dat$SiteOutput)),]
      #caluclate mean corr del from gmes eq
      dat3 <- mean(dat2$Corrdel13C_Avg)
      dat3 <- as.data.frame(dat3)
      return(dat3)
      }