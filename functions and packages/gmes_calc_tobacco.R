####gmescalculation----------------------------------------------------------------------------------------------
gmcalc_tob_func <- function(x, a=4.4, ab= 2.9, e=30, b=29, f=16.2,del_growth = -8 , delR=-38, 
                        k25r=0.718, k25g=38.89, Ea_r = 46.39, Ea_g = 24.46,Rgc=8.314472, c_r=18.72, c_g=13.49){
  
  x$CiCa <- x$Ci/x$CO2R
  x$a_prime <- (ab*(x$CO2S-x$C2sfc)+a*(x$C2sfc-x$Ci))/(x$CO2S-x$Ci)
  
  x$Rd <- k25r * exp(c_r - Ea_r* 1000 / (Rgc*(273 + x$Tleaf)))
  x$Gstar <- exp(c_g - Ea_g * 1000 / (Rgc*(273 + x$Tleaf))) * x$O2/21
  
#   x$Rd <- k25r * exp(Ea_r*((x$Tleaf+273.15)-298)/(298*Rgc*(x$Tleaf+273.15)))
#   x$Gstar <- k25g * exp(Ea_g*((x$Tleaf+273.15)-298)/(298*Rgc*(x$Tleaf+273.15)))
  
  x$rd_term <- e*x$Rd*(x$Ci-x$Gstar)/((x$Photo+x$Rd)*x$CO2S)
  x$f_term <- f*x$Gstar/x$CO2S
  
  x$TleafminusTair <- x$Tleaf - x$Tair
  x$TblockminusTair <- x$TBlk - x$Tair
  
  x$CO2Rdry <- x$CO2R/(1-x$H2OR/1000)
  x$CO2Sdry <- x$CO2S/(1-x$H2OS/1000)
  
  x$t <- (1+x$a_prime/1000)*x$Trmmol/x$CndCO2/1000/2
  x$t2 <- 1/(1-x$t)
  x$t3 <- (1+x$t)/(1-x$t)
  
  x$Di <- x$a_prime * x$t2+ (x$t3 * b-x$a_prime * x$t2) * x$CiCa
  x$DiminusDo <- x$Di - x$DELTA
  
  x$rd_term2 <- x$t3- x$rd_term
  x$f_term2 <- x$t3 - x$f_term
  
  x$gm <- x$t3 * (b - 1.8 - x$Rd * e / (x$Rd+x$Photo)) * x$Photo/x$CO2S/(x$DiminusDo - x$rd_term2 - x$f_term2)
  x$gm_bar <- x$gm*100/x$Press
  return(x)
}