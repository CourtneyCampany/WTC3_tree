
##func
sunshadeplot_func <- function(v1, v2, treatment, xlab, ylab){
  
  
  
  
  ##run linear model on two variables for sun and shade
  sun_lm <- lm(v1 ~ v2,  subset=treatment=="sun-high")
  sha_lm <- lm(v1 ~ v2,  subset=treatment=="shade-low")
  
  ##predict
  
  #vector of v2 data for sun and shade
  sundat <- dfr[dfr$leaflight=="sun-high", v2]
  shadat <- dfr[dfr$leaflight=="shadelow", v2]
  #predict v2 sequence for sun and shade
  sun_seq <- seq(min(sundat), max(sundat), length=101)
  sha_seq <- seq(min(shadat), max(shadat), length=101)
  #predict sun shade CI
  sun_pred <- predict.lm(sun_lm, newdata=data.frame(v2=sun_seq), interval="confidence")
  sha_pred <- predict.lm(sha_lm, newdata=data.frame(v2=sha_seq), interval="confidence")
  
  #plot objects
  library(scales)
  suncol <- alpha("forestgreen", alpha=.75)
  shacol <- alpha("yellow4", alpha=.75)
  leafcol <- c(suncol, shacol)
  leaflab2 <- c("Sun", "Shade")
  ypos <- c(2.5,1,0)
  
  #plot
  
  #sun  
  plot(v1~v2, data=dfr, subset=leaflight=="sun-high", pch=16, col=suncol, ylim=c(0,(max(dfr$v2)*1.25)), 
       xlim=c(0,(max(dfr$v1)*1.25)), xlab=xlab, ylab="", cex=1.25)
  
  ablineclip(sun_lm, lty=1, x1=min(dfr[dfr$leaf=="sun",v2]), x2=max(gm_c13[gm_c13$leaf=="sun",v2]), 
             col="forestgreen", lwd=2)
  lines(sun_seq, sun_pred[,2], lty=2, lwd=2, col="forestgreen")
  lines(sun_seq, sun_pred[,3], lty=2, lwd=2, col="forestgreen")
  #shade
  points(v1~v2, data=dfr, subset=leaflight=="shade-low", pch=16, col=shacol, cex=1.25)
  
  ablineclip(sha_lm, lty=1, x1=min(dfr[dfr$leaflight=="shade-low",v2]), 
             x2=max(dfr[dfr$leaflight=="shade-low",v2]), col="yellow4", lwd=2)
  lines(sha_seq, sha_pred[,2], lty=2, lwd=2,col="yellow4")
  lines(sha_seq, sha_pred[,3], lty=2, lwd=2,col="yellow4")
  
  title(ylab=ylab, mgp=ypos, cex=1.2)
  legend("topleft", leaflab2, pch=16,inset = 0.03, col=leafcol) 
  
}

sunshadeplot_func(gm_c13, gm_c13$Photo, gm_c13$ci_bar, cibarlab, satlab)


####TESTING

sunshadeplot_func <- function(v1, v2, treatment, data){
  
  data$v1 <- eval(substitute(v1), data)
  data$v2 <- eval(substitute(v2), data)
  data$treatment <- eval(substitute(treatment), data)

  ##run linear model on two variables for sun and shade
  sun_lm <- lm(v1 ~ v2,  subset=subset(data, treatment=="sun-high"), data=data)
  sha_lm <- lm(v1 ~ v2,  subset=subset(data, treatment=="shade-low"), data=data)
  
  #vector of v2 data for sun and shade
  v2trt <- data.frame(cbind(v2, treatment))
  
  
   sunv2 <- v2trt[v2trt$treatment == "sun-high", v2]
   shav2 <- v2trt[v2trt$treatment == "shade-low", v2]
  return(shav2)
  
  #predict v2 sequence for sun and shade
  sun_seq <- seq(min(sunv2), max(sunv2), length=101)
  sha_seq <- seq(min(shav2), max(shav2), length=101)
  
  #predict sun shade CI
  sun_pred <- predict.lm(sun_lm, newdata=data.frame(v2=sun_seq), interval="confidence")
  sha_pred <- predict.lm(sha_lm, newdata=data.frame(v2=sha_seq), interval="confidence")
  
  
return(v2trt)
}


a <-sunshadeplot_func(Photo, gm, leaflight, data=gm_c13)




