library(scales)

#plot objects----------------------------------------------------
pchs <- c(1, 16)
pch4 <- c(16,16, 1, 1)
pchtri <- c(17,17,2,2)


gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
gmlab2 <-expression(italic(g)[m]~~(mol~m^-2~s^-1~bar^-1))
shagmlab <- expression(italic(g)[m]~Low~Light~~(mol~m^-2~s^-1~bar^-1))
fleckgmlab <- expression(italic(g)[m]~High~Light~~(mol~m^-2~s^-1~bar^-1))

lmalab <- expression(LMA~~(g~m^-2))
masslab <- "Leaf Mass  (g)"

arealab <- expression(Leaf~area~~(cm^2))
narealab <- expression(italic(N)[a]~~(gN~m^-2))
nsunlab <- expression(Sun~leaf~italic(N)[a]~~(gN~m^-2))
nshadelab <- expression(Shade~leaf~italic(N)[a]~~(gN~m^-2))

ksunlab <- expression(Leaf[sun]~Hydraulic~Conductance~~(units))
kshadelab <- expression(Leaf[shade]~Hydraulic~Conductance~~(units))


c13lab <-expression(paste(Leaf~delta^{13}, "C  (\u2030)"))
ypos <- c(2.5,1,0)
satlab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
vclab <- expression(italic(V)[cmax]~~(mu*mol~m^-2~s^-1))

condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))

vpdlab <- "VPD  (kPa)"
itelab <- expression(ITE~~(mmol~CO[2]~mol~H[2]*O^-1))

ratelab <- expression(mol~m^-2~s^-1)
parlab <- expression(PPFD~~(mu*mol~m^-2~s^-1))
photolab <- expression(italic(A[n])~~(mu*mol~m^-2~s^-1))
amaxlab <-expression(A[max]~~(mu*mol~m^-2~s^-1))
trmmollab <- expression(italic(E)[L]~~(mmol~m^-2~s^-1))

cibarlab <- expression(bar(Ci)[shade-sun]~~(ppm))
cibarlab2 <- expression(bar(Ci)~~(ppm))
cilab3 <- expression({Ci^i}[shade-sun]~~(ppm))
cilab <- expression(italic(C)[i]~~(ppm))

cclab <- expression(italic(C)[c]~~(ppm))
cclab2 <- expression(bar(italic(C)[c])~~(ppm))

deltalab <-(expression(Delta[o]))  
cicalab <- "Ci/Ca"
cicalab2 <- (expression(italic(C)[i]/italic(C)[a]))

drawdownlab <- expression(italic(C)[i]-italic(C)[c]~~(mu*mol~mol^-1))
drawdownlab2 <- expression(italic(C)[a]-italic(C)[i]~~(mu*mol~mol^-1))
drawdownlab3 <- expression(italic(C)[i]-italic(C)[c]~~(mu*mol~mol^-1))

relparlab <- expression(l[shade]:l[sun])
relnitrolab <- expression(N[shade]:N[sun])
relklab <- expression(K[shade]:K[sun])

leaftlab2 <- ("Leaf Temperature  (C)")
leaftlab = expression(paste("Temperature  (",degree,"C)")) 
airtlab <- expression(paste("Daily Max Ambient Air Temperature  (",degree,"C)")) 

templab <- c("AT", "ET")
leaflab <- c("shade", "sun")

###legends--------------------------------------------------------------------------------------------------
leglab <-  c(expression(paste(AT, " " ,"sun")), expression(paste(ET, " " ,"sun")),
           expression(paste(AT," " ,"shade")), expression(paste(ET, " " ,"shade")))

leglab2<- c(expression(paste(Sun, "-" ,"AT")), expression(paste(Sun, "-" ,"ET")),
            expression(paste(Shade,"-" ,"AT")), expression(paste(Shade, "-" ,"ET")))

leglab3<- c(expression(paste(Sun, "-" ,"AT")), expression(paste(Sun, "-" ,"ET")),
            expression(paste(Shade,"-" ,"AT")), expression(paste(Shade, "-" ,"ET")),
            expression(paste(Sunfleck,"-" ,"AT")), expression(paste(Sunfleck, "-" ,"ET")))

acileg <-  c(expression(paste(Sun, "-" ,"AT")), expression(paste(Sun, "-" ,"ET")),
             expression(paste(Shade~High~Light,"-" ,"AT")), expression(paste(Shade~High~Light, "-" ,"ET")))


ltys <- c(1,2, 1,2)

###sun shade colors-------------------------------------------------------------------------------------------

#new colors to respond to editors comments
# suncol <- alpha("#3B3A3A",0.75) #rgb(230,159,0,0.75*255,max=255)   #alpha("forestgreen", alpha=.75)
# shacol <- newshade <- newshacol <-  alpha("#009E73", 0.75) 
# #rgb(0,114,178,0.75*255,max=255)      #alpha("yellow4", alpha=.75)
# lightscol <- alpha("#117CC2", 0.75) #rgb(86,180,233,0.75*255,max=255)     #alpha("darkorange2", .75)

#remkos

#d95f02-sun
#1b9e77-shl

#shadelow: #377eb8
#shaehigh: #4daf4a


suncol <- alpha("#ff7f00",0.85) #rgb(230,159,0,0.75*255,max=255)   #alpha("forestgreen", alpha=.75)
shacol <- newshade <- newshacol <-  alpha("#377eb8", 0.85) #rgb(0,114,178,0.75*255,max=255)  #alpha("yellow4", alpha=.75)
lightscol <- alpha("#47a244", 0.85) #rgb(86,180,233,0.75*255,max=255) #alpha("darkorange2", .75)

suncol2 <- "#ff7f00"
lightscol2 <- "#47a244"
shacol2 <- "#377eb8"
#shacol2 <- "#235176"

shacol50 <- alpha(shacol, alpha=.5)
suncol50 <- alpha(suncol, alpha=.5)
lights50col <- alpha(lightscol2, alpha=.5)

leafcol <- c(suncol, shacol)
leafcol2 <- c(shacol, suncol)
leafcol3 <- c(lightscol, suncol)
leafcol4 <- c(lightscol2, suncol2)
leaflab2 <- c("Sun", "Shade")
collights<-c(lightscol, lightscol, shacol, shacol)
trtcols <-c(suncol, suncol, shacol, shacol)

###color combos for legend etc.

colaci <-c(suncol2, suncol2, lightscol2, lightscol2)
colaci2 <-c( shacol, shacol,suncol, suncol)
colaci3 <-c( newshade, newshade,suncol, suncol)
leafcols <- c(suncol, suncol, shacol, shacol, lightscol, lightscol)
lightlab <- c(lightscol,shacol)

###lights labels
leaflightlab <- c("shade-high", "shade-low", "sun")
lightleg <- c("High light", "Low light")

lightleg2 <- c(expression(paste("High light", "-" ,"AT")), expression(paste("High light", "-" ,"ET")),
  expression(paste("Low light","-" ,"AT")), expression(paste("Low light", "-" ,"ET")))

lightleg3 <- c(expression(paste("Sun light", "-" ,"AT")), expression(paste("Sun light", "-" ,"ET")),
               expression(paste("Shade light","-" ,"AT")), expression(paste("Shade light", "-" ,"ET")))

leaflightlab <- c("shade-high", "shade-low", "sun")

sunflecklab <- "Shade \n High Light"


alllab <- c("Sun", "Shade-Low Light", "Shade-High Light", "AT", "ET")
leaflightlab2 <- c("Sun", "Shade-Low Light", "Shade-High Light")
allcols=c(suncol, shacol, lightscol2, "black", "black")
allcols2=c(suncol2, newshade, lightscol2, "black", "black")



###DAte objects
octdates <- data.frame(campaign = 1, Date = as.Date(c("2013-10-26","2013-10-25","2013-10-26")))
decdates <- data.frame(campaign = 2, Date = as.Date(c("2013-12-09","2013-12-10","2013-12-11")))
jandates <- data.frame(campaign = 3, Date = as.Date(c("2014-01-28","2014-01-29","2014-01-30")))
febdates <- data.frame(campaign = 4, Date = as.Date(c("2014-02-24","2014-02-25","2014-02-26")))
mardates <- data.frame(campaign = 5, Date = as.Date(c("2014-03-19","2014-03-21","2014-03-30")))
aprdates <- data.frame(campaign = 6, Date = as.Date(c("2014-04-21","2014-04-22","2014-04-23","2014-04-24")))

campaigndates <- rbind(octdates, decdates, jandates, mardates, aprdates)
campaigndates_simple <- as.Date(c("2013-10-26","2013-10-25","2013-10-26", "2013-12-09","2013-12-10","2013-12-11","2014-01-28",
                                  "2014-01-29","2014-01-30","2014-02-24","2014-02-25","2014-02-26","2014-03-19","2014-03-21",
                                  "2014-03-30","2014-04-21","2014-04-22","2014-04-23","2014-04-24"))

octdate <- as.Date("2013-10-25")
decdate <- as.Date("2013-12-10")
jandate <- as.Date("2014-01-28")
febdate <- as.Date("2014-02-24")
mardate <- as.Date("2014-03-19")
aprdate <- as.Date("2014-04-21")

campaigns <- c(octdate, decdate, jandate, febdate, mardate, aprdate)
campaigns2 <- data.frame(Month = c("Oct", "Dec", "Jan", "Feb", "Mar", "Apr"), Date = campaigns)

