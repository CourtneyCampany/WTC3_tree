library(scales)

#plot objects----------------------------------------------------
pchs <- c(1, 16)
pch4 <- c(16,16, 1, 1)
pchtri <- c(17,17,2,2)


gmlab <-expression(g[m]~~(mol~m^-2~s^-1))
lmalab <- expression(LMA~~(g~m^-2))
masslab <- "Leaf Mass  (g)"

arealab <- expression(Leaf~Area~~(cm^2))
narealab <- expression(Leaf~Nitrogen[area]~~(gN~m^-2))
nsunlab <- expression(Leaf[sun]~Nitrogen[area]~~(gN~m^-2))
nshadelab <- expression(Leaf[shade]~Nitrogen[area]~~(gN~m^-2))

ksunlab <- expression(Leaf[sun]~Hyrdaulic~Conductance~~(units))
kshadelab <- expression(Leaf[shade]~Hyrdaulic~Conductance~~(units))


c13lab <-expression(paste(Leaf~delta^{13}, "C  (\u2030)"))
ypos <- c(2.5,1,0)
satlab <- expression(A~~(mu*mol~m^-2~s^-1))
vclab <- expression(Vc[max]~~(mu*mol~m^-2~s^-1))

condlab <- expression(g[s]~~(mol~m^-2~s^-1))
totcondlab <- expression(Total~Conductance~to~CO[2]~~(mol~m^-2~s^-1))

vpdlab <- "VPD  (kPa)"
itelab <- expression(ITE~~(mmol~CO[2]~mol~H[2]*O^-1))

ratelab <- expression(mol~m^-2~s^-1)
parlab <- expression(PPFD~~(mu*mol~m^-2~s^-1))
photolab <- expression(italic(A)~~(mu*mol~m^-2~s^-1))
trmmollab <- expression(Transpiration~~(mmol~H[2]*O~m^-2~s^-1))

cibarlab <- expression(bar(Ci)[shade-sun]~~(ppm))
cibarlab2 <- expression(bar(Ci)~~(ppm))
cilab3 <- expression({Ci^i}[shade-sun]~~(ppm))
cilab <- expression(C[i]~~(ppm))

cclab <- expression(C[c]~~(ppm))
cclab2 <- expression(bar(C[c])~~(ppm))

deltalab <-(expression(Delta[obs]))  
cicalab <- "Ci/Ca"

drawdownlab <- expression(paste(CO[2]~drawdown,", ", C[i]-C[c]))
drawdownlab2 <- expression(paste(CO[2]~drawdown,", ", C[a]-C[i]))

relparlab <- expression(l[shade]:l[sun])
relnitrolab <- expression(N[shade]:N[sun])
relklab <- expression(K[shade]:K[sun])

leaftlab2 <- ("Leaf Temperature  (C)")
leaftlab = expression(paste("Temperature  (",degree,"C)")) 
airtlab <- expression(paste("Max Daily Ambient Air Temperature  (",degree,"C)")) 

templab <- c("AT", "ET")
leaflab <- c("shade", "sun")
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

###sun shade colors
suncol2 <- "#005300"
shacol2 <- "#61726C"
lightscol2 <- "#E68A00"
newshade <- ("#613E1C")

suncol <- alpha(suncol2, alpha=.75)
shacol <- alpha(shacol2 , alpha=.75)
newshacol <- alpha(newshade, alpha=.75)

lightscol <- alpha(lightscol2, .75)


shacol50 <- alpha(shacol, alpha=.5)
suncol50 <- alpha(suncol, alpha=.5)
lights50col <- alpha(lightscol2, alpha=.5)


leafcol <- c(suncol, shacol)
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
