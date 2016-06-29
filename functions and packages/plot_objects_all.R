library(scales)
library(wesanderson)

#plot objects----------------------------------------------------
pchs <- c(1, 16)
pch4 <- c(16,16, 1, 1)
pchtri <- c(17,17,2,2)
cols <- c("blue","red")
col4 <- c("blue", "red", "blue", "red")


gmlab <-expression(g[m]~~(mol~m^-2~s^-1))
lmalab <- expression(LMA~~(g~m^-2))
masslab <- "Leaf Mass  (g)"

arealab <- expression(Leaf~Area~~(cm^2))
narealab <- expression(N[area]~~(gN~m^-2))
nsunlab <- expression(Leaf[sun]~Nitrogen[area]~~(gN~m^-2))
nshadelab <- expression(Leaf[shade]~Nitrogen[area]~~(gN~m^-2))

ksunlab <- expression(Leaf[sun]~Hyrdaulic~Conductance~~(units))
kshadelab <- expression(Leaf[shade]~Hyrdaulic~Conductance~~(units))

c13lab <-expression(paste(delta^{13}, "C (\u2030)"))
ypos <- c(2.5,1,0)
satlab <- expression(A~~(mu*mol~m^-2~s^-1))
condlab <- expression(g[s]~~(mol~m^-2~s^-1))
vpdlab <- "VPD  (kPa)"
ratelab <- expression(mol~m^-2~s^-1)
parlab <- expression(PPFD~~(mol~m^-2~s^-1))
photolab <- expression(italic(A)~~(mu*mol~m^-2~s^-1))

cibarlab <- expression(bar(Ci)[shade-sun]~~(ppm))
cibarlab2 <- expression(bar(Ci)~~(ppm))
cilab3 <- expression({Ci^i}[shade-sun]~~(ppm))
cilab <- expression(C[i]~~(ppm))

cclab <- expression(C[c]~~(ppm))
cclab2 <- expression(bar(C[c])~~(ppm))

relparlab <- expression(l[shade]:l[sun])
relnitrolab <- expression(N[shade]:N[sun])
relklab <- expression(K[shade]:K[sun])

leaftlab <- ("Leaf Temperature  (C)")

templab <- c("AT", "ET")
leaflab <- c("shade", "sun")
leglab <-  c(expression(paste(AT, " " ,"sun")), expression(paste(ET, " " ,"sun")),
             expression(paste(AT," " ,"shade")), expression(paste(ET, " " ,"shade")))

leglab2<- c(expression(paste(Sun, "-" ,"AT")), expression(paste(Sun, "-" ,"ET")),
            expression(paste(Shade,"-" ,"AT")), expression(paste(Shade, "-" ,"ET")))

###sun shade colors
suncol <- alpha(rgb(230,159,0,max=255),0.75)   #alpha("forestgreen", alpha=.75)
shacol <- alpha(rgb(0,114,178,max=255), 0.75)      #alpha("yellow4", alpha=.75)
leafcol <- c(suncol, shacol)
leaflab2 <- c("Sun", "Shade")

colaci <-c(suncol, suncol, shacol, shacol)

###lights on shade cols
leaflightlab <- c("shade-high", "shade-low", "sun")
lightscol <- alpha(rgb(230,159,0,max=255),0.75)     #alpha("darkorange2", .75)
lightlab <- c(lightscol,shacol)
lightleg <- c("High light", "Low light")


##plot bits-------------------------------------------------------------------
#gm labels
suatgm <- expression(g[m]~~Sun~AT~~(mol~m^-2~s^-1))
suetgm <- expression(g[m]~~Sun~ET~~(mol~m^-2~s^-1))
shatgm <- expression(g[m]~~Shade~AT~~(mol~m^-2~s^-1))
shetgm <- expression(g[m]~~Shade~ET~~(mol~m^-2~s^-1))

#cc labels
suatcc <- expression(Cc~~Sun~AT~~(ppm))
suetcc <- expression(Cc~~Sun~ET~~(ppm))
shatcc <- expression(Cc~~Shade~AT~~(ppm))
shetcc <- expression(Cc~~Shade~ET~~(ppm))

#ci labels
suatci <- expression(Ci~~Sun~AT~~(ppm))
suetci <- expression(Ci~~Sun~ET~~(ppm))
shatci <- expression(Ci~~Shade~AT~~(ppm))
shetci <- expression(Ci~~Shade~ET~~(ppm))

#A labels
suatA <- expression(A~~Sun~AT~~(ppm))
suetA <- expression(A~~Sun~ET~~(ppm))
shatA <- expression(A~~Shade~AT~~(ppm))
shetA <- expression(A~~Shade~ET~~(ppm))

wa<- palette(wes_palette("Zissou",5))
paircol <- c(wa[3], wa[1],wa[5])

pairlab <-  c(expression(paste(vs~~Sun~ET)), 
              expression(paste(vs~~Shade~AT)),
              expression(paste(vs~~Shade~ET)))

cilab <- expression(Ci~~(ppm))
cclab <- expression(Cc~~(ppm))