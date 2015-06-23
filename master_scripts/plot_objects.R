library(scales)

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
totcondlab <- expression(Total~Conductance~to~CO[2]~~(mol~m^-2~s^-1))

vpdlab <- "VPD  (kPa)"
itelab <- expression(ITE~~(mu*mol~CO[2]~mmol~H[2]*O^-1))

ratelab <- expression(mol~m^-2~s^-1)
parlab <- expression(PPFD~~(mol~m^-2~s^-1))
photolab <- expression(italic(A)~~(mu*mol~m^-2~s^-1))
trmmollab <- expression(Transpiration~~(mmol~H[2]*O~m^-2~s^-1))

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
suncol <- alpha("forestgreen", alpha=.75)
shacol <- alpha("yellow4", alpha=.75)
leafcol <- c(suncol, shacol)
leaflab2 <- c("Sun", "Shade")

colaci <-c(suncol, suncol, shacol, shacol)


leaflightlab <- c("shade-high", "shade-low", "sun")


