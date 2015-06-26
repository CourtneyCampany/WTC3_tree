source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

gasex <- read.csv("calculated_data/gmes_wellwatered.csv")
lightson <- gasex[gasex$leaflight != "sun-high",]

lightson_agg <- summaryBy(Photo+gm~ chamber+id+leaf +light+temp+leaflight, data=lightson, FUN=mean, keep.names=TRUE)

  lightson_agg$leaf <- gsub("s", "S", lightson_agg$leaf)
  lightson_agg$light <- gsub("high", "Sun-light", lightson_agg$light)
  lightson_agg$light <- gsub("low", "Shade-light", lightson_agg$light)

###plotting----------------------------------------------------------------------------------------------  
  
windows(11,8.5)  

#png(filename = "markdown/gm_lightson.png", width = 11, height = 8.5, units = "in", res= 400)

#par(fig=c(0,1,0.45,1), new=TRUE)
windows(8,6)
par(mar=c(5,5,2,2), las=1, cex=1.25, cex.axis=1.25, cex.lab=1.25) 
plot(Photo~gm, data=lightson_agg, subset=leaflight=="shade-high", col=lightscol, ylim=c(5,25), xlim=c(0,0.3), 
     xlab=gmlab, ylab=satlab, pch=c(16, 17)[pch=lightson_agg$temp])
points(Photo~gm, data=lightson_agg, subset=leaflight=="shade-low", col=shacol, pch=c(16, 17)[pch=lightson_agg$temp])

legend("topleft", lightleg3, pch=c(16,17,16,17),inset = 0.02, col=collights, bty='n', cex=1) 
#text(x=.3, y=24.75, "(a)", cex=1.25)
#mtext(satlab, side=2, line=3.5, cex=1.25, las=3)

#dev.copy(png, file="markdown/gm_lightson.png", width=1100, height=800)
#dev.copy2pdf(file="master_scripts/paper_figures/gm_lightson.pdf")
# dev.off()





