source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")
gm_agg <- summaryBy(Trmmol+ Cond ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
gm_agg$leaf <- gsub("s", "S", gm_agg$leaf)
gm_agg$light <- gsub("high", "Sun-light", gm_agg$light)
gm_agg$light <- gsub("low", "Shade-light", gm_agg$light)

gsE_mod <- lm(Trmmol~Cond, data=gm_agg)
summary(gsE_mod)


library(nlme)
# library(lme4)
# library(lmerTest)
# library(LMERConvenienceFunctions)

gsE_mod2 <- lme(Trmmol~Cond, random= ~1|chamber,data=gm_agg)
anova(gsE_mod2)
summary(gsE_mod2)
mcp.fnc(gsE_mod2)
source("functions and packages/r2glmm.R")
rsquared.glmm(gsE_mod2)



palette(c(lightscol, shacol, suncol))

windows(7,7)
par(mar=c(4,4,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(2.5,1,0))
plot(Trmmol~Cond, data=gm_agg,  col=leaflight,ylim=c(0,8), xlim=c(0,.5), pch="", xlab=condlab, ylab=trmmollab)
ablineclip(gsE_mod, x1= min(gm_agg$Cond), x2 = max(gm_agg$Cond), lwd=2)
points(Trmmol~Cond, data=gm_agg,  col=leaflight,  pch=c(16, 17)[pch=gm_agg$temp])
legend("topleft", alllab, pch=c(16,16,16,16,17), col=c(suncol, shacol, lightscol2, "black", "black"),inset = 0.01, bty='n',cex=.8)


dev.copy2pdf(file="master_scripts/paper_figures/Egs.pdf")
dev.off()
