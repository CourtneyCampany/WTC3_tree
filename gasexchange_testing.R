source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")
source("functions and packages/packages.R")

#read in gm data set (no drought) and Cibar(discrimination)-------------------------------------------------------
gmes <- read.csv("calculated_data/gmes_wellwatered.csv")

###get average by id
gm_agg <- summaryBy(Photo+ Cond +Ci+ gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes, FUN=mean, keep.names=TRUE)
gm2 <- summaryBy(Photo+ Cond +Ci+ gm ~ leaflight, data=gm_agg, FUN=c(mean,se))


### we can test gas exchange data here

boxplot(Ci~leaflight, data=gm_agg)
boxplot(Photo~leaflight, data=gm_agg)
boxplot(gm~leaflight, data=gm_agg)
boxplot(Cond~leaflight, data=gm_agg)

stats_gm <- boxplot(gm~leaflight, data=gm_agg)



###read in tobacco dataset
gmes_tob <- read.csv("calculated_data/gmes_tob_wellwatered.csv")

###get average by id
gm_tob_agg <- summaryBy(Photo+ Cond +Ci+ gm ~ chamber+id+leaf +light+temp+leaflight+Month, data=gmes_tob, FUN=mean, keep.names=TRUE)

gm_tob2 <- summaryBy(Photo+ Cond +Ci+ gm ~ leaflight, data=gm_tob_agg, FUN=c(mean,se))


###aci
#read in jmax vcmax
jvc<- read.csv("calculated_data/tdl_aci.csv")
