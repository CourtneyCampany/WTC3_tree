hiev_gmes <- read.csv("calculated_data/gmes_wtc.csv")

###gsub H from chamber
hiev_gmes$chamber <- gsub("h", "", hiev_gmes$chamber)
hiev_gmes$chamber <- gsub("c", "C", hiev_gmes$chamber)

###make new unique id and drop old one
hiev_gmes$leafid <- with(hiev_gmes, paste(Month,chamber, leaf, light, sep= "-"))
hiev_gmes <- hiev_gmes[, -2]

###changed name of light treatment                                        
match("light",names(hiev_gmes))
names(hiev_gmes)[83] <- "light_treatment"

###move date time to the front
match("datetime",names(hiev_gmes))
hiev_gmes <- hiev_gmes[, c(1, 59, 2:58, 60:88)]

###move treatments to front of dataset
hiev_gmes <- hiev_gmes[, c(1:2, 81:88, 3:80)]

###change treatments to correct format
match("temp",names(hiev_gmes))
names(hiev_gmes)[7] <- "T_treatment"

match("drydown",names(hiev_gmes))
names(hiev_gmes)[8] <- "Water_treatment"

hiev_gmes$Water_treatment <- gsub("drydown", "drought", hiev_gmes$Water_treatment)

write.csv(hiev_gmes, "hiev_data/WTC_TEMP_CM_GMES_20131026-20140421_L2.csv", row.names = FALSE)

colnames(hiev_gmes)
