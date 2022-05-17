#################### Demographics
##### Initialization
source("DMG_Init.R")

##### Data
d <- read.delim("Output/dFINAL.txt")

# Demographic variables
d <- AddDummyCol(d, "Smoker", 1)
d$Smoker[is.na(d$Fager)] <- 0

DemVar = c("Age", "Gender", "StudyLvl",
           "ICJE", "AUDIT", "CAST", "Fager", "Smoker",
           "Auto", "Routine",
           "UPPS", "NegUr", "PosUr", "LoPr", "LoPe", "SS",
           "PosAff", "NegAff", "BDI",
           "CERQAdaptative", "CERQNonAdaptative")

dDem <- select(d, Grp, Grp3C, all_of(DemVar))
