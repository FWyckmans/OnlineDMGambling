# Prep for RegLog
##### Initialization
source("DMG_Init.R")

##### Data Frame
## Import
dTotA <- read.delim("Output/Processed/dTotComp.txt")
dComp <- read.delim("Output/Processed/dCompR.txt")

##### Prep for RegLog
dComp$Time <- "AfterGambling"

NameNeutr = c("InstructionMarkovVideoN", "MarkovTaskNeutreJ2")
NameGamb = c("InstructionMarkovVideoG", "MarkovTaskGamblingJ2")

dComp$Time[dComp$ExpName == "InstructionMarkovVideoN"] <- "AfterNeutral"
dComp$Time[dComp$ExpName == "MarkovTaskNeutreJ2"] <- "AfterNeutral"

dRegLog <- select(dComp, subjID, Time, Trial, Stay, PrReward, PrTransition)

write.table(dRegLog, "Output/Processed/dRegLog.txt", col.names = T, row.names = F, sep = "\t", dec = ".")