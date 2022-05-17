#################### N Participants
##### Initialization
source("DMG_Init.R")
thresholdPG = 4

##### Count
# Beginning
dLS1 <- read.delim("Data/LimeSurveyQuestionnaires/Processed/dLS1.txt")
dLS1 = dLS1[!duplicated(dLS1$Mail1),]

nHC_B = sum(is.na(dLS1$ICJE))
nG_B = sum(dLS1$ICJE < thresholdPG, na.rm = T)
nPG_B = sum(dLS1$ICJE >= thresholdPG, na.rm = T)
n_B = nHC_B + nG_B + nPG_B

# Finished
dF <- read.delim("Output/Anonyme/dTotMerged.txt")

nHC_F = sum(is.na(dF$ICJE))
nG_F = sum(dF$ICJE < thresholdPG, na.rm = T)
nPG_F = sum(dF$ICJE >= thresholdPG, na.rm = T)

n_F = nHC_F + nG_F + nPG_F

# RL task exclusion
dExclRL <- filter(dF, (OKRL_AfterGamb == 1 & OKRL_AfterNeutr == 1))

nHC_ERL = sum(is.na(dExclRL$ICJE))
nG_ERL = sum(dExclRL$ICJE < thresholdPG, na.rm = T)
nPG_ERL = sum(dExclRL$ICJE >= thresholdPG, na.rm = T)

n_ERL = nHC_ERL + nG_ERL + nPG_ERL

# Q exlusion
dExclQ <- filter(dExclRL, TestQOK == 1)

nHC_EQ = sum(is.na(dExclQ$ICJE))
nG_EQ = sum(dExclQ$ICJE < thresholdPG, na.rm = T)
nPG_EQ = sum(dExclQ$ICJE >= thresholdPG, na.rm = T)

n_EQ = nHC_EQ + nG_EQ + nPG_EQ

# Output
outputB = paste0("Participants who entered the study: ", nHC_B, " controls, ", nG_B, " gamblers & ", nPG_B, " PG.")
outputF = paste0("Participants who finished the study: ", nHC_F, " controls, ", nG_F, " gamblers & ", nPG_F, " PG.")
outputERL = paste0("Participants after RL exlusion: ", nHC_ERL, " controls, ", nG_ERL, " gamblers & ", nPG_ERL, " PG.")
outputQRL = paste0("Participants after Q exlusion: ", nHC_EQ, " controls, ", nG_EQ, " gamblers & ", nPG_EQ, " PG.")
cat(outputB, "\n", outputF, "\n", outputERL, "\n", outputQRL)
