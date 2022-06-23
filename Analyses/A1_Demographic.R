remove(list = ls())

############################################ Parameter ############################################
# Initialization
source("DMG_Init.R")

Datapath = "Output/Processed/"
Output_path = "Output/"

############################################ Frame ################################################
d <- read.delim(paste0(Output_path,"dFINAL.txt"))

########################################### Descriptive ###########################################
sum(d$Grp == "PG")
sum(d$Grp == "HC")

dDescr <- TotalDescrFrame(d,
                          VoI = FromColNameToIndex(d, c("Age", "Gender", "StudyLvl",
                                                        "ICJE", "CAST", "Fager", "AUDIT",
                                                        "Auto", "Routine",
                                                        "UPPS", "NegUr", "PosUr", "LoPr", "LoPe", "SS",
                                                        "PosAff", "NegAff", "BDI",
                                                        "CERQAdaptative", "CERQNonAdaptative")),
                          Groups = "Grp", Format = "Long", CriticVal = 3.29)

write_xlsx(dDescr, paste0(Output_path, "dDescrGrp.xlsx"))
