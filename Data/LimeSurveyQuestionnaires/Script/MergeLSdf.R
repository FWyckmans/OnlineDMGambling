##### Merge dLS
# Initialization
source("DMG_Init.R")
Datapath = "Data/LimeSurveyQuestionnaires/Processed/"
Output_path = "Data/LimeSurveyQuestionnaires/Processed/"

##### Import processed LS data
dLS1 <- read.delim(paste0(Datapath, "dLS1.txt"))
dLS2 <- read.delim(paste0(Datapath, "dLS2.txt"))
dLS3 <- read.delim(paste0(Datapath, "dLS3.txt"))

dLS1 <- AddDummyCol(dLS1, "Finished", 0)

for (i in dLS1$Mail1){
  if ((i %in% dLS2$Mail2) & (i %in% dLS3$Mail3)){
    dLS1$Finished[dLS1$Mail1 == i] = 1
  }
}

dLS1 <- filter(dLS1, Finished == 1)
dLS1 = dLS1[order(dLS1[,'Mail1']),]
dLS2 <- filter(dLS2, Mail2 %in% dLS1$Mail1)
dLS2 = dLS2[order(dLS2[,'Mail2']),]
dLS3 <- filter(dLS3, Mail3 %in% dLS1$Mail1)
dLS3 = dLS3[order(dLS3[,'Mail3']),]

dQ <- cbind(dLS1, dLS2[3:length(dLS2)], dLS3[3:length(dLS3)])

write.table(dQ, paste0(Output_path, "dQuestionnaireTot.txt"), sep = "\t", dec = ".", col.names = T, row.names = F)
write_xlsx(dQ, paste0(Output_path, "dQuestionnaireTot.xlsx"))