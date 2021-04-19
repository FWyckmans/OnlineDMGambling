##### Total Merge
# Initialization
source("DMA_Init.R")

Datapath = "Data/Pavlovia/Processed/"
Output_path = "Output/"

##### DF import
dRL1 <- read.delim(paste0(Datapath, "RLDay1.txt"))
dRL2 <- read.delim(paste0(Datapath, "RLDay2.txt"))
dQ <- read.delim("Data/LimeSurveyQuestionnaires/Processed/dQuestionnaireTot.txt")

##### Remove participant who did not finish
dQ$Mail1[!(dQ$Mail1 %in% dRL1$MailP1)]
dQ$Mail1[!(dQ$Mail1 %in% dRL2$MailP1)]

dQ <- dQ%>%
  filter(Mail1 %in% dRL1$MailP1)%>%
  filter(Mail1 %in% dRL2$MailP1)%>%
  arrange(Mail1)

dRL1 <- dRL1%>%
  filter(MailP1 %in% dQ$Mail1)%>%
  filter(MailP1 %in% dRL2$MailP1)%>%
  arrange(MailP1)

dRL2 <- dRL2%>%
  filter(MailP1 %in% dQ$Mail1)%>%
  filter(MailP1 %in% dRL1$MailP1)%>%
  arrange(MailP1)

dTot <- cbind(dQ, dRL1[3:length(dRL1)], dRL2[3:length(dRL2)])

##### Organize between Alcohol first and Neutral First
# Alc First
dAlF <- dTot%>%
  filter(FirstVid == "Alcohol")

colnames(dAlF) <- sub("D1", "AfterAlc", colnames(dAlF))
colnames(dAlF) <- sub("D2", "AfterNeutr", colnames(dAlF))

colnames(dAlF) <- sub("Craving1", "CravingBfAlc", colnames(dAlF))
colnames(dAlF) <- sub("Craving2", "CravingAfAlc", colnames(dAlF))
colnames(dAlF) <- sub("Craving3", "CravingAfAlc2", colnames(dAlF))
colnames(dAlF) <- sub("Craving4", "CravingBfNeu", colnames(dAlF))
colnames(dAlF) <- sub("Craving5", "CravingAfNeu", colnames(dAlF))
colnames(dAlF) <- sub("Craving6", "CravingAfNeu2", colnames(dAlF))

colnames(dAlF) <- sub("Resist1", "ResistBfAlc", colnames(dAlF))
colnames(dAlF) <- sub("Resist2", "ResistAfAlc", colnames(dAlF))
colnames(dAlF) <- sub("Resist3", "ResistAfAlc2", colnames(dAlF))
colnames(dAlF) <- sub("Resist4", "ResistBfNeu", colnames(dAlF))
colnames(dAlF) <- sub("Resist5", "ResistAfNeu", colnames(dAlF))
colnames(dAlF) <- sub("Resist6", "ResistAfNeu2", colnames(dAlF))

colnames(dAlF) <- sub("Stress1", "StressBfAlc", colnames(dAlF))
colnames(dAlF) <- sub("Stress2", "StressAfAlc", colnames(dAlF))
colnames(dAlF) <- sub("Stress3", "StressAfAlc2", colnames(dAlF))
colnames(dAlF) <- sub("Stress4", "StressBfNeu", colnames(dAlF))
colnames(dAlF) <- sub("Stress5", "StressAfNeu", colnames(dAlF))
colnames(dAlF) <- sub("Stress6", "StressAfNeu2", colnames(dAlF))

# Neutral First
dNeF <- dTot%>%
  filter(FirstVid == "Neutral")

colnames(dNeF) <- sub("D1", "AfterNeutr", colnames(dNeF))
colnames(dNeF) <- sub("D2", "AfterAlc", colnames(dNeF))

colnames(dNeF) <- sub("Craving1", "CravingBfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Craving2", "CravingAfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Craving3", "CravingAfNeu2", colnames(dNeF))
colnames(dNeF) <- sub("Craving4", "CravingBfAlc", colnames(dNeF))
colnames(dNeF) <- sub("Craving5", "CravingAfAlc", colnames(dNeF))
colnames(dNeF) <- sub("Craving6", "CravingAfAlc2", colnames(dNeF))

colnames(dNeF) <- sub("Resist1", "ResistBfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Resist2", "ResistAfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Resist3", "ResistAfNeu2", colnames(dNeF))
colnames(dNeF) <- sub("Resist4", "ResistBfAlc", colnames(dNeF))
colnames(dNeF) <- sub("Resist5", "ResistAfAlc", colnames(dNeF))
colnames(dNeF) <- sub("Resist6", "ResistAfAlc2", colnames(dNeF))

colnames(dNeF) <- sub("Stress1", "StressBfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Stress2", "StressAfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Stress3", "StressAfNeu2", colnames(dNeF))
colnames(dNeF) <- sub("Stress4", "StressBfAlc", colnames(dNeF))
colnames(dNeF) <- sub("Stress5", "StressAfAlc", colnames(dNeF))
colnames(dNeF) <- sub("Stress6", "StressAfAlc2", colnames(dNeF))

dTot <- rbind(dAlF, dNeF)

##### Export
write.table(dTot, paste0(Output_path, "dTot.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write_xlsx(dTot, paste0(Output_path, "dTot.xlsx"))