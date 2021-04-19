##### Total Merge
# Initialization
source("DMG_Init.R")

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

##### Organize between Gambling first and Neutral First
# Gambling First
dGaF <- dTot%>%
  filter(FirstVid == "Gambling")

colnames(dGaF) <- sub("D1", "AfterGamb", colnames(dGaF))
colnames(dGaF) <- sub("D2", "AfterNeutr", colnames(dGaF))

colnames(dGaF) <- sub("Craving1", "CravingBfGamb", colnames(dGaF))
colnames(dGaF) <- sub("Craving2", "CravingAfGamb", colnames(dGaF))
colnames(dGaF) <- sub("Craving3", "CravingAfGamb2", colnames(dGaF))
colnames(dGaF) <- sub("Craving4", "CravingBfNeu", colnames(dGaF))
colnames(dGaF) <- sub("Craving5", "CravingAfNeu", colnames(dGaF))
colnames(dGaF) <- sub("Craving6", "CravingAfNeu2", colnames(dGaF))

colnames(dGaF) <- sub("Resist1", "ResistBfGamb", colnames(dGaF))
colnames(dGaF) <- sub("Resist2", "ResistAfGamb", colnames(dGaF))
colnames(dGaF) <- sub("Resist3", "ResistAfGamb2", colnames(dGaF))
colnames(dGaF) <- sub("Resist4", "ResistBfNeu", colnames(dGaF))
colnames(dGaF) <- sub("Resist5", "ResistAfNeu", colnames(dGaF))
colnames(dGaF) <- sub("Resist6", "ResistAfNeu2", colnames(dGaF))

colnames(dGaF) <- sub("Stress1", "StressBfGamb", colnames(dGaF))
colnames(dGaF) <- sub("Stress2", "StressAfGamb", colnames(dGaF))
colnames(dGaF) <- sub("Stress3", "StressAfGamb2", colnames(dGaF))
colnames(dGaF) <- sub("Stress4", "StressBfNeu", colnames(dGaF))
colnames(dGaF) <- sub("Stress5", "StressAfNeu", colnames(dGaF))
colnames(dGaF) <- sub("Stress6", "StressAfNeu2", colnames(dGaF))

# Neutral First
dNeF <- dTot%>%
  filter(FirstVid == "Neutral")

colnames(dNeF) <- sub("D1", "AfterNeutr", colnames(dNeF))
colnames(dNeF) <- sub("D2", "AfterGamb", colnames(dNeF))

colnames(dNeF) <- sub("Craving1", "CravingBfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Craving2", "CravingAfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Craving3", "CravingAfNeu2", colnames(dNeF))
colnames(dNeF) <- sub("Craving4", "CravingBfGamb", colnames(dNeF))
colnames(dNeF) <- sub("Craving5", "CravingAfGamb", colnames(dNeF))
colnames(dNeF) <- sub("Craving6", "CravingAfGamb2", colnames(dNeF))

colnames(dNeF) <- sub("Resist1", "ResistBfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Resist2", "ResistAfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Resist3", "ResistAfNeu2", colnames(dNeF))
colnames(dNeF) <- sub("Resist4", "ResistBfGamb", colnames(dNeF))
colnames(dNeF) <- sub("Resist5", "ResistAfGamb", colnames(dNeF))
colnames(dNeF) <- sub("Resist6", "ResistAfGamb2", colnames(dNeF))

colnames(dNeF) <- sub("Stress1", "StressBfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Stress2", "StressAfNeu", colnames(dNeF))
colnames(dNeF) <- sub("Stress3", "StressAfNeu2", colnames(dNeF))
colnames(dNeF) <- sub("Stress4", "StressBfGamb", colnames(dNeF))
colnames(dNeF) <- sub("Stress5", "StressAfGamb", colnames(dNeF))
colnames(dNeF) <- sub("Stress6", "StressAfGamb2", colnames(dNeF))

dTot <- rbind(dGaF, dNeF)

##### Export
write.table(dTot, paste0(Output_path, "dTot.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write_xlsx(dTot, paste0(Output_path, "dTot.xlsx"))
