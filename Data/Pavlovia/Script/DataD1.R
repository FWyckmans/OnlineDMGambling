##### Pavlovia Day 1
# Initialization
source("DMA_Init.R")

Datapath = "Data/Pavlovia/J1/"
Output_path = "Data/Pavlovia/Processed/"

dQ <- read.delim("Data/LimeSurveyQuestionnaires/Processed/dQuestionnaireTot.txt")

# Removal of (nearly) empty CSV
PSToRemove <- c(dir(Datapath, pattern = ".log.gz"),
                "PARTICIPANT_InstructionMarkovVideoA_2021-03-06_19h20.40.474.csv",
                "PARTICIPANT_InstructionMarkovVideoA_2021-03-27_19h13.24.065.csv",
                 "PARTICIPANT_InstructionMarkovVideoA_2021-03-29_12h49.11.930.csv",
                 "PARTICIPANT_InstructionMarkovVideoA_2021-03-29_13h00.04.140.csv",
                 "PARTICIPANT_InstructionMarkovVideoA_2021-04-18_16h43.11.388.csv",
                 dir(Datapath, pattern = "InstructionMarkovTaskVideoA"),
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-02-17_13h09.24.885.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-02-17_18h33.05.994.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-02-17_18h45.19.522.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-02-17_18h45.58.684.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-02-22_11h21.55.320.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-02-22_11h24.35.050.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-02-22_11h50.47.914.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-03-08_19h26.35.437.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-03-08_19h26.46.866.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-03-26_14h16.00.957.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-03-27_19h00.24.828.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-03-27_19h04.57.735.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-03-27_19h08.20.962.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-03-28_15h48.42.016.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-04-13_18h07.19.717.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-04-15_19h28.04.752.csv",
                 "PARTICIPANT_InstructionMarkovVideoNa_2021-04-05_23h32.10.554.csv")

dt <- dir(Datapath)
ParticipantToKeep <- !(dt %in% PSToRemove)
PS <- dt[ParticipantToKeep]

Mail = c()
Compt = 1
dRL1 <- data.frame()
for (i in PS) {
  d <- read.csv(paste0(Datapath, i))
  
  Mail[Compt] <- d$email[1]
  order <- d$expName[1]
  
  # Change email if needed
  if (d$email[1] %in% names(MailToChange)){
    d$email[1] <- MailToChange[[d$email[1]]]
    Mail[Compt] <- d$email[1]
  }
  
  # Compute score
  if (Mail[Compt] %in% dQ$Mail1){
    dPS <- ComputeRLT(d, 1)
    dPS$MailP1 <- Mail[Compt]
    dLick <- LickRL(d, 1)
    dRL <- cbind(dPS, dLick)
    dRL1 <- rbind(dRL1, dRL)
  }
  Compt = Compt + 1
}

dMail1 = data.frame(Mail)


write.table(dRL1, paste0(Output_path, "RLDay1.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dMail1, "AdditionalInfo/MailList/MailD1.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
