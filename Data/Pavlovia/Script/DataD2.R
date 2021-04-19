##### Pavlovia Day 2
# Initialization
source("DMG_Init.R")

Datapath = "Data/Pavlovia/J2/"
Output_path = "Data/Pavlovia/Processed/"

dQ <- read.delim("Data/LimeSurveyQuestionnaires/Processed/dQuestionnaireTot.txt")

# Removal of (nearly) empty CSV
PSToRemove <- c(dir(Datapath, pattern = ".log.gz"),
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-02-11_17h40.59.905.csv", # Empty CSV Gambling
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-02-11_17h43.55.975.csv",
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-02-11_17h58.17.544.csv",
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-02-11_19h57.15.748.csv",
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-02-17_13h05.17.947.csv",
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-02-18_13h38.07.262.csv",
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-03-30_15h25.51.707.csv",
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-04-05_09h43.19.657.csv",
                "PARTICIPANT_MarkovTaskGamblingJ2_2021-04-06_17h34.51.753.csv",
                
                "PARTICIPANT_MarkovTaskNeutreJ2_2021-02-17_13h05.02.100.csv",   # Empty CSV Neutral
                "PARTICIPANT_MarkovTaskNeutreJ2_2021-03-26_16h20.41.513.csv",
                "PARTICIPANT_MarkovTaskNeutreJ2_2021-03-26_16h20.43.329.csv",
                "PARTICIPANT_MarkovTaskNeutreJ2_2021-03-28_09h14.31.504.csv",
                "PARTICIPANT_MarkovTaskNeutreJ2_2021-03-30_15h54.59.322.csv")

dt <- dir(Datapath)
ParticipantToKeep <- !(dt %in% PSToRemove)
PS <- dt[ParticipantToKeep]

Mail = c()
Compt = 1
dRL2 <- data.frame()
for (i in PS) {
  d <- read.csv(paste0(Datapath, i))
  
  Mail[Compt] <- d$email[1]
  order <- d$expName[1]
  
  # Change email if needed
  if (d$email[1] %in% names(MailToChange)){
    d$email[1] <- MailToChange[[d$email[1]]]
    Mail[Compt] <- d$email[1]
  }
  
  if (Mail[Compt] %in% dQ$Mail1){
    dPS <- ComputeRLT(d, 2)
    dPS$MailP1 <- Mail[Compt]
    dLick <- LickRL(d, 2)
    dRL <- cbind(dPS, dLick)
    dRL2 <- rbind(dRL2, dRL)
  }
  Compt = Compt + 1
}

dMail2 = data.frame(Mail)

write.table(dRL2, paste0(Output_path, "RLDay2.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dMail2, "AdditionalInfo/MailList/MailD2.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
# print(i)