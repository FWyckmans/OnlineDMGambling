##### Pavlovia Day 2
# Initialization
source("DMA_Init.R")

Datapath = "Data/Pavlovia/J2/"
Output_path = "Data/Pavlovia/Processed/"

dQ <- read.delim("Data/LimeSurveyQuestionnaires/Processed/dQuestionnaireTot.txt")

# Removal of (nearly) empty CSV
PSToRemove <- c(dir(Datapath, pattern = ".log.gz"),
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-02-17_13h11.04.043.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-02-17_15h01.28.163.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-02-17_15h04.08.996.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-02-17_15h08.06.616.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-02-17_18h49.59.094.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-02-17_18h50.00.354.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-01_14h15.08.091.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-01_14h21.09.396.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-15_11h28.19.255.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-15_11h28.24.339.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-16_14h00.00.364.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-18_18h29.54.072.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-18_18h36.26.441.csv",
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-18_18h38.10.877.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_11h56.04.741.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_13h10.50.994.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_14h50.53.439.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_14h51.08.191.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_14h51.27.883.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_15h04.08.587.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_15h08.48.461.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_16h44.41.388.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_18h45.38.351.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-17_18h47.41.189.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-03-25_18h32.45.312.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-04-01_15h57.29.030.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-04-03_14h31.19.220.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-04-05_11h23.21.480.csv",
                "PARTICIPANT_MarkovTaskNaJ2_2021-04-10_19h49.15.994.csv",
                
                "PARTICIPANT_MarkovTaskAlcoolJ2_2021-04-05_16h41.31.505.csv", # Duplicate
                "PARTICIPANT_MarkovTaskNaJ2_2021-02-23_19h35.31.944.csv") # Duplicate

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
