##### Pavlovia Day 1
# Initialization
source("DMG_Init.R")

Datapath = "Data/Pavlovia/J1/"
Output_path = "Data/Pavlovia/Processed/"

dQ <- read.delim("Data/LimeSurveyQuestionnaires/Processed/dQuestionnaireTot.txt")

# Removal of (nearly) empty CSV
PSToRemove <- c(dir(Datapath, pattern = ".log.gz"),
                 dir(Datapath, pattern = "InstrMarkovTaskVideoG"),
                "PARTICIPANT_InstrMarkovVideoG_2021-02-17_13h04.23.228.csv",     # Empty CSV
                "PARTICIPANT_InstrMarkovVideoG_2021-03-08_14h59.04.591.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-08_21h06.58.487.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-08_21h07.12.056.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-08_21h07.16.365.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-09_12h40.27.918.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-09_12h40.39.466.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-09_12h42.21.516.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-09_12h56.28.723.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-09_13h00.43.499.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-09_17h21.15.277.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-09_17h21.21.321.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-26_14h43.43.383.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-26_15h04.02.709.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-03-28_19h21.50.808.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-04-19_14h39.43.525.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-02-17_12h52.38.966.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-02-17_12h55.42.445.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-02-17_13h04.38.708.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-02-19_15h49.39.783.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-03-08_22h21.53.352.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-03-25_20h20.20.450.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-03-25_20h20.50.170.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-03-25_20h20.52.513.csv",
                "PARTICIPANT_InstrMarkovVideoN_2021-04-19_15h51.43.751.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-02-17_12h46.54.912.csv", # Empty Video Gambling
                "PARTICIPANT_InstructionMarkovVideoG_2021-03-08_21h05.42.679.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-03-09_13h47.58.145.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-03-09_13h52.37.593.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-03-27_14h22.34.372.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-04-15_21h45.09.803.csv",
                "PARTICIPANT_InstructionMarkovVideoN_2021-03-08_14h18.09.893.csv",
                "PARTICIPANT_InstructionMarkovVideoN_2021-03-08_16h01.32.874.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-03-08_15h03.22.778.csv", # Nearly empty Video Gambling
                "PARTICIPANT_InstructionMarkovVideoG_2021-03-09_10h35.43.493.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-03-26_14h13.44.273.csv",
                "PARTICIPANT_InstrMarkovVideoG_2021-05-01_13h04.45.830.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-05-01_13h09.04.617.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-05-06_00h58.46.789.csv",
                "PARTICIPANT_InstructionMarkovVideoG_2021-05-06_01h00.11.439.csv",
                
                "PARTICIPANT_InstructionMarkovVideoN_2021-03-09_18h05.00.628.csv", # Empty Video N
                "PARTICIPANT_InstructionMarkovVideoN_2021-04-06_10h23.48.341.csv",
                "PARTICIPANT_InstructionMarkovVideoN_2021-05-08_00h22.57.790.csv",
                "PARTICIPANT_InstructionMarkovVideoN_2021-04-20_15h51.08.775.csv",
                "PARTICIPANT_InstructionMarkovVideoN_2021-10-14_18h10.17.089.csv",
                "PARTICIPANT_InstructionMarkovVideoN_2021-10-14_18h18.39.799.csv")

dt <- dir(Datapath)
ParticipantToKeep <- !(dt %in% PSToRemove)
PS <- dt[ParticipantToKeep]

Mail = c()
Compt = 1
dRL1 <- data.frame()
dComp <- data.frame()
i = 	"PARTICIPANT_InstructionMarkovVideoG_2021-03-26_14h19.28.091.csv"

for (i in PS) {
  d <- read.csv(paste0(Datapath, i))
  
  Mail[Compt] <- d$email[1]
  order <- d$expName[1]
  
  # Change email if needed
  if (d$email[1] %in% names(MailToChange)){
    d$email[1] <- MailToChange[[d$email[1]]]
    Mail[Compt] <- d$email[1]
    # print(Mail[Compt])
    # print(i)
  }
  
  # Find problematic PS
  # if (Mail[Compt] == "clement.dollon@yahoo.fr"){
  #   print(i)}
    
  # Compute score
  if (Mail[Compt] %in% dQ$Mail1){
    dPS <- ComputeRLT(d, 1)
    dPS$MailP1 = Mail[Compt]
    dCompPS$Mail = Mail[Compt]
    dComp <- rbind(dComp, dCompPS)
    dLick <- LickRL(d, 1)
    dRL <- cbind(dPS, dLick)
    dRL1 <- rbind(dRL1, dRL)
  }
  Compt = Compt + 1
}

# print(i)
dMail1 = data.frame(Mail)


write.table(dRL1, paste0(Output_path, "RLDay1.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dMail1, "AdditionalInfo/MailList/MailD1.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dComp, paste0(Output_path, "Comp1.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")