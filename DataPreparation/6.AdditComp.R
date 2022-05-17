# Additional Computations
##### Initialization
source("DMG_Init.R")
Datapath = "Data/Pavlovia/Processed/"

##### Data Frame
## Import
if (WithAnonyme == TRUE){
  dTot <- read.delim("Output/Anonyme/dTotCompAnonyme.txt")
}

dTotA <- read.delim("Output/Processed/dTotComp.txt")
dComp <- read.delim("Output/Processed/dCompR.txt")

## Data Preparation
RTCol = c("RT1_R_AfterGamb", "RT1_U_AfterGamb", "RT2_C_AfterGamb", "RT2_R_AfterGamb",
          "RT1_R_AfterNeu", "RT1_U_AfterNeu", "RT2_C_AfterNeu", "RT2_R_AfterNeu")

dTot <- AddDummyCol(dTot, ToAdd = RTCol, Val = NA)
dTotA <- AddDummyCol(dTotA, ToAdd = RTCol, Val = NA)

dComp$reward[dComp$reward == 0] <- -1
dComp$transition[dComp$transition == 0] <- -1

IndivRL <- function(d){
  RegLogInd <- glm(Stay ~ PrReward*PrTransition,
                   family = binomial(logit), data = d)
  
  MB = exp(coef(RegLogInd))[[4]]
  MF = exp(coef(RegLogInd))[[2]]
  
  dU <- filter(d, PrReward == -1)
  
  RegLogInd <- glm(Stay ~ PrTransition,
                   family = binomial(logit), data = dU)
  
  MBun = exp(coef(RegLogInd))[[2]]
  
  score = c(MB, MF, MBun)
  return(score)
}

IndivRT <- function(d){
  d$PrReward[d$PrReward == 1] <- 'Rewarded'
  d$PrReward[d$PrReward == -1] <-'Unrewarded'
  d$transition[d$transition == 1] <- 'Common'
  d$transition[d$transition == -1] <- 'Rare'
  
  dRT1 <- d%>%
    group_by(PrReward)%>%
    summarise(RT1M = mean(RT1))
  
  RT1R = dRT1$RT1M[dRT1$PrReward == "Rewarded"]
  RT1U = dRT1$RT1M[dRT1$PrReward == "Unrewarded"]
  
  dRT2 <- d%>%
    group_by(transition)%>%
    summarise(RT2m = mean(RT2))
  
  RT2C = dRT2$RT2m[dRT2$transition == "Common"]
  RT2R = dRT2$RT2m[dRT2$transition == "Rare"]
  
  RTs = c(RT1R, RT1U, RT2C, RT2R)
  
  return(RTs)}

########## RL Comp
for (i in dTotA$NS){
  ## Dataframes
  dPS = filter(dComp, subjID == i)
  
  NameNeutr = c("InstructionMarkovVideoN", "MarkovTaskNeutreJ2")
  NameGamb = c("InstructionMarkovVideoG", "MarkovTaskGamblingJ2")
  
  dPS_AN = filter(dPS, ExpName %in% NameNeutr)
  dPS_AG = filter(dPS, ExpName %in% NameGamb)
  
  ## RL
  ScoreAfterNeu <- IndivRL(dPS_AN)
  ScoreAfterGamb <- IndivRL(dPS_AG)
  
  ## RT
  RTAfterNeu <- IndivRT(dPS_AN)
  RTAfterGamb <- IndivRT(dPS_AG)
  
  ##### Add to dTot
  # AfterNeu
  dTotA$MBRL_AfterNeu[dTotA$NS == i] <- ScoreAfterNeu[1]
  dTotA$MFRL_AfterNeu[dTotA$NS == i] <- ScoreAfterNeu[2]
  dTotA$MBunRL_AfterNeu[dTotA$NS == i] <- ScoreAfterNeu[3]
  
  dTotA$RT1_R_AfterNeu[dTotA$NS == i] <- RTAfterNeu[1]
  dTotA$RT1_U_AfterNeu[dTotA$NS == i] <- RTAfterNeu[2]
  
  dTotA$RT2_C_AfterNeu[dTotA$NS == i] <- RTAfterNeu[3]
  dTotA$RT2_R_AfterNeu[dTotA$NS == i] <- RTAfterNeu[4]
  
  # AfterGamb
  dTotA$MBRL_AfterGamb[dTotA$NS == i] <- ScoreAfterGamb[1]
  dTotA$MFRL_AfterGamb[dTotA$NS == i] <- ScoreAfterGamb[2]
  dTotA$MBunRL_AfterGamb[dTotA$NS == i] <- ScoreAfterGamb[3]
  
  dTotA$RT1_R_AfterGamb[dTotA$NS == i] <- RTAfterGamb[1]
  dTotA$RT1_U_AfterGamb[dTotA$NS == i] <- RTAfterGamb[2]
  
  dTotA$RT2_C_AfterGamb[dTotA$NS == i] <- RTAfterGamb[3]
  dTotA$RT2_R_AfterGamb[dTotA$NS == i] <- RTAfterGamb[4]
  
  ##### Add to dTot Anonyme
  if (WithAnonyme == TRUE){
    # AfterNeu
    dTot$MBRL_AfterNeu[dTot$NS == i] <- ScoreAfterNeu[1]
    dTot$MFRL_AfterNeu[dTot$NS == i] <- ScoreAfterNeu[2]
    dTot$MBunRL_AfterNeu[dTot$NS == i] <- ScoreAfterNeu[3]
    
    dTot$RT1_R_AfterNeu[dTot$NS == i] <- RTAfterNeu[1]
    dTot$RT1_U_AfterNeu[dTot$NS == i] <- RTAfterNeu[2]
    
    dTot$RT2_C_AfterNeu[dTot$NS == i] <- RTAfterNeu[3]
    dTot$RT2_R_AfterNeu[dTot$NS == i] <- RTAfterNeu[4]
    
    # AfterGamb
    dTot$MBRL_AfterGamb[dTot$NS == i] <- ScoreAfterGamb[1]
    dTot$MFRL_AfterGamb[dTot$NS == i] <- ScoreAfterGamb[2]
    dTot$MBunRL_AfterGamb[dTot$NS == i] <- ScoreAfterGamb[3]
    
    dTot$RT1_R_AfterGamb[dTot$NS == i] <- RTAfterGamb[1]
    dTot$RT1_U_AfterGamb[dTot$NS == i] <- RTAfterGamb[2]
    
    dTot$RT2_C_AfterGamb[dTot$NS == i] <- RTAfterGamb[3]
    dTot$RT2_R_AfterGamb[dTot$NS == i] <- RTAfterGamb[4]}
}

write.table(dTotA, "Output/Processed/dTotAddComp.txt", col.names = T, row.names = F, sep = "\t", dec = ".")

if (WithAnonyme == TRUE){
  write.table(dTot, "Output/Anonyme/dTotAddComp_Anonyme.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
}