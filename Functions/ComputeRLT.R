# Datapath = "Data/Pavlovia/J1/"
# d <- read.csv(paste0(Datapath, "PARTICIPANT_InstructionMarkovVideoNa_2021-04-17_18h08.13.435.csv"))
# Time = 1

ComputeRLT <- function(d, Time = 1){
  if (Time == 1){
    dDaw <- d%>%
      select(email, ExpName = expName, Task1.thisTrialN,
             Choice1 = Choice1.keys, Choice2 = Choice2.keys, Transition, Reward,
             RT1 = Choice1.rt, RT2 = Choice2.rt,
             PChoice21, PChoice22, PChoice23, PChoice24)%>%
      filter(!is.na(Task1.thisTrialN))}
  
  if (Time == 2){
    dDaw <- d%>%
      select(email, ExpName = expName, Task2.thisTrialN,
             Choice1 = Choice1.keys, Choice2 = Choice2.keys, Transition, Reward,
             RT1 = Choice1.rt, RT2 = Choice2.rt,
             PChoice21, PChoice22, PChoice23, PChoice24)%>%
      filter(!is.na(Task2.thisTrialN))}
  
  dDaw <- AddDummyCol(dDaw, c("PrReward", "PrTransition", "Stay", "Stay2"))
  colnames(dDaw)[1] <- "MailP1"
  colnames(dDaw)[3] <- "Index"
  dDaw$Index = dDaw$Index + 1
  
  # Compute previous transition and previous reward
  for (i in 2:max(dDaw$Index)) {
    dDaw$PrReward[i] <- dDaw$Reward[i-1]
    dDaw$PrTransition[i] <- dDaw$Transition[i-1]
    
    if (dDaw$Choice1[i] == dDaw$Choice1[i-1]){
      dDaw$Stay[i] <- 1
    } else {
      dDaw$Stay[i] <- 0
    }
    
    if (dDaw$Choice2[i] == dDaw$Choice2[i-1]){
      dDaw$Stay2[i] <- 1
    } else {
      dDaw$Stay2[i] <- 0
    }
  }
  
  dDaw <- dDaw%>%
    filter(RT1 <= 2)%>%
    filter(RT2 <= 2)%>%
    filter(Index > 10)
  
  dCompPS <- dDaw%>%
    select(Mail = MailP1, ExpName, Trial = Index, Stay, PrReward, PrTransition, RT1, RT2,
           Choice1, Choice2, reward = Reward, transition = Transition)
  
  dCompPS <<- AddDummyCol(dCompPS, "Time", Time)
  
  dD <- dDaw%>%
    group_by(MailP1, ExpName, PrReward, PrTransition)%>%
    summarise(PStay = mean(Stay), RT1 = mean(RT1), RT2 = mean(RT2))%>%
    group_by()
  
  # Got to a large format
  dD <- AddDummyCol(dD, "PrTrial")
  dD$PrTrial[((dD$PrReward == 1) & (dD$PrTransition == 1))] <- "PRC"
  dD$PrTrial[((dD$PrReward == 1) & (dD$PrTransition == 0))] <- "PRR"
  dD$PrTrial[((dD$PrReward == 0) & (dD$PrTransition == 1))] <- "PUC"
  dD$PrTrial[((dD$PrReward == 0) & (dD$PrTransition == 0))] <- "PUR"
  
  dD <- dD%>%
    select(MailP1, ExpName, PrTrial, PStay, RT1, RT2)%>%
    unite("PStay_RT1_RT2", PStay, RT1, RT2, sep = "_")%>%
    spread(key = PrTrial, value = PStay_RT1_RT2)%>%
    separate(PRC, into = c('PRC', "PRC_RT1", "PRC_RT2"), sep = "_")%>%
    separate(PRR, into = c('PRR', "PRR_RT1", "PRR_RT2"), sep = "_")%>%
    separate(PUC, into = c('PUC', "PUC_RT1", "PUC_RT2"), sep = "_")%>%
    separate(PUR, into = c('PUR', "PUR_RT1", "PUR_RT2"), sep = "_")
  dD[3:length(dD)] <- as.numeric(dD[3:length(dD)])
  
  # Compute MB and MF scores
  dD <- dD%>%
    mutate(MF = PRC + PRR - PUC - PUR,
           MB = PRC - PRR - PUC + PUR,
           MBun = -PUC + PUR)
  
  ##### Check if task was correctly performed
  OK1 = 0
  OK2 = 0
  OK3 = 0
  OKRL = data.frame(OK1, OK2, OK3)
  
  # OK1 PStay2(rewarded common) > 0.5
  PStay2RewCom = mean(dDaw$Stay2[(dDaw$PrTransition == 1) & (dDaw$PrReward == 1)])
  if (PStay2RewCom > 0.5){
    OKRL$OK1 = 1
  }
  
  # OK2 Max 95% same choice at step 1
  fe = table(dDaw$Choice1)[1]/(table(dDaw$Choice1)[1] + table(dDaw$Choice1)[2]) < 0.95
  fi = table(dDaw$Choice1)[2]/(table(dDaw$Choice1)[2] + table(dDaw$Choice1)[1]) < 0.95
  
  if (!is.na(fe) & !is.na(fi)){
    if ((fe == T) & (fi == T)){
      OKRL$OK2 <- 1
    }
  }
  
  # Minimum 100 trials
  nT <- length(dDaw$MailP1)
  if (nT > 100){OKRL$OK3 <- 1}
  
  # Ok tot
  dD <- AddDummyCol(dD, "OKRL", Val = 0)
  
  if ((OKRL$OK2 == 1) & (OKRL$OK3 == 1)){   # if ((OKRL$OK1 == 1) & (OKRL$OK2 == 1)){
    dD$OKRL = 1
  }
  
  ##### Prepare final frame according to the video
  if (Time == 1){
    dD <- dD%>%
      select(MailP1, ExpName, OKRL_D1 = OKRL,
             MF_D1 = MF, MB_D1 = MB, MBun_D1 = MBun,
             PRC_D1 = PRC, PRR_D1 = PRR, PUC_D1 = PUC, PUR_D1 = PUR,
             PRC_RT1_D1 = PRC_RT1, PRR_RT1_D1 = PRR_RT1, PUC_RT1_D1 = PUC_RT1, PUR_RT1_D1 = PUR_RT1,
             PRC_RT2_D1 = PRC_RT2, PRR_RT2_D1 = PRR_RT2, PUC_RT2_D1 = PUC_RT2, PUR_RT2_D1 = PUR_RT2)}
  
  if (Time == 2){
    dD <- dD%>%
      select(MailP1, ExpName, OKRL_D2 = OKRL,
             MF_D2 = MF, MB_D2 = MB, MBun_D2 = MBun,
             PRC_D2 = PRC, PRR_D2 = PRR, PUC_D2 = PUC, PUR_D2 = PUR,
             PRC_RT1_D2 = PRC_RT1, PRR_RT1_D2 = PRR_RT1, PUC_RT1_D2 = PUC_RT1, PUR_RT1_D2 = PUR_RT1,
             PRC_RT2_D2 = PRC_RT2, PRR_RT2_D2 = PRR_RT2, PUC_RT2_D2 = PUC_RT2, PUR_RT2_D2 = PUR_RT2)}
  return(dD)
}