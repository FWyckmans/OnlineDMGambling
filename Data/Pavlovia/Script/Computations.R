# Computation
##### Initialization
source("DMG_Init.R")
Datapath = "Data/Pavlovia/Processed/"

# Packages
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
# install.packages("hBayesDM", dependencies=TRUE)

# Sys.setenv(BUILD_ALL='true')  # Build all the models on installation
# Sys.setenv(MAKEFLAGS='-j 4')  # Use 4 cores for compilation (or the number you want)
# 
# install.packages("hBayesDM")  # Install from CRAN

library(hBayesDM)

# Function
RLComp <- function(d, Time, i){
  if (length(d[[1]])>0){
    # MB MF
    RegLogInd <- glm(Stay ~ PrReward*PrTransition,
                     family = binomial(logit), data = d)
    
    MB = exp(coef(RegLogInd))[4]
    MF = exp(coef(RegLogInd))[2]
    pred = predict(RegLogInd)
    Conf_Mat <- ConfMat(d, pred, DV = "Stay")
    RL_Metric <- RLMetrics(Conf_Mat, pred)
    AUC <- RL_Metric$AUC
    
    # Unrewarded MB
    d <- filter(d, PrReward == -1)
    
    RegLogInd <- glm(Stay ~ PrTransition,
                     family = binomial(logit), data = d)
    
    MBUn = exp(coef(RegLogInd))[2]
    pred = predict(RegLogInd)
    Conf_Mat <- ConfMat(d, pred, DV = "Stay")
    RL_Metric <- RLMetrics(Conf_Mat, pred = pred)
    AUCun <- RL_Metric$AUC
    
    # Fill dF
    if ((dCompFinal$FirstVid[i] == "Gambling") & (Time == 1)){
      dCompFinal$MFRL_AfterGamb[i] <<- MF
      dCompFinal$MBRL_AfterGamb[i] <<- MB
      dCompFinal$AUC_AfterGamb[i] <<- AUC
      dCompFinal$MBunRL_AfterGamb[i] <<- MBUn
      dCompFinal$AUCun_AfterGamb[i] <<- AUCun
    }
    
    if ((dCompFinal$FirstVid[i] == "Neutral") & (Time == 1)){
      dCompFinal$MFRL_AfterNeu[i] <<- MF
      dCompFinal$MBRL_AfterNeu[i] <<- MB
      dCompFinal$AUC_AfterNeu[i] <<- AUC
      dCompFinal$MBunRL_AfterNeu[i] <<- MBUn
      dCompFinal$AUCun_AfterNeu[i] <<- AUCun
    }
    
    if ((dCompFinal$FirstVid[i] == "Gambling") & (Time == 2)){
      dCompFinal$MFRL_AfterNeu[i] <<- MF
      dCompFinal$MBRL_AfterNeu[i] <<- MB
      dCompFinal$AUC_AfterNeu[i] <<- AUC
      dCompFinal$MBunRL_AfterNeu[i] <<- MBUn
      dCompFinal$AUCun_AfterNeu[i] <<- AUCun
    }
    
    if ((dCompFinal$FirstVid[i] == "Neutral") & (Time == 2)){
      dCompFinal$MFRL_AfterGamb[i] <<- MF
      dCompFinal$MBRL_AfterGamb[i] <<- MB
      dCompFinal$AUC_AfterGamb[i] <<- AUC
      dCompFinal$MBunRL_AfterGamb[i] <<- MBUn
      dCompFinal$AUCun_AfterGamb[i] <<- AUCun
    }
  }
}

##### Data Frame
# Import
dComp1 <- read.delim(paste0(Datapath, "Comp1.txt"))
dComp2 <- read.delim(paste0(Datapath, "Comp2.txt"))
dTot <- read.delim("Output/dTotAnonyme.txt")
dTotA <- read.delim("Output/dTot.txt")

# Big dF
dComp <- rbind(dComp1, dComp2)

dComp <- filter(dComp, Mail %in% dTot$Mail1)

# Empty dF
if (is.null(dTot$a1_AfterGamb)){
  dTot <- AddDummyCol(dTot, c("a1_AfterGamb", "beta1_AfterGamb", "a2_AfterGamb", "beta2_AfterGamb",
                              "w_AfterGamb", "pi_AfterGamb", "lambda_AfterGamb",
                              "a1_AfterNeu", "beta1_AfterNeu", "a2_AfterNeu", "beta2_AfterNeu",
                              "w_AfterNeu", "pi_AfterNeu", "lambda_AfterNeu",
                              "MFRL_AfterGamb", "MBRL_AfterGamb", "AUC_AfterGamb","MBunRL_AfterGamb", "AUCun_AfterGamb",
                              "MFRL_AfterNeu", "MBRL_AfterNeu", "AUC_AfterNeu", "MBunRL_AfterNeu", "AUCun_AfterNeu"))
  dTotA <- AddDummyCol(dTotA, c("a1_AfterGamb", "beta1_AfterGamb", "a2_AfterGamb", "beta2_AfterGamb",
                                "w_AfterGamb", "pi_AfterGamb", "lambda_AfterGamb",
                                "a1_AfterNeu", "beta1_AfterNeu", "a2_AfterNeu", "beta2_AfterNeu",
                                "w_AfterNeu", "pi_AfterNeu", "lambda_AfterNeu",
                                "MFRL_AfterGamb", "MBRL_AfterGamb", "AUC_AfterGamb","MBunRL_AfterGamb", "AUCun_AfterGamb",
                                "MFRL_AfterNeu", "MBRL_AfterNeu", "AUC_AfterNeu", "MBunRL_AfterNeu", "AUCun_AfterNeu"))
}

if ("dCompFinal.txt" %in% dir("Output/")){
  dCompFinal <- read.delim("Output/dCompFinal.txt")
} else { 
  dCompFinal <- dTot[,c(1, 2, 3,
                        FromColNameToIndex(dTot, "a1_AfterGamb"):FromColNameToIndex(dTot, "AUCun_AfterNeu"))]}

ColRL <- colnames(dTot[c(FromColNameToIndex(dTot, "MFRL_AfterGamb"):FromColNameToIndex(dTot, "AUCun_AfterNeu"))])
ColCom <- colnames(dTot[c(FromColNameToIndex(dTot, "a1_AfterGamb"):FromColNameToIndex(dTot, "lambda_AfterNeu"))])

###### Prepare dF for computations
# Change label for PrReward and PrTransition
dComp$PrReward[dComp$PrReward == 0] <- -1
dComp$PrTransition[dComp$PrTransition == 0] <- -1

# Add a subjID column
dComp <- AddDummyCol(dComp, "subjID", NA)

for (i in unique(dComp$Mail)){
  dComp$subjID[dComp$Mail==i] <- dTot$NS[dTot$Mail1==i]
}

# Change Choice 1 values
dComp$Choice1[dComp$Choice1 == "e"] <- 1
dComp$Choice1[dComp$Choice1 == "i"] <- 2
dComp$Choice1 <- as.numeric(dComp$Choice1)

# Change Choice 2 values
dComp$Choice2[((dComp$Choice1 == 1) & (dComp$transition == 1) & (dComp$Choice2 == "e"))] <- 1
dComp$Choice2[((dComp$Choice1 == 1) & (dComp$transition == 1) & (dComp$Choice2 == "i"))] <- 2

dComp$Choice2[((dComp$Choice1 == 1) & (dComp$transition == 0) & (dComp$Choice2 == "e"))] <- 3
dComp$Choice2[((dComp$Choice1 == 1) & (dComp$transition == 0) & (dComp$Choice2 == "i"))] <- 4

dComp$Choice2[((dComp$Choice1 == 2) & (dComp$transition == 1) & (dComp$Choice2 == "e"))] <- 3
dComp$Choice2[((dComp$Choice1 == 2) & (dComp$transition == 1) & (dComp$Choice2 == "i"))] <- 4

dComp$Choice2[((dComp$Choice1 == 2) & (dComp$transition == 0) & (dComp$Choice2 == "e"))] <- 1
dComp$Choice2[((dComp$Choice1 == 2) & (dComp$transition == 0) & (dComp$Choice2 == "i"))] <- 2

dComp$Choice2 <- as.numeric(dComp$Choice2)

# Rename cols
dComp <- rename(dComp, level1_choice = Choice1, level2_choice = Choice2)

##### Main loop
for (i in c(unique(dCompFinal$Mail1))) {
  dPS <- filter(dComp, Mail == dCompFinal$Mail1[i])
  dPS1 <- filter(dPS, Time == 1)
  dPS2 <- filter(dPS, Time == 2)
  
  # RL computations
  if (T %in% is.na(dCompFinal[i, FromColNameToIndex(dCompFinal, "MFRL_AfterGamb"):FromColNameToIndex(dCompFinal, "MBRL_AfterNeu")])){
    RLComp(d = dPS1, Time = 1, i)
    RLComp(dPS2, Time = 2, i)
  }
}

# dTot <- cbind(dTot[ , -which(names(dTot) %in% c(ColRL, ColCom))], dCompFinal[c(4:length(dCompFinal))])
# dTotA <- cbind(dTotA[ , -which(names(dTotA) %in% c(ColRL, ColCom))], dCompFinal[c(4:length(dCompFinal))])

##### MBMF computation
### dF
NotDone <- dCompFinal$NS[is.na(dCompFinal$a1_AfterGamb)]

dCompAfGamb <- dComp%>%
  filter((ExpName == "InstructionMarkovVideoG" & Time == 1) | (ExpName == "MarkovTaskGamblingJ2" & Time == 2))%>%
  filter(subjID %in% NotDone)

dCompAfNeu <- dComp%>%
  filter((ExpName == "InstructionMarkovVideoN" & Time == 1) | (ExpName == "MarkovTaskNeutreJ2" & Time == 2))%>%
  filter(subjID %in% NotDone)

niter = 4000
nwarmup = niter/2

### Test, comment if not needed
# dCompAfAlc <- filter(dCompAfAlc, ((dCompAfAlc$subjID == 124) | (dCompAfAlc$subjID == 46) | (dCompAfAlc$subjID == 54)))
# dCompAfNeu <- filter(dCompAfNeu, ((dCompAfNeu$subjID == 124) | (dCompAfNeu$subjID == 46) | (dCompAfNeu$subjID == 54)))

### AlcFirst
# Comp
outputAfGamb <- ts_par7(data = dCompAfGamb, niter = 4000, nwarmup = 200, nchain = 4, ncore = 4)
dOutputAfGamb <- outputAfGamb$allIndPars

# Fill dF
for (i in dOutputAfGamb[["subjID"]]) {
  dTot$a1_AfterGamb[dTot$NS == i] <- dOutputAfGamb$a1[dOutputAfGamb$subjID == i]
  dTot$beta1_AfterGamb[dTot$NS == i] <- dOutputAfGamb$beta1[dOutputAfGamb$subjID == i]
  dTot$a2_AfterGamb[dTot$NS == i] <- dOutputAfGamb$a2[dOutputAfGamb$subjID == i]
  dTot$beta2_AfterGamb[dTot$NS == i] <- dOutputAfGamb$beta2[dOutputAfGamb$subjID == i]
  dTot$w_AfterGamb[dTot$NS == i] <- dOutputAfGamb$w[dOutputAfGamb$subjID == i]
  dTot$pi_AfterGamb[dTot$NS == i] <- dOutputAfGamb$pi[dOutputAfGamb$subjID == i]
  dTot$lambda_AfterGamb[dTot$NS == i] <- dOutputAfGamb$lambda[dOutputAfGamb$subjID == i]
  
  dTotA$a1_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$a1[dOutputAfGamb$subjID == i]
  dTotA$beta1_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$beta1[dOutputAfGamb$subjID == i]
  dTotA$a2_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$a2[dOutputAfGamb$subjID == i]
  dTotA$beta2_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$beta2[dOutputAfGamb$subjID == i]
  dTotA$w_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$w[dOutputAfGamb$subjID == i]
  dTotA$pi_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$pi[dOutputAfGamb$subjID == i]
  dTotA$lambda_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$lambda[dOutputAfGamb$subjID == i]
  
  dCompFinal$a1_AfterGamb[dCompFinal$NS == i] <- dOutputAfGamb$a1[dOutputAfGamb$subjID == i]
  dCompFinal$beta1_AfterGamb[dCompFinal$NS == i] <- dOutputAfGamb$beta1[dOutputAfGamb$subjID == i]
  dCompFinal$a2_AfterGamb[dCompFinal$NS == i] <- dOutputAfGamb$a2[dOutputAfGamb$subjID == i]
  dCompFinal$beta2_AfterGamb[dCompFinal$NS == i] <- dOutputAfGamb$beta2[dOutputAfGamb$subjID == i]
  dCompFinal$w_AfterGamb[dCompFinal$NS == i] <- dOutputAfGamb$w[dOutputAfGamb$subjID == i]
  dCompFinal$pi_AfterGamb[dCompFinal$NS == i] <- dOutputAfGamb$pi[dOutputAfGamb$subjID == i]
  dCompFinal$lambda_AfterGamb[dCompFinal$NS == i] <- dOutputAfGamb$lambda[dOutputAfGamb$subjID == i]
}

# Write tables
write.table(dTot, "Output/dTotAnonyme.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dTotA, "Output/dTot.txt", col.names = T, row.names = F, sep = "\t", dec = ".")

dCompFinal <- dCompFinal[ , -which(names(dCompFinal) == "Mail1")]
write.table(dCompFinal, "Output/dCompFinal.txt", col.names = T, row.names = F, sep = "\t", dec = ".")

### Neutral first
# Comp
outputAfNeu <- ts_par7(data = dCompAfNeu, niter = 3000, nwarmup = 1500, nchain = 4, ncore = 4)
dOutputAfNeu <- outputAfNeu$allIndPars

# Fill dF
for (i in dOutputAfNeu[["subjID"]]) {
  dTot$a1_AfterNeu[dTot$NS == i] <- dOutputAfNeu$a1[dOutputAfNeu$subjID == i]
  dTot$beta1_AfterNeu[dTot$NS == i] <- dOutputAfNeu$beta1[dOutputAfNeu$subjID == i]
  dTot$a2_AfterNeu[dTot$NS == i] <- dOutputAfNeu$a2[dOutputAfNeu$subjID == i]
  dTot$beta2_AfterNeu[dTot$NS == i] <- dOutputAfNeu$beta2[dOutputAfNeu$subjID == i]
  dTot$w_AfterNeu[dTot$NS == i] <- dOutputAfNeu$w[dOutputAfNeu$subjID == i]
  dTot$pi_AfterNeu[dTot$NS == i] <- dOutputAfNeu$pi[dOutputAfNeu$subjID == i]
  dTot$lambda_AfterNeu[dTot$NS == i] <- dOutputAfNeu$lambda[dOutputAfNeu$subjID == i]
  
  dTotA$a1_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$a1[dOutputAfNeu$subjID == i]
  dTotA$beta1_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$beta1[dOutputAfNeu$subjID == i]
  dTotA$a2_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$a2[dOutputAfNeu$subjID == i]
  dTotA$beta2_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$beta2[dOutputAfNeu$subjID == i]
  dTotA$w_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$w[dOutputAfNeu$subjID == i]
  dTotA$pi_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$pi[dOutputAfNeu$subjID == i]
  dTotA$lambda_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$lambda[dOutputAfNeu$subjID == i]
  
  dCompFinal$a1_AfterNeu[dCompFinal$NS == i] <- dOutputAfNeu$a1[dOutputAfNeu$subjID == i]
  dCompFinal$beta1_AfterNeu[dCompFinal$NS == i] <- dOutputAfNeu$beta1[dOutputAfNeu$subjID == i]
  dCompFinal$a2_AfterNeu[dCompFinal$NS == i] <- dOutputAfNeu$a2[dOutputAfNeu$subjID == i]
  dCompFinal$beta2_AfterNeu[dCompFinal$NS == i] <- dOutputAfNeu$beta2[dOutputAfNeu$subjID == i]
  dCompFinal$w_AfterNeu[dCompFinal$NS == i] <- dOutputAfNeu$w[dOutputAfNeu$subjID == i]
  dCompFinal$pi_AfterNeu[dCompFinal$NS == i] <- dOutputAfNeu$pi[dOutputAfNeu$subjID == i]
  dCompFinal$lambda_AfterNeu[dCompFinal$NS == i] <- dOutputAfNeu$lambda[dOutputAfNeu$subjID == i]
}

# Write tables
write.table(dTot, "Output/dTotAnonyme.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dTotA, "Output/dTot.txt", col.names = T, row.names = F, sep = "\t", dec = ".")

dCompFinal <- dCompFinal[ , -which(names(dCompFinal) == "Mail1")]
write.table(dCompFinal, "Output/dCompFinal.txt", col.names = T, row.names = F, sep = "\t", dec = ".")


##### Prepare df for total reglog
dRegLog <- AddDummyCol(dComp, colnames(dTot[3:length(dTot)]), NA)

for (i in colnames(dRegLog[FromColNameToIndex(dRegLog, "FirstVid"):length(dRegLog)])) {
  for (m in unique(dRegLog$Mail)) {
    dRegLog[dRegLog$Mail == m, i] <- dTot[dTot$Mail1==m, i]
  }
}

dRegLog$Time[(dRegLog$Time == 1 & dRegLog$FirstVid=="Gambling")|
               (dRegLog$Time == 2 & dRegLog$FirstVid=="Neutral")] <- "AfterGambling"
dRegLog$Time[(dRegLog$Time == 1 & dRegLog$FirstVid=="Neutral")|
               (dRegLog$Time == 2 & dRegLog$FirstVid=="Gambling")] <- "AfterNeutral"

dRegLog <- dRegLog[ , -which(names(dRegLog) == "Mail")]

write.table(dRegLog, "Output/dRegLog.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
