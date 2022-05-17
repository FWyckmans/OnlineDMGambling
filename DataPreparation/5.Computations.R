# Computation
##### Initialization
source("DMG_Init.R")
Datapath = "Output/Processed/"
DoComp = FALSE

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

##### Data Frame
# Import
dComp1 <- read.delim(paste0(Datapath, "Comp1A.txt"))
dComp2 <- read.delim(paste0(Datapath, "Comp2A.txt"))
dTotA <- read.delim("Output/Processed/dTot.txt")

if (WithAnonyme == TRUE){
  dTot <- read.delim("Output/Anonyme/dTotAnonyme.txt")
}

# Big dF
dComp <- rbind(dComp1, dComp2)

dComp <- filter(dComp, subjID %in% dTotA$NS)

# Empty dF
dTotA <- AddDummyCol(dTotA, c("a1_AfterGamb", "beta1_AfterGamb", "a2_AfterGamb", "beta2_AfterGamb",
                              "w_AfterGamb", "pi_AfterGamb", "lambda_AfterGamb",
                              "a1_AfterNeu", "beta1_AfterNeu", "a2_AfterNeu", "beta2_AfterNeu",
                              "w_AfterNeu", "pi_AfterNeu", "lambda_AfterNeu",
                              "MFRL_AfterGamb", "MBRL_AfterGamb", "MBunRL_AfterGamb",
                              "MFRL_AfterNeu", "MBRL_AfterNeu", "MBunRL_AfterNeu"))

if (WithAnonyme == TRUE){
  dTot <- read.delim("Output/Anonyme/dTotAnonyme.txt")
  dTot <- AddDummyCol(dTot, c("a1_AfterGamb", "beta1_AfterGamb", "a2_AfterGamb", "beta2_AfterGamb",
                              "w_AfterGamb", "pi_AfterGamb", "lambda_AfterGamb",
                              "a1_AfterNeu", "beta1_AfterNeu", "a2_AfterNeu", "beta2_AfterNeu",
                              "w_AfterNeu", "pi_AfterNeu", "lambda_AfterNeu",
                              "MFRL_AfterGamb", "MBRL_AfterGamb", "MBunRL_AfterGamb",
                              "MFRL_AfterNeu", "MBRL_AfterNeu", "MBunRL_AfterNeu"))
} 

###### Prepare dF for computations
# Change label for PrReward and PrTransition
dComp$PrReward[dComp$PrReward == 0] <- -1
dComp$PrTransition[dComp$PrTransition == 0] <- -1

# # Add a subjID column
# dComp <- AddDummyCol(dComp, "subjID", NA)
# 
# for (i in unique(dComp$Mail)){
#   dComp$subjID[dComp$Mail==i] <- dTot$NS[dTot$Mail1==i]
# }

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
write.table(dComp, "Output/Processed/dCompR.txt", col.names = T, row.names = F, sep = "\t", dec = ".")

##### MBMF computation
### dF
# NotDone <- dCompFinal$NS[is.na(dCompFinal$a1_AfterGamb)]

dCompAfGamb <- dComp%>%
  filter((ExpName == "InstructionMarkovVideoG" & Time == 1) | (ExpName == "MarkovTaskGamblingJ2" & Time == 2))

dCompAfNeu <- dComp%>%
  filter((ExpName == "InstructionMarkovVideoN" & Time == 1) | (ExpName == "MarkovTaskNeutreJ2" & Time == 2))

NIter = 2000
NWarmup = NIter/2

### AlcFirst
# Comp
if (DoComp == TRUE){
  outputAfGamb <- ts_par7(data = dCompAfGamb, niter = 4000, nwarmup = 2000, nchain = 4, ncore = 4)
  saveRDS(outputAfGamb, file="Output/Model/output7P_AfGamb.Rdata")
  
  }else{
    outputAfGamb <- readRDS(file="Output/Model/output7P_AfGamb.Rdata")
  }

dOutputAfGamb <- outputAfGamb$allIndPars
write.table(dOutputAfGamb, "Output/Processed/allIndParsG.txt", col.names = T, row.names = F, sep = "\t", dec = ".")

# Fill dF
for (i in dOutputAfGamb[["subjID"]]) {
  if (WithAnonyme == TRUE){
    dTot$a1_AfterGamb[dTot$NS == i] <- dOutputAfGamb$a1[dOutputAfGamb$subjID == i]
    dTot$beta1_AfterGamb[dTot$NS == i] <- dOutputAfGamb$beta1[dOutputAfGamb$subjID == i]
    dTot$a2_AfterGamb[dTot$NS == i] <- dOutputAfGamb$a2[dOutputAfGamb$subjID == i]
    dTot$beta2_AfterGamb[dTot$NS == i] <- dOutputAfGamb$beta2[dOutputAfGamb$subjID == i]
    dTot$w_AfterGamb[dTot$NS == i] <- dOutputAfGamb$w[dOutputAfGamb$subjID == i]
    dTot$pi_AfterGamb[dTot$NS == i] <- dOutputAfGamb$pi[dOutputAfGamb$subjID == i]
    dTot$lambda_AfterGamb[dTot$NS == i] <- dOutputAfGamb$lambda[dOutputAfGamb$subjID == i]}
  
  dTotA$a1_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$a1[dOutputAfGamb$subjID == i]
  dTotA$beta1_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$beta1[dOutputAfGamb$subjID == i]
  dTotA$a2_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$a2[dOutputAfGamb$subjID == i]
  dTotA$beta2_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$beta2[dOutputAfGamb$subjID == i]
  dTotA$w_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$w[dOutputAfGamb$subjID == i]
  dTotA$pi_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$pi[dOutputAfGamb$subjID == i]
  dTotA$lambda_AfterGamb[dTotA$NS == i] <- dOutputAfGamb$lambda[dOutputAfGamb$subjID == i]
}

# Write tables
if (WithAnonyme == TRUE){
  write.table(dTot, "Output/Anonyme/dTotCompAnonyme.txt", col.names = T, row.names = F, sep = "\t", dec = ".")}

write.table(dTotA, "Output/Processed/dTotComp.txt", col.names = T, row.names = F, sep = "\t", dec = ".")

### Neutral first
# Comp
if (DoComp == TRUE){
  outputAfNeu <- ts_par7(data = dCompAfNeu, niter = 4000, nwarmup = 2000, nchain = 4, ncore = 4)
  # Save Computations if needed
  saveRDS(outputAfNeu, file="Output/Model/output7P_AfNeu.Rdata")
  }else{
    outputAfNeu <- readRDS(file="Output/Model/output7P_AfNeu.Rdata")
  }

dOutputAfNeu <- outputAfNeu$allIndPars
write.table(dOutputAfNeu, "Output/Processed/allIndParsN.txt", col.names = T, row.names = F, sep = "\t", dec = ".")

# Fill dF
for (i in dOutputAfNeu[["subjID"]]) {
  if (WithAnonyme == TRUE){
    dTot$a1_AfterNeu[dTot$NS == i] <- dOutputAfNeu$a1[dOutputAfNeu$subjID == i]
    dTot$beta1_AfterNeu[dTot$NS == i] <- dOutputAfNeu$beta1[dOutputAfNeu$subjID == i]
    dTot$a2_AfterNeu[dTot$NS == i] <- dOutputAfNeu$a2[dOutputAfNeu$subjID == i]
    dTot$beta2_AfterNeu[dTot$NS == i] <- dOutputAfNeu$beta2[dOutputAfNeu$subjID == i]
    dTot$w_AfterNeu[dTot$NS == i] <- dOutputAfNeu$w[dOutputAfNeu$subjID == i]
    dTot$pi_AfterNeu[dTot$NS == i] <- dOutputAfNeu$pi[dOutputAfNeu$subjID == i]
    dTot$lambda_AfterNeu[dTot$NS == i] <- dOutputAfNeu$lambda[dOutputAfNeu$subjID == i]}
  
  dTotA$a1_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$a1[dOutputAfNeu$subjID == i]
  dTotA$beta1_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$beta1[dOutputAfNeu$subjID == i]
  dTotA$a2_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$a2[dOutputAfNeu$subjID == i]
  dTotA$beta2_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$beta2[dOutputAfNeu$subjID == i]
  dTotA$w_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$w[dOutputAfNeu$subjID == i]
  dTotA$pi_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$pi[dOutputAfNeu$subjID == i]
  dTotA$lambda_AfterNeu[dTotA$NS == i] <- dOutputAfNeu$lambda[dOutputAfNeu$subjID == i]
}

# Write tables
if (WithAnonyme == TRUE){
  write.table(dTot, "Output/Anonyme/dTotCompAnonyme.txt", col.names = T, row.names = F, sep = "\t", dec = ".")}

write.table(dTotA, "Output/Processed/dTotComp.txt", col.names = T, row.names = F, sep = "\t", dec = ".")