# Initialization
source("DMG_Init.R")
Datapath = "Data/LimeSurveyQuestionnaires/Raw/"
Output_path = "Data/LimeSurveyQuestionnaires/Processed/"

########## Prep dataframe
# Import LimeSurvey data
dLS3 <- read.csv(paste0(Datapath, "ResultLS3.csv"), encoding="UTF-8")

# Rename and select columns
colnames(dLS3)[1] <- "NS"
TestMail <- c("test", "test ", "Test", "TEST", "TEST ", "TEST 2", "TEST3", "test 3", "testbis", "de",
              "https://survey.ulb.ac.be/survey3/index.php/388295")

dLS3 <- dLS3 %>%
  filter(lastpage == 10)%>% # Remove unfinished
  filter(!ID01 %in% TestMail)

# Main df
dLS3 <- dLS3%>%
  select(NS, Mail3 = ID01, "CoH.CoH01.":colnames(dLS3[length(dLS3)]))

# Remove duplicates
dLS3 = dLS3[order(dLS3[,'Mail3'],-dLS3[,'NS']),]
dLS3 = dLS3[!duplicated(dLS3$Mail3),]
dLS3 = dLS3[order(dLS3[,'NS']),]  

# Change badly spelled email if needed
for (i in c(1:length(dLS3$Mail3))) {
  if (dLS3$Mail3[i] %in% names(MailToChange)){
    dLS3$Mail3[i] <- MailToChange[[dLS3$Mail3[i]]]
  }
}

########## Columns handling
##### Test
dLS3 <- rename(dLS3, Test01 = UPPS.Test01., Test02 = PANAS.Test02.)

##### CoH
dCoH <- select(dLS3, NS, CoH.CoH01.:CoH.CoH27.)

for (i in colnames(dCoH[2:length(dCoH)])) {
  dCoH[,i] <- str_remove(dCoH[,i], "A")
  dCoH[,i] <- as.numeric(dCoH[,i])
  dCoH[,i] <- dCoH[,i] -1
}

dCoH <- dCoH%>%
  mutate(Auto = CoH.CoH03. + CoH.CoH05. + CoH.CoH09. + CoH.CoH11. + CoH.CoH16. +
                 CoH.CoH19. + CoH.CoH23. + CoH.CoH25. + CoH.CoH26.,
         Routine = CoH.CoH01. + CoH.CoH02. + CoH.CoH04. + CoH.CoH06. + CoH.CoH07. +
           CoH.CoH10. + CoH.CoH12. + CoH.CoH14. + CoH.CoH15. + CoH.CoH18. + CoH.CoH20. + CoH.CoH22. + CoH.CoH27.)

##### UPPS
dUPPS <- select(dLS3, NS, UPPS.UPPS01.:UPPS.UPPS20.)

for (i in colnames(dUPPS[2:length(dUPPS)])) {
  dUPPS[,i] <- str_remove(dUPPS[,i], "U01")
  dUPPS[,i] <- as.numeric(dUPPS[,i])
}

dUPPS <- dUPPS%>%
  mutate(NegUr = 5-UPPS.UPPS04. + 5-UPPS.UPPS07. + 5-UPPS.UPPS12. + 5-UPPS.UPPS17.,
         PosUr = 5-UPPS.UPPS02. + 5-UPPS.UPPS10. + 5-UPPS.UPPS15. + 5-UPPS.UPPS20.,
         LoPr = UPPS.UPPS01. + UPPS.UPPS06. + UPPS.UPPS13. + UPPS.UPPS19.,
         LoPe = UPPS.UPPS05. + UPPS.UPPS08. + UPPS.UPPS11. + UPPS.UPPS16.,
         SS = 5-UPPS.UPPS03. + 5-UPPS.UPPS09. + 5-UPPS.UPPS14. + 5-UPPS.UPPS18.,
         UPPS = NegUr + PosUr + LoPr + LoPe + SS)

##### CAST
dCAST <- select(dLS3, NS, CAST0:CAST06)

for (i in colnames(dCAST[2:length(dCAST)])) {
  dCAST[,i] <- str_remove(dCAST[,i], "C0")
  dCAST[,i] <- as.numeric(dCAST[,i])
}

dCAST <- dCAST%>%
  mutate(CAST = CAST01 + CAST02 + CAST03 + CAST04 + CAST05 + CAST06)

##### Fagertström
dFager <- select(dLS3, NS, F1:F6)

for (i in colnames(dFager[2:length(dFager)])) {
  dFager[,i] <- str_remove(dFager[,i], "A")
  dFager[,i] <- as.numeric(dFager[,i])
}

dFager$F1 = 4 - dFager$F1
dFager$F2 = 2 - dFager$F2
dFager$F3 = 2 - dFager$F3
dFager$F4 = dFager$F4 - 1
dFager$F5 = 2 - dFager$F5
dFager$F6 = 2 - dFager$F6

dFager <- dFager%>%
  mutate(Fager = F1 + F2 + F3 + F4 + F5 + F6)

##### Beck
dBeck <- select(dLS3, NS, B1:B13)

for (i in colnames(dBeck[2:length(dBeck)])) {
  dBeck[,i] <- str_remove(dBeck[,i], "A")
  dBeck[,i] <- as.numeric(dBeck[,i])
  dBeck[,i] <- dBeck[,i] - 1
}

dBeck <- dBeck%>%
  mutate(BDI = B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 + B13)

##### PANAS
dPANAS <- select(dLS3, NS, PANAS.PANAS01.:PANAS.PANAS20.)

for (i in colnames(dPANAS[2:length(dPANAS)])) {
  dPANAS[,i] <- str_remove(dPANAS[,i], "A")
  dPANAS[,i] <- as.numeric(dPANAS[,i])
}

dPANAS <- dPANAS%>%
  mutate(PosAff = PANAS.PANAS01. + PANAS.PANAS03. + PANAS.PANAS05. + PANAS.PANAS09. + PANAS.PANAS10. +
           PANAS.PANAS12. + PANAS.PANAS14. + PANAS.PANAS16. + PANAS.PANAS17. + PANAS.PANAS19.,
         NegAff = PANAS.PANAS02. + PANAS.PANAS04. + PANAS.PANAS06. + PANAS.PANAS07. + PANAS.PANAS08. +
           PANAS.PANAS11. + PANAS.PANAS13. + PANAS.PANAS15. + PANAS.PANAS18. + PANAS.PANAS20.)

##### CERQ
dCERQ <- select(dLS3, NS, CERQ1.CERQ1.:CERQ1.CERQ36.)

for (i in colnames(dCERQ[2:length(dCERQ)])) {
  dCERQ[,i] <- str_remove(dCERQ[,i], "CRQO")
  dCERQ[,i] <- as.numeric(dCERQ[,i])
}

dCERQ <- dCERQ%>%
  mutate(CERQSelfBlame = CERQ1.CERQ1. + CERQ1.CERQ10. + CERQ1.CERQ19. + CERQ1.CERQ28.,
         CERQAcceptation = CERQ1.CERQ2. + CERQ1.CERQ11. + CERQ1.CERQ20. + CERQ1.CERQ29.,
         CERQRumination = CERQ1.CERQ3. + CERQ1.CERQ12. + CERQ1.CERQ21. + CERQ1.CERQ30.,
         CERQPosCentration = CERQ1.CERQ4. + CERQ1.CERQ13. + CERQ1.CERQ22. + CERQ1.CERQ31.,
         CERQActionCentration = CERQ1.CERQ5. + CERQ1.CERQ14. + CERQ1.CERQ23. + CERQ1.CERQ32.,
         CERQPosReevaluation = CERQ1.CERQ6. + CERQ1.CERQ15. + CERQ1.CERQ24. + CERQ1.CERQ33.,
         CERQPerspective = CERQ1.CERQ7. + CERQ1.CERQ16. + CERQ1.CERQ25. + CERQ1.CERQ34.,
         CERQDramatization = CERQ1.CERQ8. + CERQ1.CERQ17. + CERQ1.CERQ26. + CERQ1.CERQ35.,
         CERQOtherBlame = CERQ1.CERQ9. + CERQ1.CERQ18. + CERQ1.CERQ27. + CERQ1.CERQ36.,
         CERQAdaptative = CERQAcceptation + CERQPosCentration +
           CERQActionCentration + CERQPosReevaluation + CERQPerspective,
         CERQNonAdaptative = CERQSelfBlame + CERQRumination + CERQDramatization + CERQOtherBlame,
  )

##### AUDIT
dAUDIT <- select(dLS3, NS, AUDIT01:AUDIT10)

# AUDIT01
dAUDIT$AUDIT01[dAUDIT$AUDIT01 == "A1"] <- 0
dAUDIT$AUDIT01[dAUDIT$AUDIT01 == "A2"] <- 1
dAUDIT$AUDIT01[dAUDIT$AUDIT01 == "A3"] <- 2
dAUDIT$AUDIT01[dAUDIT$AUDIT01 == "A4"] <- 3
dAUDIT$AUDIT01[dAUDIT$AUDIT01 == "A5"] <- 4
dAUDIT$AUDIT01[dAUDIT$AUDIT01 == ""] <- NA

dAUDIT$AUDIT01 <- as.numeric(dAUDIT$AUDIT01)

# AUDIT02
dAUDIT$AUDIT02[dAUDIT$AUDIT02 == "A0"] <- 0
dAUDIT$AUDIT02[dAUDIT$AUDIT02 == "A1"] <- 1
dAUDIT$AUDIT02[dAUDIT$AUDIT02 == "A2"] <- 2
dAUDIT$AUDIT02[dAUDIT$AUDIT02 == "A3"] <- 3
dAUDIT$AUDIT02[dAUDIT$AUDIT02 == "A4"] <- 4
dAUDIT$AUDIT02[dAUDIT$AUDIT02 == ""] <- NA

dAUDIT$AUDIT02 <- as.numeric(dAUDIT$AUDIT02)

# AUDIT03
dAUDIT$AUDIT03[dAUDIT$AUDIT03 == "A031"] <- 0
dAUDIT$AUDIT03[dAUDIT$AUDIT03 == "A032"] <- 1
dAUDIT$AUDIT03[dAUDIT$AUDIT03 == "A033"] <- 2
dAUDIT$AUDIT03[dAUDIT$AUDIT03 == "A034"] <- 3
dAUDIT$AUDIT03[dAUDIT$AUDIT03 == "A035"] <- 4
dAUDIT$AUDIT03[dAUDIT$AUDIT03 == ""] <- NA

dAUDIT$AUDIT03 <- as.numeric(dAUDIT$AUDIT03)

# AUDIT04
dAUDIT$AUDIT04[dAUDIT$AUDIT04 == "A041"] <- 0
dAUDIT$AUDIT04[dAUDIT$AUDIT04 == "A042"] <- 1
dAUDIT$AUDIT04[dAUDIT$AUDIT04 == "A043"] <- 2
dAUDIT$AUDIT04[dAUDIT$AUDIT04 == "A044"] <- 3
dAUDIT$AUDIT04[dAUDIT$AUDIT04 == "A045"] <- 4
dAUDIT$AUDIT04[dAUDIT$AUDIT04 == ""] <- NA

dAUDIT$AUDIT04 <- as.numeric(dAUDIT$AUDIT04)

# AUDIT05
dAUDIT$AUDIT05[dAUDIT$AUDIT05 == "A051"] <- 0
dAUDIT$AUDIT05[dAUDIT$AUDIT05 == "A052"] <- 1
dAUDIT$AUDIT05[dAUDIT$AUDIT05 == "A053"] <- 2
dAUDIT$AUDIT05[dAUDIT$AUDIT05 == "A054"] <- 3
dAUDIT$AUDIT05[dAUDIT$AUDIT05 == "A055"] <- 4
dAUDIT$AUDIT05[dAUDIT$AUDIT05 == ""] <- NA

dAUDIT$AUDIT05 <- as.numeric(dAUDIT$AUDIT05)

# AUDIT06
dAUDIT$AUDIT06[dAUDIT$AUDIT06 == "A061"] <- 0
dAUDIT$AUDIT06[dAUDIT$AUDIT06 == "A062"] <- 1
dAUDIT$AUDIT06[dAUDIT$AUDIT06 == "A063"] <- 2
dAUDIT$AUDIT06[dAUDIT$AUDIT06 == "A064"] <- 3
dAUDIT$AUDIT06[dAUDIT$AUDIT06 == "A065"] <- 4
dAUDIT$AUDIT06[dAUDIT$AUDIT06 == ""] <- NA

dAUDIT$AUDIT06 <- as.numeric(dAUDIT$AUDIT06)

# AUDIT07
dAUDIT$AUDIT07[dAUDIT$AUDIT07 == "A071"] <- 0
dAUDIT$AUDIT07[dAUDIT$AUDIT07 == "A072"] <- 1
dAUDIT$AUDIT07[dAUDIT$AUDIT07 == "A073"] <- 2
dAUDIT$AUDIT07[dAUDIT$AUDIT07 == "A074"] <- 3
dAUDIT$AUDIT07[dAUDIT$AUDIT07 == "A075"] <- 4
dAUDIT$AUDIT07[dAUDIT$AUDIT07 == ""] <- NA

dAUDIT$AUDIT07 <- as.numeric(dAUDIT$AUDIT07)

# AUDIT08
dAUDIT$AUDIT08[dAUDIT$AUDIT08 == "A081"] <- 0
dAUDIT$AUDIT08[dAUDIT$AUDIT08 == "A082"] <- 1
dAUDIT$AUDIT08[dAUDIT$AUDIT08 == "A083"] <- 2
dAUDIT$AUDIT08[dAUDIT$AUDIT08 == "A084"] <- 3
dAUDIT$AUDIT08[dAUDIT$AUDIT08 == "A085"] <- 4
dAUDIT$AUDIT08[dAUDIT$AUDIT08 == ""] <- NA

dAUDIT$AUDIT08 <- as.numeric(dAUDIT$AUDIT08)

# AUDIT09
dAUDIT$AUDIT09[dAUDIT$AUDIT09 == "A091"] <- 0
dAUDIT$AUDIT09[dAUDIT$AUDIT09 == "A092"] <- 2
dAUDIT$AUDIT09[dAUDIT$AUDIT09 == "A093"] <- 4
dAUDIT$AUDIT09[dAUDIT$AUDIT09 == ""] <- NA

dAUDIT$AUDIT09 <- as.numeric(dAUDIT$AUDIT09)

# AUDIT10
dAUDIT$AUDIT10[dAUDIT$AUDIT10 == "A101"] <- 0
dAUDIT$AUDIT10[dAUDIT$AUDIT10 == "A102"] <- 2
dAUDIT$AUDIT10[dAUDIT$AUDIT10 == "A103"] <- 4
dAUDIT$AUDIT10[dAUDIT$AUDIT10 == ""] <- NA

dAUDIT$AUDIT10 <- as.numeric(dAUDIT$AUDIT10)

dAUDIT <- mutate(dAUDIT, AUDIT = AUDIT01 + AUDIT02 + AUDIT03 + AUDIT04 + AUDIT05 + AUDIT06 + AUDIT07 + AUDIT08 + AUDIT09 + AUDIT10)

##### Check if tests OK
dTest <- select(dLS3, NS, Test01, Test02)
dTest <- AddDummyCol(dTest, "TestQOK", 0)

dTest$Test01[dTest$Test01 != "U013"] <- 0
dTest$Test01[dTest$Test01 == "U013"] <- 1
dTest$Test01 <- as.numeric(dTest$Test01)

dTest$Test02[dTest$Test02 != "A2"] <- 0
dTest$Test02[dTest$Test02 == "A2"] <- 1
dTest$Test02 <- as.numeric(dTest$Test02)

dTest <- mutate(dTest, Testok = Test01 + Test02)
dTest$TestQOK[dTest$Testok == 2] <- 1

########## Final Frame
dF <- cbind(dLS3[c(1, 2)], dTest[c("TestQOK", "Test01", "Test02")],
            dCAST["CAST"], dFager["Fager"], dAUDIT["AUDIT"],
            dCoH[c("Auto", "Routine")], dUPPS[c("UPPS", "NegUr", "PosUr", "LoPr", "LoPe", "SS")], dPANAS[c("PosAff", "NegAff")],
            dBeck["BDI"], dCERQ[c("CERQAdaptative", "CERQNonAdaptative")])

dMailLS3 <- select(dLS3, Mail3)

########## Export
write.table(dF, paste0(Output_path, "dLS3.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dMailLS3, "AdditionalInfo/MailList/MailLS3.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
