# Initialization
source("DMG_Init.R")
Datapath = "Data/LimeSurveyQuestionnaires/Raw/"
Output_path = "Data/LimeSurveyQuestionnaires/Processed/"

########## Prep dataframe
# Import LimeSurvey data
dLS1 <- read.csv(paste0(Datapath, "ResultLS1.csv"), encoding="UTF-8")

# Rename and select columns
colnames(dLS1)[1] <- "NS"
TestMail <- c("test", "test ", "Test", "TEST", "TEST ", "TEST 2", "TEST3", "test 3", "testbis", "de")

dLS1 <- dLS1 %>%
  filter(lastpage == 4)%>% # Remove unfinished
  filter(E2 == "A1")%>% # Remove not consenting
  filter(!E1 %in% TestMail)

# Main df
dLS1 <- select(dLS1, NS, Mail1 = E1, Age = IP01, Gender = IP02, StudyLvl = IP03, Work = IP06, Contactable = IP04, DrugUse = IP05,
               "AUDIT01", "AUDIT02", "AUDIT03", "AUDIT04", "AUDIT05", "AUDIT06", "AUDIT07", "AUDIT08", "AUDIT09", "AUDIT10",
               BD1 = QAU1, BD2 = QAU2, BD3 = QUA3, LastDrink = A6)

# Remove duplicates
dLS1 = dLS1[order(dLS1[,'Mail1'],-dLS1[,'NS']),]
dLS1 = dLS1[!duplicated(dLS1$Mail1),]
dLS1 = dLS1[order(dLS1[,'NS']),]

########## Columns handling
##### Demo
# Age
dLS1$Age <- str_remove(dLS1$Age, " ans")
dLS1$Age <- str_remove(dLS1$Age, "ans")

dLS1$Age <- as.numeric(dLS1$Age)

# Gender
dLS1$Gender[dLS1$Gender == "A1"] <- "Male"
dLS1$Gender[dLS1$Gender == "A2"] <- "Female"
dLS1$Gender[dLS1$Gender == "A3"] <- "Other"

dLS1$Gender <- factor(dLS1$Gender, ordered = F)

# StudyLevel
dLS1$StudyLvl[dLS1$StudyLvl == "A1"] <- 0
dLS1$StudyLvl[dLS1$StudyLvl == "A2"] <- 6
dLS1$StudyLvl[dLS1$StudyLvl == "A3"] <- 9
dLS1$StudyLvl[dLS1$StudyLvl == "A4"] <- 12
dLS1$StudyLvl[dLS1$StudyLvl == "A5"] <- 15
dLS1$StudyLvl[dLS1$StudyLvl == "A6"] <- 17
dLS1$StudyLvl[dLS1$StudyLvl == "A7"] <- 21

dLS1$StudyLvl <- as.numeric(dLS1$StudyLvl)

# Work
dLS1$Work[dLS1$Work == 1] <- "Student"
dLS1$Work[dLS1$Work == 2] <- "Workman"
dLS1$Work[dLS1$Work == 3] <- "Employee"
dLS1$Work[dLS1$Work == 4] <- "Executive"
dLS1$Work[dLS1$Work == 5] <- "Independent"
dLS1$Work[dLS1$Work == 6] <- "Unemployed"
dLS1$Work[dLS1$Work == 7] <- "Retirement"
dLS1$Work[dLS1$Work == 8] <- "Other"

dLS1$Work <- factor(dLS1$Work, ordered = F)

# Contactable
dLS1$Contactable[dLS1$Contactable == "A1"] <- "Yes"
dLS1$Contactable[dLS1$Contactable == "A2"] <- "No"

dLS1$Contactable <- factor(dLS1$Contactable, ordered = F)

# DrugUse
dLS1$DrugUse[dLS1$DrugUse == "A1"] <- "Yes"
dLS1$DrugUse[dLS1$DrugUse == "A2"] <- "No"

dLS1$DrugUse <- factor(dLS1$DrugUse, ordered = F)

##### AUDIT
# AUDIT01
dLS1$AUDIT01[dLS1$AUDIT01 == "A1"] <- 0
dLS1$AUDIT01[dLS1$AUDIT01 == "A2"] <- 1
dLS1$AUDIT01[dLS1$AUDIT01 == "A3"] <- 2
dLS1$AUDIT01[dLS1$AUDIT01 == "A4"] <- 3
dLS1$AUDIT01[dLS1$AUDIT01 == "A5"] <- 4
dLS1$AUDIT01[dLS1$AUDIT01 == ""] <- NA

dLS1$AUDIT01 <- as.numeric(dLS1$AUDIT01)

# AUDIT02
dLS1$AUDIT02[dLS1$AUDIT02 == "A0"] <- 0
dLS1$AUDIT02[dLS1$AUDIT02 == "A1"] <- 1
dLS1$AUDIT02[dLS1$AUDIT02 == "A2"] <- 2
dLS1$AUDIT02[dLS1$AUDIT02 == "A3"] <- 3
dLS1$AUDIT02[dLS1$AUDIT02 == "A4"] <- 4
dLS1$AUDIT02[dLS1$AUDIT02 == ""] <- NA

dLS1$AUDIT02 <- as.numeric(dLS1$AUDIT02)

# AUDIT03
dLS1$AUDIT03[dLS1$AUDIT03 == "A031"] <- 0
dLS1$AUDIT03[dLS1$AUDIT03 == "A032"] <- 1
dLS1$AUDIT03[dLS1$AUDIT03 == "A033"] <- 2
dLS1$AUDIT03[dLS1$AUDIT03 == "A034"] <- 3
dLS1$AUDIT03[dLS1$AUDIT03 == "A035"] <- 4
dLS1$AUDIT03[dLS1$AUDIT03 == ""] <- NA

dLS1$AUDIT03 <- as.numeric(dLS1$AUDIT03)

# AUDIT04
dLS1$AUDIT04[dLS1$AUDIT04 == "A041"] <- 0
dLS1$AUDIT04[dLS1$AUDIT04 == "A042"] <- 1
dLS1$AUDIT04[dLS1$AUDIT04 == "A043"] <- 2
dLS1$AUDIT04[dLS1$AUDIT04 == "A044"] <- 3
dLS1$AUDIT04[dLS1$AUDIT04 == "A045"] <- 4
dLS1$AUDIT04[dLS1$AUDIT04 == ""] <- NA

dLS1$AUDIT04 <- as.numeric(dLS1$AUDIT04)

# AUDIT05
dLS1$AUDIT05[dLS1$AUDIT05 == "A051"] <- 0
dLS1$AUDIT05[dLS1$AUDIT05 == "A052"] <- 1
dLS1$AUDIT05[dLS1$AUDIT05 == "A053"] <- 2
dLS1$AUDIT05[dLS1$AUDIT05 == "A054"] <- 3
dLS1$AUDIT05[dLS1$AUDIT05 == "A055"] <- 4
dLS1$AUDIT05[dLS1$AUDIT05 == ""] <- NA

dLS1$AUDIT05 <- as.numeric(dLS1$AUDIT05)

# AUDIT06
dLS1$AUDIT06[dLS1$AUDIT06 == "A061"] <- 0
dLS1$AUDIT06[dLS1$AUDIT06 == "A062"] <- 1
dLS1$AUDIT06[dLS1$AUDIT06 == "A063"] <- 2
dLS1$AUDIT06[dLS1$AUDIT06 == "A064"] <- 3
dLS1$AUDIT06[dLS1$AUDIT06 == "A065"] <- 4
dLS1$AUDIT06[dLS1$AUDIT06 == ""] <- NA

dLS1$AUDIT06 <- as.numeric(dLS1$AUDIT06)

# AUDIT07
dLS1$AUDIT07[dLS1$AUDIT07 == "A071"] <- 0
dLS1$AUDIT07[dLS1$AUDIT07 == "A072"] <- 1
dLS1$AUDIT07[dLS1$AUDIT07 == "A073"] <- 2
dLS1$AUDIT07[dLS1$AUDIT07 == "A074"] <- 3
dLS1$AUDIT07[dLS1$AUDIT07 == "A075"] <- 4
dLS1$AUDIT07[dLS1$AUDIT07 == ""] <- NA

dLS1$AUDIT07 <- as.numeric(dLS1$AUDIT07)

# AUDIT08
dLS1$AUDIT08[dLS1$AUDIT08 == "A081"] <- 0
dLS1$AUDIT08[dLS1$AUDIT08 == "A082"] <- 1
dLS1$AUDIT08[dLS1$AUDIT08 == "A083"] <- 2
dLS1$AUDIT08[dLS1$AUDIT08 == "A084"] <- 3
dLS1$AUDIT08[dLS1$AUDIT08 == "A085"] <- 4
dLS1$AUDIT08[dLS1$AUDIT08 == ""] <- NA

dLS1$AUDIT08 <- as.numeric(dLS1$AUDIT08)

# AUDIT09
dLS1$AUDIT09[dLS1$AUDIT09 == "A091"] <- 0
dLS1$AUDIT09[dLS1$AUDIT09 == "A092"] <- 2
dLS1$AUDIT09[dLS1$AUDIT09 == "A093"] <- 4
dLS1$AUDIT09[dLS1$AUDIT09 == ""] <- NA

dLS1$AUDIT09 <- as.numeric(dLS1$AUDIT09)

# AUDIT10
dLS1$AUDIT10[dLS1$AUDIT10 == "A101"] <- 0
dLS1$AUDIT10[dLS1$AUDIT10 == "A102"] <- 2
dLS1$AUDIT10[dLS1$AUDIT10 == "A103"] <- 4
dLS1$AUDIT10[dLS1$AUDIT10 == ""] <- NA

dLS1$AUDIT10 <- as.numeric(dLS1$AUDIT10)

dLS1 <- mutate(dLS1, AUDIT = AUDIT01 + AUDIT02 + AUDIT03 + AUDIT04 + AUDIT05 + AUDIT06 + AUDIT07 + AUDIT08 + AUDIT09 + AUDIT10)

dLS1 <- AddDummyCol(dLS1, "Alc", "HC")
dLS1$Alc[dLS1$AUDIT >= 10] <- "Alc"

##### Binge Drinking
dLS1 <- AddDummyCol(dLS1, "BD")

##### Last drink
dLS1$LastDrink[dLS1$LastDrink == "A1"] <- "MoreThan1Month"
dLS1$LastDrink[dLS1$LastDrink == "A2"] <- "LessThan1Month"
dLS1$LastDrink[dLS1$LastDrink == "A3"] <- "ThisWeek"
dLS1$LastDrink[dLS1$LastDrink == "A4"] <- "Yesterday"
dLS1$LastDrink[dLS1$LastDrink == "A5"] <- "Today"

dLS1$LastDrink <- factor(dLS1$LastDrink,
                         levels = c("MoreThan1Month", "LessThan1Month", "ThisWeek", "Yesterday", "Today"), ordered = T)


##### Order
dLS1 <- AddDummyCol(dLS1, "FirstVid", "Alcohol")
dLS1$FirstVid[dLS1$Age%%2 == 1] <- "Neutral"

dLS1$FirstVid <- factor(dLS1$FirstVid, ordered = F)

########## Final Frames
dF <- select(dLS1, NS, Mail1, FirstVid, Age, Gender, StudyLvl, Work, AUDIT, Alc, BD, LastDrink)
dRecr <- select(dLS1, Mail1, AUDIT, BD, Age, Gender, Contactable, DrugUse)
dMailLS1 <- select(dLS1, Mail1)

########## Export
write.table(dF, paste0(Output_path, "dLS1.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dRecr, "dRecrutement.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dMailLS1, "AdditionalInfo/MailList/MailLS1.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
