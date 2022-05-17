# Initialization
source("DMG_Init.R")
Datapath = "Data/LimeSurveyQuestionnaires/Raw/"
Output_path = "Data/LimeSurveyQuestionnaires/Processed/"

########## Prep dataframe
# Import LimeSurvey data
dLS1 <- read.csv(paste0(Datapath, "ResultLS1.csv"), encoding="UTF-8")

# Rename and select columns
colnames(dLS1)[1] <- "NS"
TestMail <- c("test", "test ", "Test", "TEST", "TEST ", "TEST 2", "TEST3", "test 3", "testbis", "de",
              "https://survey.ulb.ac.be/survey3/index.php/388295")

dLS1 <- dLS1 %>%
  filter(lastpage == 4)%>% # Remove unfinished
  filter(E2 == "A1")%>% # Remove not consenting
  filter(!E1 %in% TestMail)

# Create df for type of game
dType <- dLS1%>%
  select(NS, Loterie = A3.A1., Casino = A3.A2., Bingo = A3.A3., Carte = A3.A5., Courses = A3.A6., Bourse = A3.A7.,
         Machine = A3.A8., Adresse = A3.A10., Des = A3.A9., Sport = A3.A4., Poker = A3.A12., Other = A3.A11.)
dType <- AddDummyCol(dType, "MainGame")

for (i in c(2:(length(dType)-1))) {
  dType[i][dType[i]=="A1"] <- 0
  dType[i][dType[i]=="A2"] <- 1
  dType[i][dType[i]=="A3"] <- 2
  dType[i][dType[i]=="A4"] <- 3
  dType[i][dType[i]=="A5"] <- 4
  dType[[i]] <- as.numeric(dType[[i]])
}

for (i in c(1:length(dType$MainGame))) {
  if (!is.na(dType$Loterie[i])){
    maingamesCol = as.numeric(which.max(dType[i,c(2:(length(dType)-2))]))
    maingames = colnames(dType[maingamesCol+1])
    dType$MainGame[i] = maingames
    }
}

dLS1 <- cbind(dLS1, dType["MainGame"])

# Main df
dLS1 <- select(dLS1, NS, Mail1 = E1, Age = IP01, Gender = IP02, StudyLvl = IP03, Work = IP06, Contactable = IP04, DrugUse = IP05,
               ICJE1 = ICJE.ICJE01., ICJE2 = ICJE.ICJE02., ICJE3 = ICJE.ICJE03., ICJE4 = ICJE.ICJE04.,
               ICJE5 = ICJE.ICJE05., ICJE6 = ICJE.ICJE06., ICJE7 = ICJE.ICJE07., ICJE8 = ICJE.ICJE08., ICJE9 = ICJE.ICJE09.,
               LastSession = A6, MainGame)#, Online = A5)

# Remove duplicates
dLS1 <- filter(dLS1, !(Mail1 == "tomhologne@gmail.com" & NS == 440))
dLS1 = dLS1[order(dLS1[,'Mail1'],-dLS1[,'NS']),]
dLS1 = dLS1[!duplicated(dLS1$Mail1),]
dLS1 = dLS1[order(dLS1[,'NS']),]

########## Columns handling
##### Demo
# Age
dLS1$Age <- str_remove(dLS1$Age, " ans")
dLS1$Age <- str_remove(dLS1$Age, "ans")

dLS1$Age <- as.numeric(dLS1$Age)

# Change badly encoded age
dLS1$Age[dLS1$Age == 1997] <- 24

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
# ICJE1
dLS1$ICJE1[dLS1$ICJE1 == "A1"] <- 0
dLS1$ICJE1[dLS1$ICJE1 == "A2"] <- 1
dLS1$ICJE1[dLS1$ICJE1 == "A3"] <- 2
dLS1$ICJE1[dLS1$ICJE1 == "A4"] <- 3
dLS1$ICJE1[dLS1$ICJE1 == ""] <- NA

dLS1$ICJE1 <- as.numeric(dLS1$ICJE1)

# ICJE2
dLS1$ICJE2[dLS1$ICJE2 == "A1"] <- 0
dLS1$ICJE2[dLS1$ICJE2 == "A2"] <- 1
dLS1$ICJE2[dLS1$ICJE2 == "A3"] <- 2
dLS1$ICJE2[dLS1$ICJE2 == "A4"] <- 3
dLS1$ICJE2[dLS1$ICJE2 == ""] <- NA

dLS1$ICJE2 <- as.numeric(dLS1$ICJE2)

# ICJE3
dLS1$ICJE3[dLS1$ICJE3 == "A1"] <- 0
dLS1$ICJE3[dLS1$ICJE3 == "A2"] <- 1
dLS1$ICJE3[dLS1$ICJE3 == "A3"] <- 2
dLS1$ICJE3[dLS1$ICJE3 == "A4"] <- 3
dLS1$ICJE3[dLS1$ICJE3 == ""] <- NA

dLS1$ICJE3 <- as.numeric(dLS1$ICJE3)

# ICJE4
dLS1$ICJE4[dLS1$ICJE4 == "A1"] <- 0
dLS1$ICJE4[dLS1$ICJE4 == "A2"] <- 1
dLS1$ICJE4[dLS1$ICJE4 == "A3"] <- 2
dLS1$ICJE4[dLS1$ICJE4 == "A4"] <- 3
dLS1$ICJE4[dLS1$ICJE4 == ""] <- NA

dLS1$ICJE4 <- as.numeric(dLS1$ICJE4)

# ICJE5
dLS1$ICJE5[dLS1$ICJE5 == "A1"] <- 0
dLS1$ICJE5[dLS1$ICJE5 == "A2"] <- 1
dLS1$ICJE5[dLS1$ICJE5 == "A3"] <- 2
dLS1$ICJE5[dLS1$ICJE5 == "A4"] <- 3
dLS1$ICJE5[dLS1$ICJE5 == ""] <- NA

dLS1$ICJE5 <- as.numeric(dLS1$ICJE5)

# ICJE6
dLS1$ICJE6[dLS1$ICJE6 == "A1"] <- 0
dLS1$ICJE6[dLS1$ICJE6 == "A2"] <- 1
dLS1$ICJE6[dLS1$ICJE6 == "A3"] <- 2
dLS1$ICJE6[dLS1$ICJE6 == "A4"] <- 3
dLS1$ICJE6[dLS1$ICJE6 == ""] <- NA

dLS1$ICJE6 <- as.numeric(dLS1$ICJE6)

# ICJE7
dLS1$ICJE7[dLS1$ICJE7 == "A1"] <- 0
dLS1$ICJE7[dLS1$ICJE7 == "A2"] <- 1
dLS1$ICJE7[dLS1$ICJE7 == "A3"] <- 2
dLS1$ICJE7[dLS1$ICJE7 == "A4"] <- 3
dLS1$ICJE7[dLS1$ICJE7 == ""] <- NA

dLS1$ICJE7 <- as.numeric(dLS1$ICJE7)

# ICJE8
dLS1$ICJE8[dLS1$ICJE8 == "A1"] <- 0
dLS1$ICJE8[dLS1$ICJE8 == "A2"] <- 1
dLS1$ICJE8[dLS1$ICJE8 == "A3"] <- 2
dLS1$ICJE8[dLS1$ICJE8 == "A4"] <- 3
dLS1$ICJE8[dLS1$ICJE8 == ""] <- NA

dLS1$ICJE8 <- as.numeric(dLS1$ICJE8)

# ICJE9
dLS1$ICJE9[dLS1$ICJE9 == "A1"] <- 0
dLS1$ICJE9[dLS1$ICJE9 == "A2"] <- 1
dLS1$ICJE9[dLS1$ICJE9 == "A3"] <- 2
dLS1$ICJE9[dLS1$ICJE9 == "A4"] <- 3
dLS1$ICJE9[dLS1$ICJE9 == ""] <- NA

dLS1$ICJE9 <- as.numeric(dLS1$ICJE9)

dLS1 <- mutate(dLS1, ICJE = ICJE1 + ICJE2 + ICJE3 + ICJE4 + ICJE5 + ICJE6 + ICJE7 + ICJE8 + ICJE9)

dLS1 <- AddDummyCol(dLS1, "Grp", "HC")
dLS1$Grp[dLS1$ICJE >= 4] <- "PG"

##### Last drink
dLS1$LastSession[dLS1$LastSession == "A1"] <- NA
dLS1$LastSession[dLS1$LastSession == "A2"] <- "MoreThan1Month"
dLS1$LastSession[dLS1$LastSession == "A3"] <- "LessThan1Month"
dLS1$LastSession[dLS1$LastSession == "A4"] <- "ThisWeek"
dLS1$LastSession[dLS1$LastSession == "A5"] <- "Yesterday"
dLS1$LastSession[dLS1$LastSession == "A6"] <- "Today"

dLS1$LastSession[is.na(dLS1$ICJE)] <- NA
dLS1$LastSession <- factor(dLS1$LastSession,
                         levels = c("MoreThan1Month", "LessThan1Month", "ThisWeek", "Yesterday", "Today"), ordered = T)


##### Order
dLS1 <- AddDummyCol(dLS1, "FirstVid", "Gambling")
dLS1$FirstVid[dLS1$Age%%2 == 1] <- "Neutral"

dLS1$FirstVid <- factor(dLS1$FirstVid, ordered = F)

########## Final Frames
dF <- select(dLS1, NS, Mail1, FirstVid, Age, Gender, StudyLvl, Work, ICJE, Grp, MainGame, LastSession)

# Recruitement
dRecr <- dLS1%>%
  select(Mail1, Grp, ICJE, MainGame, Age, Gender, Contactable, DrugUse)%>%
  filter(Contactable == "Yes")

dRecrDaw <- dRecr%>%
  filter(Grp == "PG", Gender == "Male")

dMailLS1 <- select(dLS1, Mail1)
dAge <- dLS1[, c(1, 2, 3)]

########## Export
write.table(dF, paste0(Output_path, "dLS1.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write_xlsx(dRecr, "dRecrutementGambling.xlsx")
write_xlsx(dRecrDaw, "dRecrutementGamblingDaw.xlsx")
write.table(dMailLS1, "AdditionalInfo/MailList/MailLS1.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dAge, "AdditionalInfo/MailList/AgeLS1.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
