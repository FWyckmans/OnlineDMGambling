# Initialization
source("DMG_Init.R")
Datapath = "Data/LimeSurveyQuestionnaires/Raw/"
Output_path = "Data/LimeSurveyQuestionnaires/Processed/"

########## Prep dataframe
# Import LimeSurvey data
dLS2 <- read.csv(paste0(Datapath, "ResultLS2.csv"), encoding="UTF-8")

# Rename and select columns
colnames(dLS2)[1] <- "NS"
TestMail <- c("test", "test ", "Test", "TEST", "TEST ", "TEST 2", "TEST3", "test 3", "testbis", "de",
              "https://survey.ulb.ac.be/survey3/index.php/388295")

dLS2 <- dLS2 %>%
  filter(lastpage == 1)%>% # Remove unfinished
  filter(!E1 %in% TestMail)

# Main df
dLS2 <- dLS2%>%
  select(NS, Mail2 = E1, LastSession2 = Ga1, LastSessionDuration = Ga2)

# Remove duplicates
dLS2 = dLS2[order(dLS2[,'Mail2'],-dLS2[,'NS']),]
dLS2 = dLS2[!duplicated(dLS2$Mail2),]
dLS2 = dLS2[order(dLS2[,'NS']),]

# Change badly spelled email if needed
for (i in c(1:length(dLS2$Mail2))) {
  if (dLS2$Mail2[i] %in% names(MailToChange)){
    dLS2$Mail2[i] <- MailToChange[[dLS2$Mail2[i]]]
  }
}

########## Columns handling
# Last Drink
dLS2$LastSession2[dLS2$LastSession2 == "Ga11"] <- "Today"
dLS2$LastSession2[dLS2$LastSession2 == "Ga12"] <- "AfterS1"
dLS2$LastSession2[dLS2$LastSession2 == "Ga13"] <- "BeforeS1"
dLS2$LastSession2[dLS2$LastSession2 == "Ga14"] <- "Earlier"

dLS2$LastSession2 <- factor(dLS2$LastSession2, levels = c("Earlier", "BeforeS1", "AfterS2", "Today"), ordered = T)

########## Final Frames
dF <- select(dLS2, NS, Mail2, LastSession2, LastSessionDuration)
dMailLS2 <- select(dLS2, Mail2)

########## Export
write.table(dF, paste0(Output_path, "dLS2.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dMailLS2, "AdditionalInfo/MailList/MailLS2.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
