# Initialization
source("DMA_Init.R")
Datapath = "Data/LimeSurveyQuestionnaires/Raw/"
Output_path = "Data/LimeSurveyQuestionnaires/Processed/"

########## Prep dataframe
# Import LimeSurvey data
dLS2 <- read.csv(paste0(Datapath, "ResultLS2.csv"), encoding="UTF-8")

# Rename and select columns
colnames(dLS2)[1] <- "NS"
dLS2 <- dLS2 %>%
  filter(lastpage == 1)%>% # Remove unfinished
  filter(E1 != "test")%>%
  filter(E1 != "Test")%>%
  filter(E1 != "TEST")%>%
  filter(E1 != "noemie.bonjean@ulb.be")

# Main df
dLS2 <- dLS2%>%
  select(NS, Mail2 = E1, LastDrink2 = Al1, LastDrinkQuantity = Al2)

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
dLS2$LastDrink2[dLS2$LastDrink2 == "Al11"] <- "Today"
dLS2$LastDrink2[dLS2$LastDrink2 == "Al12"] <- "AfterS1"
dLS2$LastDrink2[dLS2$LastDrink2 == "Al13"] <- "BeforeS1"
dLS2$LastDrink2[dLS2$LastDrink2 == "Al14"] <- "Earlier"

dLS2$LastDrink2 <- factor(dLS2$LastDrink2, levels = c("Earlier", "BeforeS1", "AfterS2", "Today"), ordered = T)

########## Final Frames
dF <- select(dLS2, NS, Mail2, LastDrink2, LastDrinkQuantity)
dMailLS2 <- select(dLS2, Mail2)

########## Export
write.table(dF, paste0(Output_path, "dLS2.txt"), col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dMailLS2, "AdditionalInfo/MailList/MailLS2.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
