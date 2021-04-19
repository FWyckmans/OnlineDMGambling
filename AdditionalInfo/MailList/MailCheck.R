# Mail check
# Initialization
source("DMA_Init.R")

Datapath = "AdditionalInfo/MailList/"
Output_path = "AdditionalInfo/MailList/"

# Function
RemoveUseless <- function(d, ToRemove){
  d <- d%>%
    filter(!(d[,1] %in% ToRemove))
  return(d)
}

# Import Data
MailD1 <- read.csv(paste0(Datapath, "MailD1.txt"), sep="")
MailD2 <- read.csv(paste0(Datapath, "MailD2.txt"), sep="")

MailLS1 <- read.csv(paste0(Datapath, "MailLS1.txt"), sep="")
MailLS2 <- read.csv(paste0(Datapath, "MailLS2.txt"), sep="")
MailLS3 <- read.csv(paste0(Datapath, "MailLS3.txt"), sep="")

# Remove tests
ToRemove <- c("Test", "test", "TEST", "TEST ", "noemie.bonjean@ulb.be")

MailD1 <- RemoveUseless(MailD1, ToRemove)
MailD2 <- RemoveUseless(MailD2, ToRemove)
MailLS1 <- RemoveUseless(MailLS1, ToRemove)
MailLS2 <- RemoveUseless(MailLS2, ToRemove)
MailLS3 <- RemoveUseless(MailLS3, ToRemove)

# Remove those in each list
MailLS1 <- AddDummyCol(MailLS1, c("D1", "LS2", "D2", "LS3"), 0)

for (i in c(1:length(MailLS1$Mail1))) {
  if (MailLS1$Mail1[i] %in% MailLS2$Mail2){
    MailLS1$LS2[i] = 1
  }
  if (MailLS1$Mail1[i] %in% MailLS3$Mail3){
    MailLS1$LS3[i] = 1
  }
  if (MailLS1$Mail1[i] %in% MailD1$Mail){
    MailLS1$D1[i] = 1
  }
  if (MailLS1$Mail1[i] %in% MailD2$Mail){
    MailLS1$D2[i] = 1
  }
}
MailLS1 <- mutate(MailLS1, nOK = LS2 + LS3 + D1 + D2)
