##### Anonymize PS
##### Total Merge
# Initialization
source("DMG_Init.R")

Datapath = "Data/Pavlovia/Processed/"
Output_path = "Output/Processed/"

##### Data Frame
# Import
dComp1 <- read.delim(paste0(Datapath, "Comp1.txt"))
dComp2 <- read.delim(paste0(Datapath, "Comp2.txt"))
dTot <- read.delim("Output/Anonyme/dTotAnonyme.txt")
dTotA <- read.delim("Output/Processed/dTot.txt")

# Remove Mail not in dTot
dComp1 <- filter(dComp1, Mail %in% dTot$Mail1)
length(unique(dComp1$Mail))

dComp2 <- filter(dComp2, Mail %in% dTot$Mail1)
length(unique(dComp2$Mail))


##### Anonymise dComp
dComp1A <- AddDummyCol(dComp1, "subjID")
dComp2A <- AddDummyCol(dComp2, "subjID")

for (i in dTot$Mail) {
  dComp1A$subjID[dComp1A$Mail==i] <- dTot$NS[dTot$Mail1==i]
  dComp2A$subjID[dComp2A$Mail==i] <- dTot$NS[dTot$Mail1==i]
}

length(unique(dComp1A$subjID))
length(unique(dComp2A$subjID))

dComp1A <- select(dComp1A,"ExpName":"subjID")
dComp2A <- select(dComp2A,"ExpName":"subjID")

write.table(dComp1A, "Output/Processed/Comp1A.txt", col.names = T, row.names = F, sep = "\t", dec = ".")
write.table(dComp2A, "Output/Processed/Comp2A.txt", col.names = T, row.names = F, sep = "\t", dec = ".")