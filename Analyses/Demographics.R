#################### Demographics
##### Initialization
source("DMG_Init.R")

##### Data
d <- read.delim("Output/dFINAL.txt")

# Demographic variables
d <- AddDummyCol(d, "Smoker", "Yes")
d$Smoker[is.na(d$Fager)] <- "No"

DemVar = c("Age", "Gender", "StudyLvl",
           "ICJE", "AUDIT", "CAST", "Fager", "Smoker",
           "Auto", "Routine",
           "UPPS", "NegUr", "PosUr", "LoPr", "LoPe", "SS",
           "PosAff", "NegAff", "BDI",
           "CERQAdaptative", "CERQNonAdaptative")

dDem <- select(d, NS, Grp, Grp3, all_of(DemVar))
HCd <- c()
Gd <- c()
PGd <- c()
HCn <- c()
Gn <- c()
PGn <- c()

i = 'Age'
compt = 1

for (i in DemVar){
  if (typeof(dDem[[i]]) == "integer"){
    mHC = round(mean(dDem[dDem$Grp3 == "HC",i], na.rm = T), 2)
    sdHC = round(sd(dDem[dDem$Grp3 == "HC",i], na.rm = T), 2)
    mdHC = round(median(dDem[dDem$Grp3 == "HC",i], na.rm = T), 2)
    
    HCi = paste0(mHC, " (", sdHC, ") | ", mdHC)
    HCni = NormCheck(dDem[dDem$Grp3 == "HC",i])
    
    mG = round(mean(dDem[dDem$Grp3 == "G",i], na.rm = T), 2)
    sdG = round(sd(dDem[dDem$Grp3 == "G",i], na.rm = T), 2)
    mdG = round(median(dDem[dDem$Grp3 == "G",i], na.rm = T), 2)
    
    Gi = paste0(mG, " (", sdG, ") | ", mdG)
    Gni = NormCheck(dDem[dDem$Grp3 == "G",i])
    
    mPG = round(mean(dDem[dDem$Grp3 == "PG",i], na.rm = T), 2)
    sdPG = round(sd(dDem[dDem$Grp3 == "PG",i], na.rm = T), 2)
    mdPG = round(median(dDem[dDem$Grp3 == "PG",i], na.rm = T), 2)
    
    PGi = paste0(mPG, " (", sdPG, ") | ", mdPG)
    PGni = NormCheck(dDem[dDem$Grp3 == "PG",i])
  }
  
  if (typeof(dDem[[i]]) == "character"){
    namelvl = unique(dDem[[i]])
    
    HCi <- CharDescr(namelvl, "HC", i)
    Gi <- CharDescr(namelvl, "G", i)
    PGi <- CharDescr(namelvl, "PG", i)
    
    HCni <- NA
    Gni <- NA
    PGni <- NA
    }
    
  
  HCd[compt] <- HCi
  Gd[compt] <- Gi
  PGd[compt] <- PGi
  HCn[compt] <- HCni
  Gn[compt] <- Gni
  PGn[compt] <- PGni
  
  compt = compt + 1
}

nameHC <- paste0("HC (", sum(dDem$Grp3 == "HC"), ")")
nameG <- paste0("G (", sum(dDem$Grp3 == "G"), ")")
namePG <- paste0("PG (", sum(dDem$Grp3 == "PG"), ")")

dDescr <- data.frame(DemVar, HCd, Gd, PGd, HCn, Gn, PGn)

names(dDescr)[names(dDescr) == 'HCd'] <- nameHC
names(dDescr)[names(dDescr) == 'PGd'] <- namePG
names(dDescr)[names(dDescr) == 'Gd'] <- nameG
names(dDescr)[names(dDescr) == 'DemVar'] <- "Variables"

write_xlsx(dDescr, "output/DescriptiveClinical.xlsx")