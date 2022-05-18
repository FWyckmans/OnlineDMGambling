#################### Craving
##### Initialization
source("DMG_Init.R")

##### Data
d <- read.delim("Output/dFINAL.txt")

##### VoI
VoI <- c("Ind_CravingG", "Ind_CravingN", "dIndCraving")
dCrav <- select(d, NS, Grp, Grp3, all_of(VoI))

HCd <- c()
Gd <- c()
PGd <- c()
HCn <- c()
Gn <- c()
PGn <- c()
compt = 1

for (i in VoI){
  if (typeof(dCrav[[i]]) == "integer"){
    mHC = round(mean(dCrav[dCrav$Grp3 == "HC",i], na.rm = T), 2)
    sdHC = round(sd(dCrav[dCrav$Grp3 == "HC",i], na.rm = T), 2)
    mdHC = round(median(dCrav[dCrav$Grp3 == "HC",i], na.rm = T), 2)
    
    HCi = paste0(mHC, " (", sdHC, ") | ", mdHC)
    HCni = NormCheck(dCrav[dCrav$Grp3 == "HC",i])
    
    mG = round(mean(dCrav[dCrav$Grp3 == "G",i], na.rm = T), 2)
    sdG = round(sd(dCrav[dCrav$Grp3 == "G",i], na.rm = T), 2)
    mdG = round(median(dCrav[dCrav$Grp3 == "G",i], na.rm = T), 2)
    
    Gi = paste0(mG, " (", sdG, ") | ", mdG)
    Gni = NormCheck(dCrav[dCrav$Grp3 == "G",i])
    
    mPG = round(mean(dCrav[dCrav$Grp3 == "PG",i], na.rm = T), 2)
    sdPG = round(sd(dCrav[dCrav$Grp3 == "PG",i], na.rm = T), 2)
    mdPG = round(median(dCrav[dCrav$Grp3 == "PG",i], na.rm = T), 2)
    
    PGi = paste0(mPG, " (", sdPG, ") | ", mdPG)
    PGni = NormCheck(dCrav[dCrav$Grp3 == "PG",i])
  }
  
  if (typeof(dCrav[[i]]) == "character"){
    namelvl = unique(dCrav[[i]])
    
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

nameHC <- paste0("HC (", sum(dCrav$Grp3 == "HC"), ")")
nameG <- paste0("G (", sum(dCrav$Grp3 == "G"), ")")
namePG <- paste0("PG (", sum(dCrav$Grp3 == "PG"), ")")

dCravDescr <- data.frame(VoI, HCd, Gd, PGd, HCn, Gn, PGn)

names(dCravDescr)[names(dCravDescr) == 'HCd'] <- nameHC
names(dCravDescr)[names(dCravDescr) == 'PGd'] <- namePG
names(dCravDescr)[names(dCravDescr) == 'Gd'] <- nameG
names(dCravDescr)[names(dCravDescr) == 'VoI'] <- "Variables"

write_xlsx(dCravDescr, "output/DescriptiveCrav.xlsx")
