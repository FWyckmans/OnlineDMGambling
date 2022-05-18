################################################ Feature Engineering
# Initialization
source("DMG_Init.R")

Datapath = "Output/Processed/"
Output_path = "Output/"

############## DataFrame
##### Import
d <- read.delim(paste0(Datapath, "dTotAddComp.txt"))

##### DataFrame Cleaning
# Remove participants who bombed
ToRem = c(443, 458, 460, 459, 462, 463, 464, 465, 466)
d <- d%>%
  filter(!NS %in% ToRem)%>%
  filter(TestQOK == 1)

# Threshold for gambling
Threshold = 4
d$Grp[d$ICJE >= Threshold] <- 'PG'
d$Grp[d$ICJE < Threshold] <- 'HC'

d$GrpC <- -0.5
d$GrpC[d$Grp == 'HC'] <- 0.5

sum(d$Grp == 'PG')
sum(d$Grp == 'HC')

# Grp3
Threshold3 = 4
d$Grp3 <- 'G'
d$Grp3[d$ICJE >= Threshold3] <- 'PG'
d$Grp3[is.na(d$ICJE)] <- 'HC'

d$Grp3C <- -0.5
d$Grp3C[d$Grp3 == 'HC'] <- 0.5
d$Grp3C[d$Grp3 == 'G'] <- 0

sum(d$Grp3 == 'PG')
sum(d$Grp3 == 'G')
sum(d$Grp3 == 'HC')

#Gambler No Gambler
d$G_NGC <- -1
d$G_NGC[is.na(d$ICJE)] <- 1

##### Age Correction
for (i in c(1: length(d$NS))){
  if (d$Age[i] > 100){
    d$Age[i] = 2020 - d$Age[i]
  }
}

##### Add smoker column
d <- AddDummyCol(d, "Smoker", "Yes")
d$Smoker[is.na(d$Fager)] <- "No"

##### Feature engineering
# MB-MF
d$MBc_AfterGamb = d$w_AfterGamb*d$beta1_AfterGamb
d$MBc_AfterNeu = d$w_AfterNeu*d$beta1_AfterNeu

d$MFc_AfterGamb = (1-d$w_AfterGamb)*d$beta1_AfterGamb
d$MFc_AfterNeu = (1-d$w_AfterNeu)*d$beta1_AfterNeu

d$dw = d$w_AfterGamb - d$w_AfterNeu
d$dbeta1 = d$beta1_AfterGamb - d$beta1_AfterNeu
d$dbeta2 = d$beta2_AfterGamb - d$beta2_AfterNeu
d$da1 = d$a1_AfterGamb - d$a1_AfterNeu
d$da2 = d$a2_AfterGamb - d$a2_AfterNeu
d$dlambda = d$lambda_AfterGamb - d$lambda_AfterNeu
d$dpi = d$pi_AfterGamb - d$pi_AfterNeu
d$dMBc = d$MBc_AfterGamb - d$MBc_AfterNeu
d$dMFc = d$MFc_AfterGamb - d$MFc_AfterNeu

d$wM = (d$w_AfterGamb + d$w_AfterNeu)/2
d$beta1M = (d$beta1_AfterGamb + d$beta1_AfterNeu)/2
d$beta2M = (d$beta2_AfterGamb + d$beta2_AfterNeu)/2
d$a1M = (d$a1_AfterGamb + d$a1_AfterNeu)/2
d$a2M = (d$a2_AfterGamb + d$a2_AfterNeu)/2
d$lambdaM = (d$lambda_AfterGamb + d$lambda_AfterNeu)/2
d$piMM = (d$pi_AfterGamb + d$pi_AfterNeu)/2
d$MBcM = (d$MBc_AfterGamb + d$MBc_AfterNeu)/2
d$MFcM = (d$MFc_AfterGamb + d$MFc_AfterNeu)/2

# Craving
d$Ind_CravingG = d$CravingAfGamb - d$CravingBfGamb
d$dCraving2G = d$CravingAfGamb2 - d$CravingBfGamb
d$dCravingMG = (d$CravingAfGamb2 + d$CravingAfGamb)/2 - d$CravingBfGamb

d$Ind_CravingN = d$CravingAfNeu - d$CravingBfNeu
d$dCraving2N = d$CravingAfNeu2 - d$CravingBfNeu
d$dCravingMN = (d$CravingAfNeu2 + d$CravingAfNeu)/2 - d$CravingBfNeu

d$dIndCraving = d$Ind_CravingG - d$Ind_CravingN
d$dCraving2 = d$dCraving2G - d$dCraving2N
d$dCravingM = d$dCravingMG - d$dCravingMN

############## Write Tables
write.table(d, paste0(Output_path, 'dFINAL.txt'), col.names = T, row.names = F, sep = "\t", dec = ".")

mean(d$w_AfterGamb[d$Grp=='PG'])
mean(d$w_AfterNeu[d$Grp=='PG'])

mean(d$w_AfterGamb[d$Grp=='HC'])
mean(d$w_AfterNeu[d$Grp=='HC'])

t.test(d$w_AfterGamb[d$Grp=='PG'], d$w_AfterGamb[d$Grp=='HC']) 

t.test(d$w_AfterNeu[d$Grp=='PG'], d$w_AfterNeu[d$Grp=='HC'])

boxplot(d$w_AfterGamb[d$Grp=='PG'], d$w_AfterNeu[d$Grp=='PG'],
        d$w_AfterGamb[d$Grp=='HC'], d$w_AfterNeu[d$Grp=='HC'],
        names = c('PG_Gamb', 'PG_Neu', 'HC_Gamb', 'HC_Neu'))

boxplot(d$w_AfterGamb[d$Grp3=='PG'], d$w_AfterNeu[d$Grp3=='PG'],
        d$w_AfterGamb[d$Grp3=='G'], d$w_AfterNeu[d$Grp3=='G'],
        d$w_AfterGamb[d$Grp3=='HC'], d$w_AfterNeu[d$Grp3=='HC'],
        names = c('PG_Gamb', 'PG_Neu', 'G_Gamb', 'G_Neu', 'HC_Gamb', 'HC_Neu'))

dG <- filter(d, !is.na(ICJE))
mG = aov(w_AfterGamb~Grp3, data = dG)
summary(mG)
mN = aov(w_AfterNeu~Grp3, data = dG)
summary(mN)

boxplot(d$MB_AfterGamb[d$Grp=='PG'], d$MB_AfterNeu[d$Grp=='PG'],
        d$MB_AfterGamb[d$Grp=='HC'], d$MB_AfterNeu[d$Grp=='HC'],
        names = c('PG_Gamb', 'PG_Neu', 'HC_Gamb', 'HC_Neu'))

boxplot(d$MF_AfterGamb[d$Grp=='PG'], d$MF_AfterNeu[d$Grp=='PG'],
        d$MF_AfterGamb[d$Grp=='HC'], d$MF_AfterNeu[d$Grp=='HC'],
        names = c('PG_Gamb', 'PG_Neu', 'HC_Gamb', 'HC_Neu'))

boxplot(d$w_AfterGamb, d$beta1_AfterGamb,
        d$MBc_AfterGamb, d$MFc_AfterGamb,
        names = c('w_Gamb', 'beta1_Neu', 'MB_Gamb', 'MF_Neu'))

boxplot(d$w_AfterNeu, d$beta1_AfterNeu,
        d$MBc_AfterNeu, d$MFc_AfterNeu,
        names = c('w_Neu', 'beta1_Neu', 'MB_Neu', 'MF_Neu'))
