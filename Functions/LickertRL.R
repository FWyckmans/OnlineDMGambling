LickRL <- function(d, Time){
  dLick <- d%>%
    select(Craving = CravingKR.keys, Resist = ResistKR.keys, Stress = StressKR.keys)%>%
    filter(!is.na(Craving))
  
  if (length(dLick$Craving) == 2){
    Add <- c(NA, NA, NA)
    dLick <- rbind(dLick, Add)
  }
  
  dLick <- AddDummyCol(dLick, "Time", 1)
  dLick$Time <- c("BeforeV", "AfterV", "AfterTask")
  dLick <- dLick%>%
    unite("Craving_Resist_Stress", Craving:Stress, sep = "_")%>%
    spread(key = Time, value = Craving_Resist_Stress)%>%
    separate(BeforeV, into = c("Craving1", "Resist1", "Stress1"))%>%
    separate(AfterV, into = c("Craving2", "Resist2", "Stress2"))%>%
    separate(AfterTask, into = c("Craving3", "Resist3", "Stress3"))
  if (Time == 1){
    dLick <- dLick%>%
      select(Craving1, Craving2, Craving3,
             Resist1, Resist2, Resist3,
             Stress1, Stress2, Stress3)}
  
  if (Time == 2){
    dLick <- dLick%>%
      select(Craving4 = Craving1, Craving5 = Craving2, Craving6 = Craving3,
             Resist4 = Resist1, Resist5 = Resist2, Resist6 = Resist3,
             Stress4 = Stress1, Stress5 = Stress2, Stress6 = Stress3)}
  
  return(dLick)}