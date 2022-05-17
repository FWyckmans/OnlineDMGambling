GatherParam <- function(d, Param, Crav, AddCol, CodeTypeVid = T){
  
  #' GatherParam
  #'
  #' @param d: original dataframe 
  #' @param Param : name of the computational parameter
  #' @param Crav : name of the craving measure
  #' @param AddCol : name of the additional columns
  #' @param CodeTypeVid : Code TypeVid or not
  #'
  #' @return Long dataframe
  #' 
  #' @example 
  #' ImpCol = c("NS", "ICJE", "Grp", "GrpC", "Grp3", "Grp3C", "TestQOK")
  #' dw <- GatherParam(d, "w", "dCraving", ImpCol)
  
  ParamG = paste0(Param, "_AfterGamb")
  ParamN = paste0(Param, "_AfterNeu")
  
  CravG = paste0(Crav, "G")
  CravN = paste0(Crav, "N")
  
  dL <- d[all_of(c(AddCol, ParamG, ParamN, CravG, CravN))]%>%
    unite(col = "Gambling", c(ParamG, CravG))%>%
    unite(col = "Neutral", c(ParamN, CravN))%>%
    gather(key = "TypeVid", value = "p_Crav", "Gambling":"Neutral")%>%
    separate("p_Crav", sep = "_", into = c(Param, Crav))
  
  if (CodeTypeVid == T){
    dL <- InsertCol(dL, rep(0.5, nrow(dL)), pos = "TypeVid", name = "TypeVidC")
    dL$TypeVidC[dL$TypeVid == "Gambling"] = 0.5}
  
  dL$w = as.numeric(dL$w)
  dL[[Crav]] = as.numeric(dL[[Crav]])
  
  return(dL)
  
}
