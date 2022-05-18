CharDescr <- function(namelvl, Grp, col){
  output = c()
  for (j in namelvl){
    n <- sum(dDem[[col]] == j & dDem$Grp3 == Grp)
    
    if (j != namelvl[length(namelvl)]){
      output <- paste0(output, n, " ", j, " | ")
    } else {
      output <- paste0(output, n, " ", j)
    }
  }
  return(output)
}