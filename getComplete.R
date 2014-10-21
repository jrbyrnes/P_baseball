source("getdat.R")

getComplete = function(league, pitching, year){
  
  if(pitching == T){
    dfName = c("ID", "pName", "Age", "tName", "W", "L", "WL%", "ERA", "G", "GS", "GF", "CG", "SHO", "SV", "IP", "H", "R", "ER", "HR", "BB", "IBB", "SO", "HBP", "BK", "WP", "BF", "ERA+", "FIP", "WHIP", "H9", "HR9", "BB9", "SO9", "K/BB")     
    classes = c(rep(c("Numeric", "Character"), 2), rep("Numeric",30))
  }
  else{
    dfName = c("ID", "pName", "Age", "tName", "G", "PA", "AB", "R", "H", "2B", "3B", "HR", "RBI", "SB", "CS", "BB", "SO", "BA", "OBP", "SLG", "OPS", "OPS+", "TB", "GDP", "HBP", "SH", "SF", "IBB", "Pos")
    classes = c(rep(c("Numeric", "Character"), 2), rep("Numeric",25))
  }
  
  
  first = getdat(xClass = "full_table",year,pitching, league)
  second = getdat(xClass = "full_table non_qual",year,pitching,league)
  

  dat = as.data.frame(matrix(nrow = (nrow(first) + nrow(second)), ncol = ncol(first)), stringsAsFactors = F)
  dat[1:nrow(first),] = first
  dat[(nrow(first)+1):nrow(dat),] = second
  
  out = dat[order(dat[,1]),]
  out[,1] = 1:length(out[,1])
  names(out) = dfName
  out$pName = gsub("Â", "", out$pName)
  return(out)
}