library(XML)

getdat = function(xClass,year,pitching, league){

  if(pitching == T){
    fileurl = paste("http://www.baseball-reference.com/leagues/",league, "/",year,"-standard-pitching.shtml",sep = "")
    cols = c(1,3,5:35)
    dfName = c("ID", "pName", "Age", "tName", "W", "L", "WL%", "ERA", "G", "GS", "GF", "CG", "SHO", "SV", "IP", "H", "R", "ER", "HR", "BB", "IBB", "SO", "HBP", "BK", "WP", "BF", "ERA+", "FIP", "WHIP", "H9", "HR9", "BB9", "SO9", "K/BB") 
    ec = 35
    classes = c(rep(c("Numeric", "Character"), 2), rep("Numeric",30))
  }
  
  else{
    fileurl = paste("http://www.baseball-reference.com/leagues/", league, "/",year,"-standard-batting.shtml",sep = "")
    cols = c(1,3,5:28)
    dfName = c("ID", "pName", "Age", "tName", "G", "PA", "AB", "R", "H", "2B", "3B", "HR", "RBI", "SB", "CS", "BB", "SO", "BA", "OBP", "SLG", "OPS", "OPS+", "TB", "GDP", "HBP", "SH", "SF", "IBB", "Pos")
    ec = 29
    classes = c(rep(c("Numeric", "Character"), 2), rep("Numeric",25))
  }
  
  doc = htmlTreeParse(fileurl,useInternal = TRUE)
  string = paste("//tr[@class='",xClass,"']",sep = "") 
  dat = xpathSApply(doc, string, xmlValue)
  
  almost = matrix(nrow = length(dat), ncol = ec)
  
  almost = sapply(1:length(dat), function(i){
    a = dat[i]
    a1 = gsub("\n","",a)
    a2 = strsplit(a1,"   ")
    if(length(a2[[1]]) == (ec-1)){
      a2[[1]][ec] = NA
    }
    
    a2[[1]][which(a2[[1]] == "")] = NA
    a2[[1]][2] = gsub("Â", "",a2[[1]][2])
    
    almost[i,] <<- unlist(a2)
    invisible(a2)
    
  })
  
  maybe = matrix(unlist(almost), nrow = length(almost),byrow = T)
  what = as.data.frame(maybe)
  
  what[,1:ncol(what)] = sapply(what[,1:ncol(what)], as.character)
  what[,cols] = sapply(what[,cols], as.numeric)  

  
  names(what) = dfName

  return(what)
}

