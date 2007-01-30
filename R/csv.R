csv <- function(z, file = "", noDates = F, row.names = !is.tis(z), ...){
  if(!inherits(file, "connection")) {
    if(file == "") 
      file <- paste(deparse(substitute(z)), ".csv", sep = "")
    file <- gsub(".csv.csv$", ".csv", paste(file, ".csv", sep = ""))
  }
  zz <- stripTis(as.matrix(z))
  colList <- columns(zz)
  if(!is.numeric(zz)){ ## Try turning columns into numbers
    for(i in 1:length(colList)){
      zi <- colList[[i]]
      naSpots <- is.na(zi)
      zn <- as.numeric(zi)
      if(!any(is.na(zn[!naSpots])))
        colList[[i]] <- zn
    }
  }
  if(is.tis(z) && !noDates)
    colList <- c(list(Date = unclass(ssDate(ti(z)))), colList)

  df <- do.call("data.frame", c(colList, list(stringsAsFactors = FALSE)))
  write.table(df, file, sep = ",", row.names = row.names,
              qmethod = "double", ...)
}
