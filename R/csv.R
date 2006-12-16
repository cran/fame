csv <- function(z, file = "", noDates = F, row.names = !is.tis(z), ...){
  if(!inherits(file, "connection")){
    if(file == "")
      file <- paste(deparse(substitute(z)), ".csv", sep = "")
    file <- gsub(".csv.csv$", ".csv", paste(file, ".csv", sep = ""))
  }
  
  z <- as.matrix(z)
  if(is.tis(z) && !noDates)
    z <- cbind(Date = ssDate(ti(z)), z)

  write.table(z, file, sep = ",", row.names = row.names, qmethod = "double", ...)
}

