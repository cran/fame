hostName <- function(){
  nodeName <- tolower(Sys.info()["nodename"])
  if(runningWindows()){
    string <- grep("DNS Suffix", system("ipconfig", intern = T), value = T)
    dnsSuffix <- gsub("\r", "", gsub(".*:", "", gsub(" ", "", string)))
    paste(nodeName, dnsSuffix, sep = ".")
  }
  else nodeName
}
