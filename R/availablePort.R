portsInUse <- function(){
  ans <- numeric()
  if(runningLinux()){
    hexTcp <- system("cat /proc/net/tcp | awk '{print $2}'", intern = T)
    hexUdp <- system("cat /proc/net/udp | awk '{print $2}'", intern = T)
    ans <- unique(sort(hex2numeric(gsub(".*:", "", c(hexTcp[-1], hexUdp[-1])))))
  }
  else {
    if(runningWindows()){
      strings <-grep(":[0-9]",
                     system("netstat -an", intern = T),
                     value = T)
      if(length(strings)> 0){
        bounds <- regexpr(":[0-9]*", strings)
        starts <- as.vector(bounds) + 1
        ends <- starts + attr(bounds, "match.length") - 2
        ans <- unique(sort(as.numeric(substr(strings, starts, ends))))
      }
    }
    else stop("unsupported operating system")
  }
  ans
}

availablePort <- function(){
  takenPorts <- portsInUse()
  port <- 40001
  while(!is.na(match(port, takenPorts))) port <- port + 1
  port
}
