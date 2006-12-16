startRemoteServer <- function(host = getOption("remoteHost"),
                              user. = user(),
                              timeout = 3600){
  clientHost <- hostName()
  clientPort <- availablePort()
  ## This depends on the server R process invoking 'runCommandLine()' on startup
  Rcmd <- paste("R --no-save --args",
                dQuote(paste("serveHostAndPort(",
                             sQuote(clientHost), ", ",
                             clientPort, ", ",
                             timeout, ")", sep = "")))
  ## escape parentheses, single quotes and double quotes
  Rcmd <- gsub("(", "\\(", Rcmd, fixed = T)
  Rcmd <- gsub(")", "\\)", Rcmd, fixed = T)
  Rcmd <- gsub('"', '\\"', Rcmd, fixed = T)
  Rcmd <- gsub("'", "\\'", Rcmd, fixed = T)
  
  ssh(Rcmd, host = host, user. = user., intern = F)  
  assign(".serverSession", receiveSocketObject(clientPort), env = globalenv())
}
