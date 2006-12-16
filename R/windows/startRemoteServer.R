startRemoteServer <- function(host = getOption("remoteHost"),
                              user. = user(),
                              timeout = 3600){
  clientHost <- hostName()
  clientPort <- availablePort()
  ## This depends on the server R process invoking 'runCommandLine()' on startup
  Rcmd <- paste("R --no-save --args 'serveHostAndPort(\\\"",
                clientHost, "\\\", ",
                clientPort, ", ",
                timeout, ")'",
                sep = "")
  ssh(Rcmd, host = host, user. = user., intern = F, wait = F)
  assign(".serverSession", receiveSocketObject(clientPort), env = globalenv())
}
