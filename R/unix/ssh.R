ssh <- function(command, host = getOption("remoteHost"),
                user. = user(), wait = F, ...){
  if(is.null(host)){
    host <- askForString(prompt = "DNS name of remote host:")
    options(remoteHost = host)
  }
  cmd <- paste(if(wait) "ssh -l" else "ssh -fl",
                  user., host, command)
  system(cmd, ...)
}

