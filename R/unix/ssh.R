ssh <- function(command, host = getOption("remoteHost"),
                user. = user(), wait = F, ...){
  if(exists("localSsh", mode = "function")){
    Call <- match.call()
    Call[[1]] <- as.name("localSsh")
    eval(Call, parent.frame())
  }
  else {
    if(is.null(host)){
      host <- askForString(prompt = "DNS name of remote host:")
      options(remoteHost = host)
    }
    cmd <- paste(if(wait) "ssh -l" else "ssh -fl",
                 user., host, command)
    system(cmd, ...)
  }
}

