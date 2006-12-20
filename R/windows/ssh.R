ssh <- function(command, host = getOption("remoteHost"),
                user. = user(), password = "", wait = F, ...){
  if(exists("localSsh", mode = "function")){
    Call <- match.call()
    Call[[1]] <- as.name("localSsh")
    eval(Call, parent.frame())
  }
  else {
    ## this will fail immediately if plink isn't available
    plinkUsage <- system("plink", intern = T)
    canHide <- length(grep("hide_console", plinkUsage)) > 0
    
    if(is.null(host)){
      host <- askForString(prompt = "Remote host: ")
      options(remoteHost = host)
    }
    
    if(password == ""){
      passwordPrompt <- paste("Password on ", host, ": ", sep = "")
      password <- askForPassword(passwordPrompt)
    }
    
    sshCmd <- paste("plink -ssh -l", user.,
                    "-pw", password,
                    if(canHide) "-hide_console" else "",
                    host)
    cmd <- paste(sshCmd, command)
    system(cmd, wait = wait, ...)
  }
}

