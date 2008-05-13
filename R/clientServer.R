sendSocketObject <- function(object, host, port){
  ## open a blocking client socket, send object on it, and close
  conn <- socketConnection(host, port, open = "wb", blocking = T)
  on.exit(close(conn))
  serialize(object, connection = conn)
}

receiveSocketObject <- function(port){
  ## listen on a server socket for something to arrive
  conn <- socketConnection(port = port, open = "rb", server = T, blocking = T)
  on.exit(close(conn))
  unserialize(conn)
}

## A client-server 'session' has a client, a server, and possibly a timeout.
## (The timeout is meaningless if the server R is on Windows.)
##
## The client uses ssh to start R on the server, then listens on a socket for
## the server to return a session object.  The client stores the session in
## the global environment and, if the server is capable of timing out, updates
## the session timestamp with each message sent or received.  Before each
## attempt at sending or receiving, the client checks the session to see if
## the timeout has been exceeded, and does not attempt the connection if it
## has.

serveHostAndPort <- function(clientHost, clientPort, timeout = 3600, quitAfter = T){
  ## This is the server end
  ## Note that the server session is local to this function on the server
  ## end, while it is effectively a global variable on the client end.
  if(!is.null(timeout)) alarmc(timeout)

  serverHost <- hostName()
  serverPort <- availablePort()
  session <- structure(list(clientHost = clientHost,
                            clientPort = clientPort,
                            serverHost = serverHost,
                            serverPort = serverPort,
                            serverPid  = Sys.getpid(),
                            timeout = timeout,
                            timestamp = Sys.time()),
                       class = "serverSession")
  
  ## Internal send and receive functions
  send <- function(object) sendSocketObject(object, clientHost, clientPort)  
  receive <- function() receiveSocketObject(serverPort)
  
  send(session)
  repeat {
    if(!is.null(timeout)) alarmc(timeout)
    
    inThing <- receive()
    result <- switch(mode(inThing),
                     character = {try(eval(parse(text = inThing)))},
                     expression = {try(eval(inThing))},
                     call = {try(eval(inThing))},
                     inThing)
    send(result)
  }
  
  send("bye")
  if(!is.null(timeout)) alarmc()
  if(quitAfter) q("no")
}

hasExpired <- function(ss){
  timeout <- ss$timeout
  timestamp <- ss$timestamp
  timeNow <- Sys.time()
  elapsedTime <- unclass(timeNow) - unclass(timestamp)
  return((!is.null(timeout)) && (elapsedTime > timeout))
}

print.serverSession <- function(x, ...){
  x$status <- if(hasExpired(x)) "expired" else "alive"
  x$timestamp <- format(x$timestamp)
  print.simple.list(x, ...)
}

serverSession <- function(){
  if(exists(".serverSession", env = globalenv()))
    get(".serverSession", env = globalenv())
  else NULL
}

validServerIsRunning <- function(fail = F){
  ss <- serverSession()
  nullServer <- is.null(ss)
  if(nullServer) expired <- TRUE
  else expired <- hasExpired(ss)
  if(fail && nullServer) stop("No server")
  if(fail && expired)    stop("Expired server")
  !(nullServer || expired)
}

ensureValidServer <- function(...){
  if(!validServerIsRunning()){
    if(exists(".serverSession", envir = globalenv()))
      rm(list = ".serverSession", envir = globalenv())
    startRemoteServer(...)
  }
}

sendToServer <- function(object){
  ss <- serverSession()
  if(is.null(ss)) stop("No server")
  if(hasExpired(ss)) stop("Expired server")
  on.exit(assign(".serverSession", ss, env = globalenv()))
  ss$timestamp <- Sys.time()
  sendSocketObject(object, ss$serverHost, ss$serverPort)
}

receiveFromServer <- function(){
  ss <- serverSession()
  if(is.null(ss)) stop("No server")
  if(hasExpired(ss)) stop("Expired server")
  on.exit(assign(".serverSession", ss, env = globalenv()))
  ss$timestamp <- Sys.time()
  receiveSocketObject(ss$clientPort)
}

sendExpression <- function(expr){
  sendToServer(substitute(expr))
  return(receiveFromServer())
}

endServerSession <- function(){
  ss <- serverSession()
  if(!is.null(ss)){
    if(hasExpired(ss)) ## kill the remote R process if it's still there
      ssh(paste("kill -KILL", ss$serverPid), host = ss$serverHost)
    else try(sendExpression(break))
    rm(list = ".serverSession", envir = globalenv())
  }
}
