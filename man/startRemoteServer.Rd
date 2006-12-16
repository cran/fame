\name{clientServerR}
\alias{startRemoteServer}
\alias{serveHostAndPort}
\alias{hasExpired}
\alias{print.serverSession}
\alias{serverSession}
\alias{validServerIsRunning}
\alias{ensureValidServer}
\alias{sendToServer}
\alias{receiveFromServer}
\alias{sendExpression}
\alias{endServerSession}
\alias{startRemoteServer}
\title{Client - Server \R}
\description{These functions implement \R client and server sessions.

  The first function, \code{serveHostAndPort()} runs on the server end
  of the session.  It finds an available port on the host it is running
  on, and uses that host and port as the server end of a
  \code{serverSession} with the given client host and port.  It returns
  the \code{serverSession} to the client and begins a loop listening on
  the serverPort for \R objects to be sent through.  

  When an object arrives on the port, what happens depends on its mode.
  If the new arrival is an expression or call, it gets evaluated and the
  result is sent back to the client.  If the arriving object is a
  character string, it gets parsed and evaluated and the result sent
  back.  Any other mode of arriving object is just sent back.  All
  attempted evaluations take place in a \code{try} block, and a
  \code{try-error} gets returned to the client if the parsing or
  evaluating fails.  

  After handling the incoming object, the server returns to the top of
  the loop.  There are two ways to break out of the loop.  The first is
  to send the expresssion \code{break} to the server.  The other way,
  timing out, works only if the server runs on a real operating system,
  i.e., Unix or Linux.  If so, the server sets an alarm timer when first
  starting the loop, and resets it after each pass through the loop.  If
  the timer expires due to inactivity, the SIGALRM signal is sent to the
  server \R process, which kills it.

  All of the other functions documented here run on the client end of
  the session.  
  
  \code{startRemoteServer()} assumes that the machine it is running on
  is to be the client end of an \R server session.  It uses \code{ssh}
  to invoke \code{serveHostAndPort()} on the remote host, and waits for
  the remote server to return a serverSession object, which it then
  stores in the global environment as ".serverSession".

  \code{print.serverSession} is a print method for serverSession
  objects.

  \code{serverSession()} returns the serverSession stored in the global
  environment.

  \code{hasExpired()} checks a serverSessions timestamp, which is
  updated on each send or receive operation, against it's timeout and
  the current time.  Note that there is no good way to check with the
  server itself, since it may not be running.  If the server has already
  died, attempting to contact it will likely hang the current \R session.

  \code{endServerSession()} checks to see if there is a serverSession
  stored in the global environment, exiting if there isn't.  Assuming
  there is a session and it has not yet expired, the \code{break}
  expression is sent to kill it.  If the session timestamp indicates
  that the session has expired, the function asks the remote machine to
  kill the session serverPid process.  If the process has already died,
  this does no harm.

  \code{validServerIsRunning()} answers \code{TRUE} if
  \code{serverSession()} returns a serverSession that has not timed out
  yet.

  \code{ensureValidServer} returns the current serverSession if there is
  one, or starts a new one and returns that.

  \code{sendToServer()} and \code{receiveFromServer} do what their names
  imply.

  Finally, \code{sendExpression} sends its unevaluated argument
  (retrieved via \code{substitute}) to the server, and waits for the
  server to return a result, which becomes the return value of
  \code{sendExpression}. 
}
\usage{
serveHostAndPort(clientHost, clientPort, timeout = 3600, quitAfter = T)
startRemoteServer(host = getOption("remoteHost"), user. = user(), timeout = 3600)
hasExpired(ss)
print.serverSession(x, \dots)
serverSession()
endServerSession()
validServerIsRunning(fail = F)
ensureValidServer(\dots)
sendToServer(object)
receiveFromServer()
sendExpression(expr)
}
\arguments{
  \item{clientHost}{DNS name of the machine the R client is running on}
  \item{clientPort}{Port number on the client}
  \item{timeout}{number of seconds of inactivity after which the R
	server commits suicide}
  \item{quitAfter}{if \code{TRUE} (the default), upon breaking out of
	the receive-eval-return result loop, the server invokes \code{q("no")}
	to kill itself.  For testing, you may want to start the remote
	server by hand and invode \code{serveHostAndPort} with
	\code{quitAfter = FALSE} to see what is happening on the far end.}
  \item{host}{DNS name of the machine the R server is to run on}
  \item{user.}{user name the R server is to run under}
  \item{ss}{a serverSession object}
  \item{x}{a serverSession object}
  \item{\dots}{For \code{print.servserSession}, arguments to be passed
	on to \code{print.simple.list}.  For \code{ensureServerSession},
	arguments to be passed through to \code{startRemoteServer}}
  \item{fail}{logical.  If \code{TRUE} (not the default), raise an error
	exception if there is not a valid server session running}
  \item{object}{arbitrary \R object to send to the server}
  \item{expr}{an expression (entered without quotes) to send to the
  remote \R server, where it will be evaluated and the result returned.}
}
\value{ \code{serverSession()} returns the stored serverSession, while
  \code{hasExpired()} and \code{validServerIsRunning()} return \code{TRUE}
  or \code{FALSE}.
  
  \code{sendExpression()} returns the value that the given expression
  evaluated to on the server.

  The other functions return nothing of interest.
}
\note{
  Getting the detailed sequencing right for blocking sockets can be very
  frustrating, since the \R processes at both ends tend to hang on every
  mistake.  The easiest way to get the sequence right is to just use
  \code{startRemoteServer()} from the client end to get everything
  started, and then use \code{sendExpression()} to send expressions to
  the server and get results on the client.  Finally, you can call
  \code{endServerSession()} when you are finished to clean up, as in the
  example below.  An even better idea is to leave the server running,
  but call \code{endServerSession()} in your \code{.Last} function.

  The remote server is started via ssh, and this will usually require you to
  interactively supply a password.  No facility to store your password
  and use it when needed has been provided, as that would just be asking
  for security trouble.  On Windows, the \code{ssh} function uses the
  \code{plink} program to establish the ssh connection, and uses it's
  password prompt as well.
}
\author{Jeff Hallman}
\seealso{\code{\link{ssh}}}
\examples{
\dontrun{
startRemoteServer(host = "mralx2")
blah <- sendExpression(getfame("gdp.q", db = "us"))
endServerSession()
}}
\keyword{utilities}
