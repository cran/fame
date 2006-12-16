\name{alarmc}
\alias{alarmc}
\title{Interface to alarm() system call}
\description{This is a simple wrapper around the POSIX \code{alarm()}
  system call.  Unfortunately, it is not available on Windows.
}
\usage{
alarmc(seconds = 0)
}
\arguments{
  \item{seconds}{ integer number of seconds to wait before sending
  SIGALRM to the \R process, or \code{0} (zero) to cancel any existing timer.}
}
\details{\code{alarmc(seconds)} sets a timer running for \code{seconds}
  seconds and returns immediately. The default value \code{0} unsets the timer, if
  there is one.

  When the time is up, the SIGALRM signal is sent to the \R process.
  Since \R does not define a handler for this signal, the default
  handler is invoked, which terminates the \R process.
}
\references{ See the man page for alarm }
\author{Jeff Hallman}
\note{Microsoft, in it's infinite wisdom, does not provide the
  \code{alarm()} system call in its C libraries.  Instead, they expect
  you to use the \code{SetTimer} function to do this kind of stuff, but
  SetTimer is not available to a console-mode application.  I do wonder
  how they get away with claiming POSIX compatibility.
}
\seealso{ \code{\link{Sys.sleep}}}
\examples{
\dontrun{
## wait up to 10 minutes for a socket connection or die
alarmc(600) 
conn <- socketConnection(port = 40001, server = T, blocking = T)
alarmc(0)  ## turn off the timer
incomingText <- readLines(conn)
close(conn)
}
}
\keyword{utilities}
