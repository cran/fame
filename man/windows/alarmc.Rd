\name{alarmc}
\alias{alarmc}
\title{Interface to alarm() system call}
\description{On Unix, this is a simple wrapper around the POSIX \code{alarm()}
  system call.  On Windows, no such system call is available, so this
  function does nothing.  It is here only to stop \code{R CMD check}
  from complaining about it not being here.
}
\usage{
alarmc(seconds = 0)
}
\arguments{
  \item{seconds}{ ignored}
}

\author{Jeff Hallman}
\note{Microsoft, in it's infinite wisdom, does not provide the
  \code{alarm()} system call in its C libraries.  Instead, they expect
  you to use the \code{SetTimer} function to do this kind of stuff, but
  SetTimer is not available to a console-mode application.  I do wonder
  how they get away with claiming POSIX compatibility.
}
\seealso{ \code{\link{Sys.sleep}}}
\keyword{utilities}
