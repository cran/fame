\name{ssh}
\alias{ssh}
\title{Execute a command on another machine}
\description{This function composes an ssh (Secure SHell) command to run
  something on another machine and invokes it via \code{system()}. It
  uses the \code{plink} program (part of the open-source PuTTY suite) to
  send a command to a remote computer via the \code{ssh} protocol.
}
\usage{
ssh(command, host = getOption("remoteHost"), user. = user(),
    password = "", wait = F, ...)
}
\arguments{
  \item{command}{command to be executed on the remote machine }
  \item{host}{hostname of the remote machine}
  \item{user.}{username on the remote machine}
  \item{password}{password on the remote machine.  If missing, the
    function will prompt for it.}
  \item{wait}{if \code{TRUE}, return only after \code{command} has
  finished running on the remote system.  If \code{FALSE} (the default),
  return immediately after sending \code{command} to the remote system.}
  \item{\dots}{ additional arguments passed to \code{system}}
}
\value{The return value is whatever the \code{system()} function
  returns.  If \code{\dots} includes \code{intern = T}, this will be
  whatever the ssh or plink returned.
}
\author{ Jeff Hallman }
\note{
  This is a very simple-minded implementation.  I did just enough
  work on it to get the \code{startRemoteServer} function working and
  quit while I was ahead.  No error checking is done. The Linux version
  assumes that the \code{ssh} command is available on your path, while
  the Windows implementation makes the same assumption about the
  \code{plink} program.
}
\examples{
\dontrun{
ssh("uname -a", host = "localhost")
}}
\keyword{utilities}
