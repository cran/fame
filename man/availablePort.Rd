\name{availablePort}
\alias{availablePort}
\alias{portsInUse}
\title{Find Available TCP Port or Ports In Use }
\usage{
portsInUse()
availablePort()
}
\description{
  \code{portsInUse} returns a vector of UDP and TCP port numbers
  currently being used by the operating system.  Under Linux, these are
  the ports listed in \code{/proc/net/[u|t]cp}, while for Windows they
  are the ports listed by \code{netstat -an}.

  Under either operating system, \code{availablePort} returns the first
  TCP port number greater than 40000 that is not currenly in use. 
}
\value{\code{portsInUse} returns a numeric vector.  \code{availablePort}
  returns an integer.}
\author{Jeff Hallman}
\seealso{\code{\link{hostName}}}
\keyword{utilities}
