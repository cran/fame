\name{hostName}
\alias{hostName}
\title{DNS Host name}
\description{
  On Linux, this is just \code{tolower(Sys.info()["nodename"])}. On
  Windows the node name does not include the DNS suffix, which has to be
  found by parsing the output of the \code{ipconfig} program.
}
\usage{
hostName()
}
\value{The DNS host name as a string.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{availablePort}} to find a port number that is not
  in use.}
\keyword{utilities}
