\name{commandLineString}
\alias{commandLineString}
\title{Command Line Arguments}
\usage{
commandLineString()
}
\description{
  \code{commandLineString} returns whatever followed \code{--args} on
  the command line that started \R. 
}
\author{Jeff Hallman}
\seealso{\code{\link{commandArgs}}}
\examples{
\dontrun{
if(length(.cmd <- commandLineString()) > 0)
    try(source(textConnection(.cmd), echo = T, prompt.echo = "> "))
}}
\keyword{environment}
\keyword{sysdata}
\keyword{programming}
