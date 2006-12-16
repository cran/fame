\name{format.ti}
\alias{format.ti}
\title{ Time Index Conversion to Character }
\description{
  Format a time index object for printing
}
\usage{
format.ti(x, ..., tz = "GMT")
}
\arguments{
  \item{x}{ a time index object }
  \item{tz}{ A timezone specification to be used for the conversion if
  \code{x} has an intraday frequency. System-specific, but \code{""} is
  the current time zone, and \code{"GMT"} is UTC.}
  \item{\dots}{ other args passed on to \code{format.POSIXlt}.}
}
\details{
  \code{x} is converted to a \code{POSIXlt} object and then
  \code{format.POSIXlt} takes over.
}
\value{
  a character vector representing \code{x}
}
\author{ Jeff Hallman }
\note{
  \code{format.POSIXlt} has been modified to understand two additional
  format symbols in addition to those documented in
  \code{link{strftime}}: "\%q" in the format string is replaced with the
  quarter number (1 thru 4) and "\%N" is replaced with the first letter
  of the month name.
}
\seealso{ \code{\link{format.POSIXlt}}, \code{\link{strftime}} }
\examples{
format(today() + 0:9, "\%x")
}
\keyword{ chron }
