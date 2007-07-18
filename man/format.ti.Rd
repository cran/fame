\name{format.ti}
\alias{format.ti}
\alias{format.jul}
\alias{as.character.ti}
\alias{as.character.jul}
\title{ Convert Time Index or Jul to Character }
\description{
  \code{format} formats a jul or time index object for printing.
  \code{as.character} for a \code{jul} or \code{ti} object is
  essentially just an alias for \code{format}.
}
\usage{
\method{format}{ti}(x, ..., tz = "")
\method{format}{jul}(x, ...)
\method{as.character}{ti}(x, ...)
\method{as.character}{jul}(x, ...)
}
\arguments{
  \item{x}{ a \code{jul} or \code{ti} (time index) object }
  \item{tz}{ A timezone specification to be used for the conversion if
	\code{x} has an intraday frequency. System-specific, but \code{""} is
	the current time zone, and \code{"GMT"} is UTC.}
  \item{\dots}{ other args passed on to \code{format.POSIXlt}.}
}
\details{
  The \code{as.character} methods do nothing but call the corresponding
  \code{format} methods.
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
as.character(jul(today()))
}
\keyword{ chron }
