\name{POSIXct}
\alias{POSIXct}
\alias{POSIXct.jul}
\alias{POSIXct.ti}
\alias{POSIXct.default}
\alias{POSIXlt}
\alias{POSIXlt.jul}
\alias{POSIXlt.ti}
\alias{POSIXlt.default}
\title{Date-time Constructor Functions}
\description{
  Functions to create objects of classes \code{"POSIXlt"} and
  \code{"POSIXct"} representing calendar dates and times.
}
\usage{
POSIXct(x, \dots)
POSIXlt(x, \dots)
\method{POSIXct}{jul}(x, \dots)
\method{POSIXct}{ti}(x, offset = 1, \dots)
\method{POSIXct}{default}(x, \dots)
\method{POSIXlt}{jul}(x, \dots)
\method{POSIXlt}{ti}(x, \dots)
\method{POSIXlt}{default}(x, \dots)
}
\arguments{
  \item{x}{An object to be converted.}
  \item{offset}{a number between 0 and 1 specifying where in the period
	represented by the \code{ti} object \code{x} the desired time falls.
	\code{offset = 1} gives the first second of the period and
	\code{offset = 1} the last second, \code{offset = 0.5} the middle
	second, and so on.}
  \item{\dots}{
	other args passed to \code{ISOdatetime} (\code{POSIXct.jul} and
	\code{POSIXct.ti}), \code{as.POSIXct} or \code{as.POSIXlt} as
	appropriate.  May include a \code{tz} argument to specify a
	timezone, \emph{if one is required}. System-specific, but \code{""}
	is the current timezone, and \code{"GMT"} is UTC (Coordinated
	Universal Time, in French).
  } 
}
\details{
  The default methods \code{POSIXct.default} and \code{POSIXlt.default}
  do nothing but call \code{as.POSIXct} and \code{as.POSIXlt},
  respectively.  The \code{POSIXct.ti} method can take an \code{offset}
  argument as explained above, and the \code{POSIXct.jul} method can
  handle \code{jul} objects with a fractional part.  The \code{ti} and
  \code{jul} methods for \code{POSIXlt} just call the \code{POSIXct}
  constructor and then convert it's value to a \code{POSIXlt} object.
}
\value{
  \code{POSIXct} and \code{POSIXlt} return objects of the
  appropriate class.  If \code{tz} was specified it will be reflected in
  the "tzone" attribute of the result.
}
\author{ Jeff Hallman }
\seealso{
  \code{\link{as.POSIXct}} and \code{link{as.POSIXlt}} for the default
  conversion functions, and \link{DateTimeClasses} for details of the
  classes. 
}
\keyword{utilities}
\keyword{chron}
