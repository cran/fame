\name{as.POSIXct.jul}
\alias{as.POSIXct.jul}
\alias{as.POSIXct.ti}
\alias{as.POSIXlt}
\alias{as.POSIXlt.Date}
\alias{as.POSIXlt.default}
\alias{as.POSIXlt.jul}
\alias{as.POSIXlt.POSIXlt}
\alias{as.POSIXlt.ti}
\title{Date-time Conversion Functions}
\description{
  Functions to create objects of classes \code{"POSIXlt"} and
  \code{"POSIXct"} representing calendar dates and times.
}
\usage{
\method{as.POSIXct}{jul}(x, \dots)
\method{as.POSIXct}{ti}(x, \dots)
as.POSIXlt(x, tz = "", \dots)
\method{as.POSIXlt}{default}(x, tz = "", \dots)
\method{as.POSIXlt}{Date}(x, \dots)
\method{as.POSIXlt}{jul}(x, \dots)
\method{as.POSIXlt}{ti}(x, \dots)
\method{as.POSIXlt}{POSIXlt}(x, tz = "", \dots)
}
\arguments{
  \item{x}{An object to be converted.}
  \item{tz}{A timezone specification to be used for the conversion,
    \emph{if one is required}.  System-specific, but \code{""}
    is the current timezone, and \code{"GMT"} is UTC
    (Coordinated Universal Time, in French).}
  \item{\dots}{other args passed to underlying functions}
}
\details{
  The \code{as.POSIX*} functions convert an object to one of the two
  classes used to represent date/times (calendar dates plus time to the
  nearest second).  They can take convert a wide variety of objects,
  including objects of the other class and of classes \code{"Date"},
  \code{"date"} (from package \pkg{\link[date:as.date]{date}} or
  \pkg{\link[survival:as.date]{survival}}), \code{"chron"} and
  \code{"dates"} (from package \pkg{\link[chron]{chron}}) to these
  classes.  Dates are treated as being at midnight UTC.

  They can also convert character strings of the formats
  \code{"2001-02-03"} and \code{"2001/02/03"} optionally followed by
  white space and a time in the format \code{"14:52"} or
  \code{"14:52:03"}.  (Formats such as \code{"01/02/03"} are ambiguous
  but can be converted via a format specification by
  \code{\link{strptime}}.)

  Logical \code{NA}s can be converted to either of the classes, but no
  other logical vectors can be.

  \code{asPOSIXlt} is generic, while the other functions documented here are
  methods for the generic \code{as.POSIX*} functions that convert
  objects to the \code{POSIXct} and \code{POSIXlt} classes.
}
\value{
  \code{as.POSIXct} and \code{as.POSIXlt} return objects of the
  appropriate class.  If \code{tz} was specified, \code{as.POSIXlt}
  will give an appropriate \code{"tzone"} attribute.
}
\note{
  The standard \code{as.POSIXlt} function in \code{package:base} is not
  generic. That function has been renamed \code{as.POSIZlt.default} and
  replaced with a generic function calling \code{UseMethod}.
}
\author{ Jeff Hallman }
\seealso{
  \code{\link{as.POSIXct}} and \code{link{as.POSIXlt}} for the generic
  functions, and \link{DateTimeClasses} for details of the classes.
}
\keyword{utilities}
\keyword{chron}
