\name{hms}
\alias{hms}
\title{ Hours, Minutes and Seconds from a Time Index or Jul}
\description{Extract the fractional part of a \code{ti} (time index) or
  \code{jul} (julian date) object as a normalized list of hours,
  minutes, and seconds.
}
\usage{
hms(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{jul} or something numeric that can be converted into
  a \code{jul} with a fractional part.}
}
\details{The fractional part of \code{x} is multiplied by 86400 (the
  number of seconds in a day) and rounded to get the number of seconds.
  This is then divided by 3600 to get the number of hours, and the
  remainder of that is divided by 60 to get the normalized number of
  minutes.  The remainder from the second division is the normalized
  number of seconds. 
}
\value{
  A list with components:
  \item{hours}{Normalized number of hours}
  \item{minutes}{Normalized number of minutes}
  \item{seconds}{Normalized number of seconds}
  See the details.
}
\author{ Jeff Hallman }
\note{Support for fractional days in \code{ti} and \code{jul} objects is
  relatively new and untested.  There is probably code lurking about
  that assumes the numeric parts of \code{ti} and \code{jul} objects are
  integers, or even code that may round them to make sure they are
  integers.  The fractional parts of \code{ti} and \code{jul} objects
  may not survive encounters with such code.  
}
\seealso{ \code{\link{ti}} and \code{\link{jul}}.  Also see
  \code{\link{hourly}} for information on intraday frequencies}
\examples{
hms(today() + 0.5)
hms(today())
hms(today() + 43201/86400)
}
\keyword{ chron }
