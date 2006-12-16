\name{tiDaily}
\alias{tiDaily}
\alias{tiBusiness}
\title{ Daily and Business Day Time Indexes }
\description{Return a daily or business day \code{ti} corresponding to a
  specified position within a time index.}
\usage{
tiDaily(xTi, offset = 1)
tiBusiness(xTi, offset = 1)
}
\arguments{
  \item{xTi}{
	a \code{ti} object or something that the \code{ti()}
	function can turn into a \code{ti} object }
  \item{offset}{
	for \code{ti} \code{xTi}, a number in the range [0,1] telling where in the
	period represented by \code{x} to find the day.  0 means the first
	day of the period, 1 the last day of the period, and fractional
	values for in-between day.}
}
\value{
  \code{tiDaily} converts its first argument to a \code{jul} using the
  offset provided, and returns a daily \code{ti} for that day.
  
  \code{tiBusiness} converts its first argument to a \code{jul} using the
  offset provided, and returns a "business" \code{ti} for that day.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{ti}}, \code{\link{jul}} }
\keyword{ chron }
\keyword{ ts }
