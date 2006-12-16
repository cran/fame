\name{dateRange}
\alias{dateRange}
\title{Start and End Time Indices for a Series}
\description{Returns the starting and ending times of a series in a
  \code{ti} object of length 2.
}
\usage{
dateRange(x)
}
\arguments{
  \item{x}{a \code{ts} or \code{tis} time series}
}
\value{a \code{ti} (Time Index) object of length two.  The first element
  is the starting time index, while the second is the ending time index.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{start}}, \code{\link{end}}, \code{\link{ti}}, \code{\link{tis}} }
\examples{
aTs <- ts(1:24, start = c(2001, 1), freq = 12)
aTis <- as.tis(aTs)
dateRange(aTs)
dateRange(aTis)
}
\keyword{ts}
