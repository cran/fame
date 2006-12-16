\name{window.tis}
\alias{window.tis}
\title{
  Time windows for Time Indexed Series
}
\description{
  \code{window.tis} extracts the subset of the object \code{x} observed
  between the times \code{start} and \code{end}. 
}
\usage{
window.tis(x, start = NULL, end = NULL, extend = F, ...)
}
\arguments{
  \item{x}{
	a \code{tis} object
  }
  \item{start}{
	the start time of the period of interest.
  }
  \item{end}{
	the end time of the period of interest.
  }
  \item{extend}{
	logical.  If true, the \code{start} and \code{end} values are
	allowed to extend the series. If false, attempts to extend the
	series give a warning and are ignored. 
  }
  \item{\dots}{
	other arguments to this function are ignored.
  }
}
\details{
  The start and end times can be \code{ti} objects, or anything that
  \code{ti(z, tif = tif, freq = frequency)}, can turn into a \code{ti} object.
}
\value{
  A \code{tis} object that starts and ends at the given times.
}
\author{ Jeff Hallman }
\note{
  The replacement method \code{window<-.tis} has not been implemented.
  Use the subscript operator with a \code{ti} argument to replace values
  of a \code{tis} object.
}
\examples{
z <- tis(1:24, start = c(2001,1), freq = 12)
z2 <- window(z, start = 19991231, extend = TRUE) ## z2 extends back with NA's
window(z, end = end(z) - 3)
}
\keyword{ ts }
