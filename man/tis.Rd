\name{tis}
\alias{tis}
\alias{as.tis}
\alias{as.tis.tis}
\alias{as.tis.ts}
\alias{as.tis.default}
\alias{is.tis}
\title{ Time Indexed Series }
\description{
  The function \code{tis} is used to create time-indexed series objects.

  \code{as.tis} and \code{is.tis} coerce an object to a time-indexed
  series and test whether an object is a time-indexed series.
}
\usage{
tis(data, start = 1, tif = NULL, frequency = NULL, end = NULL)
as.tis(x, ...)
\method{as.tis}{ts}(x, ...)
\method{as.tis}{tis}(x, ...)
\method{as.tis}{default}(x, ...)
is.tis(x)
}
\arguments{
  \item{data}{
	a numeric vector or matrix of the observed time-series values.
  }
   \item{start}{
	 the time of the first observation. This can be a \code{ti} object, or
	 anything that \code{ti(start, tif = tif, freq = frequency)}, can turn into a {ti} object. 
   }
  \item{\dots}{
	other args to be passed to the method called by the generic
	function.  \code{as.tis.default} passes \code{x} and \dots to the
	constructor function \code{tis}.
  }
  \item{tif}{
	a ti Frequency, given as either a numerical code or a string.
	\code{tif()} with no arguments returns a list of the allowable
	numerical codes and names.  
  }
  \item{frequency}{
	As an alternative to supplying a \code{tif}, some \code{tif}s can
	alternatively be specified by their frequency, such as 1 (annual), 2
	(semiannual), 4 (quarterly), 6 (bimonthly), 12 (monthly), 24
	(semimonthly), 26 (biweekly), 36 (tenday), 52 (weekly), 262
	(business) and 365 (daily).  Many frequencies have multiple
	\code{tif}s associated with them.  For example, all of the
	\code{tif}s (wsunday, wmonday, ..., wsaturday) have frequency 52. In
	this case, specifying \code{freq} gets you the default weekly
	\code{tif} wmonday.
  }
  \item{end}{
	the time of the last observation, specified in the same way as \code{start}.
  }
  \item{x}{
	object to be tested (\code{is.tis}) or converted into a \code{tis}
	object.  As described in the details below, \code{as.tis} can deal with
	several different kinds of \code{x}.  
  }
}
\details{
  The function \code{tis} is used to create \code{tis} objects, which
  are vectors or matrices with class of \code{"tis"} and a \code{start}
  attribute that is a \code{ti} (time index) object.  Time-indexed
  series are a form of time series that is more flexible
  than the standard \code{ts} time series. While observations for a
  \code{ts} object are supposed to have been sampled at equispaced
  points in time, the observation times for a \code{tis} object are the
  times given by successive increments of the more flexible time index
  contained in the series \code{start} attribute.  There is a close
  correspondence between Fame time series and \code{tis} objects, in
  that all of the Fame frequencies have corresponding \code{tif} codes.

  \code{tis} objects operate much like vanilla R \code{ts} objects.
  Most of the methods implemented for \code{ts} objects have \code{tis}
  variants as well.  Evaluate \code{methods(class = "tis")} to see a
  list of them.

  One way or another, \code{tis} needs to figure out how to create a
  \code{start} attribute.  If \code{start} is supplied, the function
  \code{ti} is called with it, \code{tif} and \code{frequency} as
  arguments.  The same process is repeated for \code{end} if it was
  supplied. If only one of \code{start} and \code{end} was supplied, the
  other is inferred from it and the number of observations in \code{data}. If
  both \code{start} and \code{end} are supplied, the function \code{rep}
  is used to make \code{data} the length implied by \code{end - start + 1}.

  \code{as.tis} is a generic function with specialized methods for other
  kinds of time series. The fallback default method calls
  \code{tis(x, ...)}.
}
\value{
  \code{tis} and \code{as.tis} return time-indexed series.
  \code{is.tis} returns TRUE or FALSE.
}
\author{ Jeff Hallman }
\note{
  The \code{tis} class is a rewrite of the FRB Splus \code{td} class,
  which itself was based on Jim Berry's TD series idea in Speakeasy.
}
\seealso{ Compare with \code{\link{ts}}.  See \code{\link{ti}} for
  details on time indexes.  \code{\link{cbind.tis}} combines several
  time indexed series into a multivariate \code{tis}, while
  \code{\link{mergeSeries}} merges series, and \code{\link{convert}} and
  \code{\link{aggregate}} convert series from one frequency to another.
  \code{\link{start.tis}} and \\code{\link{end.tis}} return \code{ti}
  objects, while \code{\link{ti.tis}} returns a vector \code{ti}. There
  is a print method \code{\link{print.tis}} and several plotting
  methods, including \code{\link{lines.tis}} and \code{\link{points.tis}}.
  The \code{\link{window.tis}} method is also sufficiently different
  from the \code{ts} one to deserve its own documentation.
}
\examples{
tis(1:48, start = c(2000, 1), freq = 12)
tis(1:48, start = ti(20000101, tif = "monthly"))    ## same result
tis(0, start = c(2000,1), end = c(2000,52), tif = "weekly")
}
\keyword{ ts }
