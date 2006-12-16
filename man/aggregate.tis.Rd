\name{aggregate.tis}
\alias{aggregate.tis}
\alias{aggregate.ts}
\title{Compute Summary Statistics of Time Series Subsets}
\usage{
\method{aggregate}{tis}(x, FUN = sum, \dots)
\method{aggregate}{ts}(x, nfrequency = 1, FUN = sum, ndeltat = 1,
          ts.eps = getOption("ts.eps"), \dots)
}
\description{
  Splits the data into subsets, computes summary statistics for each,
  and returns the result in a convenient form.
}
\arguments{
  \item{x}{a \code{ts} or \code{tis} time series.}
  \item{FUN}{a scalar function to compute the summary statistics which
    can be applied to all data subsets.}
  \item{nfrequency}{new number of observations per unit of time; must
    be a divisor of the frequency of \code{x}.}
  \item{ndeltat}{new fraction of the sampling period between
    successive observations; must be a divisor of the sampling
    interval of \code{x}.}
  \item{ts.eps}{tolerance used to decide if \code{nfrequency} is a
    sub-multiple of the original frequency.}
  \item{\dots}{further arguments passed to or used by methods.}
}
\details{
  These are time series methods for the generic \code{aggregate}
  function.

  \code{aggregate.ts} has been reimplemented in \code{package:fame}
  to insure that the resulting time series starts on
  a boundary of the new frequency.  Suppose, for example, that \code{x}
  is a monthly series starting in February 1990, \code{nfrequency} is 4,
  and \code{FUN} is \code{mean}.  The \code{package:frb} implementation
  will return a series whose first observation is the average of the
  April, May and June observations of the input series, and the first
  element of it's \code{tsp} will be 1990.25. The quarters end in
  March, June, September and December. The first two monthly
  observations (February and March) are ignored because they don't span
  a quarter. 

  The \code{package:stats} implementation would return a quarterly
  series whose first observation is the average of the February, March
  and April monthly observations, and it's \code{tsp} will start with
  1990.083, which corresponds to quarters ending in January, April, July
  and October.  In our experience at the Fed, this is not the expected
  behavior. 
  
  \code{aggregate.ts} and \code{aggregate.tis} operate similarly.  If
  \code{x} is not a time series, it is coerced to one.  Then, the
  variables in \code{x} are split into appropriate blocks of length 
  \code{frequency(x) / nfrequency}, and \code{FUN} is applied to each
  such block, with further (named) arguments in \code{\dots} passed to
  it.  The result returned is a time series with frequency
  \code{nfrequency} holding the aggregated values.

}
\author{Jeff Hallman}
\seealso{
  \code{\link{apply}}, \code{\link{lapply}}, \code{\link{tapply}},
  \code{\link{aggregate}}, and \code{\link{convert}}.
}
\examples{
z <- tis(1:24, start = latestJanuary()) ## a monthly series
aggregate(z, nf = 4, FUN = mean)        ## quarterly average
aggregate(z, nf = 1, FUN = function(x) x[length(x)]) ## December is annual level
}
\keyword{ts}
