\name{lags}
\alias{lag.default}
\alias{lag.tis}
\alias{lags}
\title{Lag a Time Series}
\description{
  Compute a lagged version of a time series, shifting the time base
  forward by a given number of observations. \code{lag} creates a single
  lagged series, while \code{lags} can create several lags at once.
}
\usage{
\method{lag}{default}(x, k = 1, ...)
\method{lag}{tis}(x, k = 1, ...)
lags(x, lags, name = "")
}
\arguments{
  \item{x}{A vector or matrix or univariate or multivariate time series
	(including \code{tis} series)}
  \item{k}{The number of lags}
  \item{\dots}{further arguments to be passed to or from methods}
  \item{lags}{vector of lag numbers}
  \item{name}{string or a character vector of names to be used in
	constructing column names for the returned series}
}
\details{
  Vector or matrix arguments 'x' are coerced to time series.
  
  For \code{lags}, column names are constructed as follows: If
  \code{name} is supplied and has as many elements as \code{x} has
  columns, those names are used as the base column names.  Otherwise the
  column names of \code{x} comprise the base column names, or if those
  don't exist, the first \code{ncols(x)} letters of the alphabet are
  used as base names.  Each column of the returned series has a name
  consisting of the basename plus a suffix indicating the lag number for
  that column.
}
\value{
  Both functions return a time series (\code{ts} or \code{tis}) object.
  If the \code{lags} argument to the \code{lags} function argument has
  more than one element, the returned object will have a column for each
  lag, with \code{NA}'s filling in where appropriate. 
}
\author{ Jeff Hallman }
\note{
  These versions of \code{lag} reverse the meaning of the sign of
  \code{k}.  The standard \R version of \code{lag} says that a series
  lagged by a positive \code{k} starts \emph{earlier}.  The opposite is
  true for this version, to maintain consistency with the common usage
  of 'first lag, second lag' and so on here at Federal Reserve Board. 
}
\keyword{ ts }
