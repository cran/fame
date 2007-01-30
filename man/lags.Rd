\name{lags}
\alias{lag.tis}
\alias{lags}
\alias{Lag}
\alias{Lags}
\title{Lag a Time Series}
\description{
  \code{lag} creates a lagged version of a time series, shifting the time base
  forward by a given number of observations. \code{Lag} does exactly the
  opposite, shifting the time base backwards by the given number of
  observations.  \code{lag} and \code{Lag} create a single lagged
  series, while \code{lags} and \code{Lags} can create a multivariate
  series with several lags at once.
}
\usage{
\method{lag}{tis}(x, k = 1, ...)

Lag(x, k = 1, ...)
lags(x, lags, name = "")
Lags(x, lags, name = "")
}
\arguments{
  \item{x}{A vector or matrix or univariate or multivariate time series
	(including \code{tis} series)}
  \item{k}{The number of lags. For \code{lag}, this is
	the number of time periods that the series is shifted \emph{forward},
	while for \code{Lag} it is the number of periods that the series is
	shifted \emph{backwards}.} 
  \item{\dots}{further arguments to be passed to or from methods}
  \item{lags}{vector of lag numbers. For code \code{lags}, each element
	gives a number of periods by which \code{x} is to be shifted
	\emph{forward}, while for \code{Lags}, each element gives a number
	of periods by which \code{x} is to be shifted \emph{backwards}.}
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
\keyword{ ts }
