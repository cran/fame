\name{constantGrowthSeries}
\alias{fanSeries}
\alias{tunnelSeries}
\title{Constant Growth Series}
\description{Create \code{tis} time series that grow at constant rates.
}
\usage{
fanSeries(startValue, start, end, rates)
tunnelSeries(startValue, start, end, rate, spreads)
}
\arguments{
  \item{startValue}{starting value for the series at time \code{start}}
  \item{start}{a \code{ti} (Time Index) for the first observation.}
  \item{end}{a \code{ti} or something that can be turned into a
  \code{ti} giving the time index for the last observation.}
  \item{rates}{annual growth rate(s) for the series to be created}
  \item{rate}{annual growth rate for the series to be created}
  \item{spreads}{vector of 2 numbers giving the percentage values by
  which the starting values of the 'tunnel' series should be offset from
  \code{startValue}}
}
\value{
  \code{fanSeries} returns a multivariate series that starts on
  \code{start} and ends on \code{end}.  There are \code{length(rates)}
  columns. Each column begins at \code{startValue} and grows at the rate
  given by its corresponding element in \code{rates}. These are not
  true growth rates, rather each column has a constant first difference
  such that over the course of the first year, column i will grow
  \code{rates[i]} percent. This yields series that plot as straight
  lines. 

  \code{tunnelSeries} first calls \code{fanSeries} to create a
  univariate series running from \code{start} to \code{end} with a
  starting value of \code{startValue} and growing \code{rate} percent
  over the first year.  It returns a bivariate series with columns that are
  offset from that series by \code{spreads[1]} and \code{spreads[2]}
  percent of the \code{startValue}. 
}
\author{ Jeff Hallman }
\seealso{ \code{\link{growth.rate}}}
\keyword{ ts }
