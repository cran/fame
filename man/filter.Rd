\name{Filter}
\alias{Filter}
\alias{Filter.default}
\alias{Filter.tis}
\title{Linear Filtering on a Time Series}
\usage{
Filter(x, \dots)
\method{Filter}{tis}(x, \dots)
\method{Filter}{default}(x, \dots)
}
\arguments{
  \item{x}{a univariate or multivariate time series.}
  \item{\dots}{arguments passed along to \code{filter}.}
}
\description{
  Applies linear filtering to a univariate time series or to each series
  separately of a multivariate time series.
}
\value{
  A \code{tis} time indexed series if \code{x} has class \code{tis},
  otherwise a class \code{ts} time series.  Leading and trailing
  \code{NA} values are stripped.
}
\seealso{ \code{\link{filter}}}
\examples{
x <- tis(1:100, start = c(2000,1), freq = 12)
Filter(x, rep(1, 3))
Filter(x, rep(1, 3), sides = 1)
Filter(x, rep(1, 3), sides = 1, circular = TRUE)
}
\keyword{ts}
