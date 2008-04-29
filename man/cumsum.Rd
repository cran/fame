\name{cumsum.tis}
\alias{cumsum.tis}
\alias{cumprod.tis}
\alias{cummin.tis}
\alias{cummax.tis}
\title{Cumulative Sums, Products, and Extremes}
\description{
  Return a \code{tis} whose elements are the cumulative sums, products,
  minima or maxima of the elements of the argument.
}
\usage{
\method{cumsum}{tis}(x)
\method{cumprod}{tis}(x)
\method{cummax}{tis}(x)
\method{cummin}{tis}(x)
}
\arguments{
  \item{x}{a \code{tis} series.}
}
\details{
  These are \code{tis} methods for generic functions.
}
\value{
  A \code{tis} like \code{x}.  An \code{NA} value in \code{x} causes the
  corresponding and following elements of the return value to be
  \code{NA}, as does integer overflow in \code{cumsum} (with a warning). 
}
\seealso{\code{\link{cumsum}}, \code{\link{cumprod}},
  \code{\link{cummin}}, \code{\link{cummax}}}
\keyword{ts}
\keyword{arith}
