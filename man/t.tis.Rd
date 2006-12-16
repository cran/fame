\name{t.tis}
\alias{t.tis}
\title{Matrix Transpose}
\description{Returns the transpose of \code{as.matrix(x)}}
\usage{
\method{t}{tis}(x)
}
\arguments{
  \item{x}{a \code{tis} object.  If \code{x} is univariate, it will be
	treated as if it were a single-column matrix, so it's transpose will
	be a single-row matrix.}
}
\value{A matrix, see \code{\link{t}}. Note that this is \strong{not} a
  time series.}
\seealso{ \code{\link{t}}, \code{\link{tis}}}
\examples{
a <- tis(matrix(1:30, 5,6), start = latestMonth())
a
t(a)  ##i.e.,  a[i, j] == t(a)[j, i] for all i,j, and t(a) is NOT a time series
}
\keyword{ts}
\keyword{array}
