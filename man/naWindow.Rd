\name{naWindow}
\alias{naWindow}
\title{Exclude NA Observations}
\description{Windows a \code{tis} or \code{ts} time series to cut off leading and trailing
  NA observations.
}
\usage{
naWindow(x, union = F)
}
\arguments{
  \item{x}{a \code{tis} or \code{ts} time series}
  \item{union}{see details below}
}
\details{
  For multivariate (multiple columns) series and \code{union = TRUE}, a
  row of \code{x} is considered to be NA if and only if all entries in
  that row are NA. If \code{union = FALSE} (the default), a row is
  considered to be NA if any of its entries is NA.

  
}
\value{A copy of \code{x} with leading and trailing NA observations deleted.
}

\author{ Jeff Hallman }
\seealso{ \code{\link{window}}}
\keyword{ ts }
