\name{fameIsScalar}
\alias{fameIsScalar}
\title{Check for (Fame or Numeric) Scalar or Time Index Series}
\description{
  \code{fameIsScalar} checks whether or not its argument could be a
  'scalar' in FAME parlance, i.e., if it is a character vector or an atomic object of length
  one and not a time series or time indexed series. \code{ti} objects of
  length one are what FAME calls a 'date', other scalars are strings,
  numbers, and logicals of length one. A vector of strings may be
  represented in FAME as a 'namelist'.
}
\usage{
fameIsScalar(x)
}
\arguments{
  \item{x}{object to be checked. }
}
\value{\code{TRUE} or \code{FALSE}.
}
\examples{
fameIsScalar("this is a scalar, since it has length one")
fameIsScalar(c("returns FALSE", "since it has length two"))
fameIsScalar(tis::today())
}
\keyword{programming}
\keyword{math}
