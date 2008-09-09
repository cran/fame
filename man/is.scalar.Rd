\name{isFameScalar}
\alias{isFameScalar}
\title{Check for (Fame or Numeric) Scalar or Time Index Series}
\description{
  \code{isFameScalar} checks whether or not its argument could be a
  'scalar' in FAME parlance, i.e., if it is an atomic object of length
  one and not a time series or time indexed series. \code{ti} objects of
  length one are what FAME calls a 'date', other scalars are strings,
  numbers, and logicals of length one.
}
\usage{
isFameScalar(x)
}
\arguments{
  \item{x}{ object to be checked. }
}
\value{\code{TRUE} or \code{FALSE}.
}
\author{ Jeff Hallman }
\examples{
isFameScalar("this is a scalar, since it has length one")
isFameScalar(c("returns FALSE", "since it has length two"))
isFameScalar(today())
}
\keyword{programming}
\keyword{math}
