\name{is.scalar}
\alias{is.scalar}
\alias{isFameScalar}
\alias{isScalarOrTis}
\title{Check for (Fame or Numeric) Scalar or Time Index Series}
\description{
  \code{is.scalar} checks whether or not its input is a single, finite number.

  \code{isFameScalar} checks whether or not its argument could be a
  'scalar' in FAME parlance, i.e., if it is an atomic object of length
  one and not a time series or time indexed series. \code{ti} objects of
  length one are what FAME calls a 'date', other scalars are strings,
  numbers, and logicals of length one.

  \code{isScalarOrTis} checks whether its argument is a FAME scalar or a
  \code{tis} (Time Indexed Series) object.
}
\usage{
is.scalar(x)
isFameScalar(x)
isScalarOrTis(x)
}
\arguments{
  \item{x}{ object to be checked.  For \code{is.scalar}, it should be a number. }
}
\value{\code{TRUE} or \code{FALSE}.
}
\author{ Jeff Hallman }
\examples{
is.scalar(42)
isFameScalar("this is a scalar, since it has length one")
isFameScalar(c("returns FALSE", "since it has length two"))
isFameScalar(today())
isScalarOrTis(today())
}
\keyword{programming}
\keyword{math}
