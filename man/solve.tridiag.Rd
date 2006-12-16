\name{solve.tridiag}
\alias{solve.tridiag}
\title{Solve a Tridiagonal System of Equations}
\description{
  This function solves the equation \code{a \%*\% x = b} for \code{x},
  where \code{a} is tridiagonal and \code{b} can be either a vector or a
  matrix. 
}
\usage{
\method{solve}{tridiag}(a, b, \dots)
}
\arguments{
  \item{a}{a \code{tridiag} object: a square tridiagonal (all zeroes
	except for the main diagonal and the diagonals immediately above and
	below it) matrix containing the coefficients of the linear system.}
  \item{b}{a vector or matrix giving the right-hand side(s) of the
	linear system.  If missing, \code{b} is taken to be an identity matrix
	and the function will return the inverse of \code{a}.}
  \item{\dots}{ignored}
}
\details{
  Uses the LINPACK \code{dgtsv} routine.
}
\seealso{\code{\link{solve}}}
\keyword{algebra}
