\name{between}
\alias{between}
\title{Check for Inclusion in a Closed Interval }
\description{
  Returns a logical vector like \code{y} showing if each element
  lies in the closed interval \code{[min(x1, x2), max(x1, x2)]}.
}
\usage{
between(y, x1, x2)
}
\arguments{
  \item{y}{ a numeric object }
  \item{x1}{a number}
  \item{x2}{a number}
}
\value{A logical object like \code{y}.
}
\author{ Jeff Hallman }
\examples{
mat <- matrix(rnorm(16), 4, 4)
mat
between(mat, -2, 1)
}
\keyword{ math }
