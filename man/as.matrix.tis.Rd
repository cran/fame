\name{as.matrix.tis}
\alias{as.matrix.tis}
\title{Create a Matrix from a Time Indexed Series}
\usage{
\method{as.matrix}{tis}(x, \dots)
}
\description{The function adds a \code{dim} attribute of
  \code{c(length(x), 1)} to its argument unless it already has a
  \code{dim} attribute of length 2.
}
\arguments{
  \item{x}{a \code{tis} object}
  \item{\dots}{ignored}
}
\value{
  A \code{tis} object with a \code{dim} attribute of length 2.
}
\author{ Jeff Hallman }
\keyword{ ts }
\keyword{ array }
