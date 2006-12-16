\name{as.data.frame.tis}
\alias{as.data.frame.tis}
\title{
  Coerce to a Data Frame
}
\description{
  Coerce a Time Indexed Series to a data frame.
}
\usage{
\method{as.data.frame}{tis}(x, \dots)
}
\arguments{
  \item{x}{a \code{tis} series}
  \item{\dots}{ other args passed on to \code{as.data.frame.matrix} or
  \code{as.data.frame.vector}}
}
\value{a data frame.
}
\details{The function is very simple: it calls
  \code{as.data.frame.matrix} if \code{x} is a matrix, or
  \code{as.data.frame.vector} if it is not. 
}
\seealso{
  \code{\link{data.frame}}
}
\keyword{ts}
