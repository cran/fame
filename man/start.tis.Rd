\name{start.tis}
\alias{start.tis}
\alias{start<-}
\alias{end.tis}
\title{ Starting and ending time indexes }
\description{
  Return the start or end time index for a \code{tis} object.
}
\usage{
\method{start}{tis}(x, \dots)
\method{end}{tis}(x, \dots)
start(x) <- value
}
\arguments{
  \item{x}{ a \code{tis} object }
  \item{value}{desired \code{start} attribute}
  \item{\dots}{ignored}
}
\value{
  \code{start.tis} returns the \code{start} attribute of \code{x}, while
  \code{end.tis} returns \code{start(x) + nobs(x) - 1}.
  \code{start(x) <- value} returns the series \code{x} shifted such that
  it's starting time is \code{value}.
}
\author{ Jeff Hallman }
\note{
  \code{start} and \code{end} are generic functions with default methods
  that assume \code{x} has (or can be given) a \code{tsp} attribute.
  The default methods return a two vector as c(year, period), while the
  methods described here return infinitely more useful \code{ti} objects.
}
\seealso{ \code{\link{start}},\code{\link{end}}}
\examples{
x <- tis(numeric(8), start = c(2001, 1), freq = 4)
start(x)           ## --> ti object representing 2001Q1
start(as.ts(x))    ## --> c(2001, 1)
}
\keyword{ ts }
