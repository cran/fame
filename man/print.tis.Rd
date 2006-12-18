\name{print.tis}
\alias{print.tis}
\title{ Printing Time Indexed Series }
\description{
  Print method for time indexed series.
}
\usage{
\method{print}{tis}(x, format = "\%Y\%m\%d", matrix.format = F, ...)
}
\arguments{
  \item{x}{
	a time indexed series
  }
  \item{format}{
	a character string describing how to format the observation times if
	either \code{x} is printed in matrix form.  Format strings are
	detailed in \code{format.ti}.
 }
 \item{matrix.format}{
   TRUE or FALSE.  See details.
 }
 \item{\dots}{
   additional arguments that may be passed along to \code{print.ts}.
   See details.
 }
}
\details{
  If \code{matrix.format} is \code{F} (the default) and \code{x} is
  a univariate monthly, quarterly or annual series, printing is
  accomplished by \code{print(as.ts(x), ...)}.  Otherwise,
  \code{x} is printed as a matrix with rownames created by
  formatting the time indexes of the observations according to the
  \code{format} string.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{format.ti}}, \code{\link{print.ts}}}
\examples{
print(tis(1:31, start = today() - 30), format = "\%b \%d, \%Y")
}
\keyword{ ts }
