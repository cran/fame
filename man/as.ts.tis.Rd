\name{as.ts.tis}
\alias{as.ts.tis}
\title{Convert a Time Indexed Series to a Time Series }
\description{Constructs a \code{ts} object from a \code{tis}
  object. The \code{tis} object's starting \code{year}, starting
  \code{cycle}, and \code{frequency}, along with the object's data, in a
  call to the \code{ts} function.
}
\usage{
\method{as.ts}{tis}(x, ...)
}
\arguments{
  \item{x}{a \code{tis} object to be converted}
  \item{\dots}{Ignored}
}
\details{The \code{tis} class covers more frequencies than the \code{ts}
  class does, so the conversion may not be accurate.
}
\value{
  A \code{ts} object with the same data as \code{x}, and with
  starting time and frequency given by:
  \preformatted{
	start = c(year(xstart), cycle(xstart))
	frequency = frequency(x)
}}
\author{ Jeff Hallman }
\note{The \code{tis} class covers more frequencies than the \code{ts}
  class does, so the conversion may not be accurate.
}
\seealso{ \code{\link{as.ts}}}
\keyword{ ts }
