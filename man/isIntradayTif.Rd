\name{isIntradayTif}
\alias{isIntradayTif}
\title{Check for Intraday Time Index Frequency}
\usage{
isIntradayTif(tif)
}
\arguments{
  \item{tif}{a character vector of \code{tif} names (see
  \code{\link{tifName}}) or a numeric vector of \code{tif} codes (see
  \code{\link{tif}} to be checked} 
}
\description{The intraday frequencies are \code{hourly(n)},
  \code{minutely(n)} and \code{secondly(n)}, where \code{n} is an
  appropriate integer. Their numeric \code{tif} codes are between 2000
  and 4900, and that is what is actually checked for. 
}
\value{A logical vector as long as the input indicating which elements
  are intraday Time Index frequencies.
}
\note{The function does not attempt to verify if the supplied \code{tif}
  is actually valid, intraday or not.
}
\author{ Jeff Hallman }
\seealso{\code{\link{hourly}}, \code{\link{minutely}}, \code{\link{secondly}}}
\examples{
isIntradayTif(hourly(6))
isIntradayTif(tif(today()))
isIntradayTif(minutely(30))
}
\keyword{ chron }
