\name{tifToFameName}
\alias{tifToFameName}
\title{FAME Names for Time Index Frequencies }
\description{Returns the FAME names for the given time index frequencies.}
\usage{
tifToFameName(tif)
}
\arguments{
  \item{tif}{character vector of \code{tifName}s or a numeric vector of
  \code{tif} codes.}
}
\value{A character vector as long as the input giving the FAME names of
  the input.}
\author{ Jeff Hallman }
\seealso{ \code{\link{tif}}, \code{\link{tifName}}}
\examples{
tifToFameName(tif(today()))
tifToFameName(tif(latestMonth()))
tifToFameName(tifName(today()))
tifToFameName(tifName(latestMonth()))
}
\keyword{ chron }
