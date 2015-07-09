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
\seealso{ \code{\link[tis:tif]{tif}}, \code{\link[tis:tif]{tifName}}}
\examples{
tifToFameName(tis::tif(tis::today()))
tifToFameName(tis::tif(tis::latestMonth()))
tifToFameName(tis::tifName(tis::today()))
tifToFameName(tis::tifName(tis::latestMonth()))
}
\keyword{ chron }
