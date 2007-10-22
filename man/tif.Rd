\name{tif}
\alias{tif}
\alias{tif.default}
\alias{tif.ti}
\alias{tif.tis}
\alias{tif.ts}
\alias{tifName}
\alias{tifName.default}
\alias{tifName.ti}
\alias{tifName.tis}
\alias{period}
\title{ Time Index Frequencies and Periods}
\description{
  Return the tif code of an object, the name associated with a tif code,
  or the period number of a time index.
}
\usage{
tif(x, \dots)
\method{tif}{ti}(x, \dots)
\method{tif}{tis}(x, \dots)
\method{tif}{ts}(x, \dots)
\method{tif}{default}(x, freq = NULL, \dots)
tifName(s)
\method{tifName}{default}(s)
\method{tifName}{ti}(s)
\method{tifName}{tis}(s)
period(z)
}
\arguments{
  \item{x}{
	a \code{ti} or \code{tis} object, or a string giving a tif name.
  }
  \item{freq}{
	numeric. If \code{x} is missing, return the \code{tif} for this
	frequency, otherwise ignore.
  }
  \item{\dots}{ignored}
  \item{s}{
	a \code{ti} or \code{tis} object, or a tif code.
  }
  \item{z}{
	a \code{ti} object.
  }
}
\details{
  The \code{tifList} object associates tifNames with tif codes.  Most
  functions that call for \code{tif} argument can take either a tif code
  or a tif name.

  Both function are generic function with methods for \code{ti} and
  \code{tis} objects, as well as a default method.  \code{tif} also has
  a method for \code{ts} objects.
}
\value{
  \code{tif} returns the tif code for \code{x}, while \code{tifName}
  returns a name for that code.  Many of the codes have several names,
  but only the default one is returned.

  \code{tif} or \code{tifName} called with no arguments returns a vector
  of all tif codes with names.

  \code{period} returns a vector like \code{z} giving the number of
  periods elapsed since the first period defined for its argument's
  frequency.  
}
\author{ Jeff Hallman }
\seealso{ \code{\link{ti}}, \code{\link{frequency}}}
\examples{
tif()                 ## returns a vector of all tif codes
tifName(today())      ## today() returns a ti
period(today())
}
\keyword{ chron }
