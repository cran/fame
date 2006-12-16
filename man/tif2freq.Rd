\name{tif2freq}
\alias{tif2freq}
\title{Periods Per Year for Time Index Frequencies}
\usage{
tif2freq(tif)
}
\arguments{
  \item{tif}{a \code{tifName} or \code{tif} code }
}
\description{
  Returns the frequency of a \code{ti} object constructed from the
  current date with the given \code{tif}.
}
\value{a number}
\author{ Jeff Hallman }
\seealso{\code{\link{tif}}, \code{\link{tifName}}, \code{\link{frequency}}}
\examples{
tif2freq("wmonday")
tif2freq("monthly")
tif2freq(tif(today()))
}
\keyword{ chron }
