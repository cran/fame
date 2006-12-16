\name{as.Date.jul}
\alias{as.Date.jul}
\alias{as.Date.ti}
\title{Convert ti or jul objects to Dates}
\description{
  Methods to convert \code{ti} and \code{jul} objects to class
  \code{"Date"} representing calendar dates. 
}
\usage{
\method{as.Date}{ti}(x, \dots)
\method{as.Date}{jul}(x, \dots)
}
\arguments{
  \item{x}{A \code{ti} or \code{jul} object to be converted.}
  \item{\dots}{Ignored.}
}
\value{An object of class \code{"\link{Date}"}.
}
\seealso{
  \code{\link{as.Date}} for the generic function, \link{Date} for
  details of the date class.
}
\examples{
as.Date(today())          ## invokes as.Date.ti
as.Date(jul(today() - 7)) ## a week ago, uses as.Date.jul
}  
\keyword{chron}
