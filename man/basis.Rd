\name{basis}
\alias{basis}
\alias{basis<-}
\alias{observed}
\alias{observed<-}
\title{FAME time series attributes }
\description{FAME time series have (sometimes implicit) \code{basis} and
  \code{observed} attributes.  
}
\usage{
basis(x)
basis(x) <- value
observed(x)
observed(x) <- value
}
\arguments{
  \item{x}{a \code{tis} series}
  \item{value}{a character string, see the details}
}
\details{
  A series \code{basis} is "business" or "daily",
  indicating whether the data values in a series are associated with a
  5-day business week or a 7-day calendar week.

  The \code{observed} attribute of series is one of the following:
  \tabular{rl}{
	\code{annualized} \tab Specifies that each time series value is the
	annualized sum of observations made throughout the associated time
	interval. For time scale conversion and totaling purposes, this
	attribute is the same as averaged. \cr
	\code{averaged} \tab Specifies that each time series value is the
	average of the observations made throughout the associated time
	interval. \cr
    \code{beginning} \tab Specifies that each time series value
    represents a single observation made at the beginning of the
    associated time interval. \cr
    \code{end} \tab Specifies that each time series value represents a
    single observation made at the end of the associated time interval. \cr
	\code{formula} \tab Specifies that the time series represents a
	transformation of other series. For time scale conversion and
	totaling purposes, this attribute is the same as averaged. \cr
	\code{high} \tab Specifies that each time series value is the
	maximum value for the time interval. \cr
	\code{low} \tab Specifies that each time series value is the minimum
	value for the time interval. \cr
	\code{summed} \tab Specifies that each time series value is the sum
	of observations made throughout the associated time interval. \cr
  }
}
\value{\code{basis} and \code{observed} return a character string.  The
  assignment forms invisibly return \code{x}.
}
\references{ The FAME documentation.}
\author{Jeff Hallman}
\seealso{\code{\link{getfame}}, \code{\link{putfame}}}
\keyword{ts}
