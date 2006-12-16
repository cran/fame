\name{ymd}
\alias{ymd}
\alias{ymd.jul}
\alias{ymd.ssDate}
\alias{ymd.ti}
\alias{ymd.default}
\alias{year}
\alias{quarter}
\alias{month}
\alias{day}
\title{ Extract parts of various Date-Time Objects}
\description{
  Extract the year, month or day, or all three (in yyyymmdd
  form), or the quarter, from a jul, ti, or from any object that
  \code{jul()} can handle.
}
\usage{
ymd(x, \dots)
\method{ymd}{jul}(x, \dots)
\method{ymd}{ssDate}(x, \dots)
\method{ymd}{ti}(x, offset = 1, \dots)
\method{ymd}{default}(x, \dots)
year(x, \dots)
quarter(x, \dots)
month(x, \dots)
day(x, \dots)
}
\arguments{
  \item{x}{
	a \code{ti} or \code{jul}, or something that \code{jul()} can create
	a \code{jul} object from. 
  }
  \item{\dots}{
	other args to be passed to the method called by the generic
	function. \code{year}, \code{quarter}, \code{month}, \code{day} and
	\code{ymd.default} may pass these args to \code{as.Date}. 
  }
  \item{offset}{
	for \code{ti} \code{x}, a number in the range [0,1] telling where in the
	period represented by \code{x} to find the day.  0 returns the first
	day of the period, while the default value 1 returns the last day of
	the period.  For example, if \code{x} has \code{tif} = "wmonday" so
	that \code{x} represents a week ending on Monday, than any
	\code{offset} in the range [0, 1/7] will return the Tuesday of that
	week, while \code{offset} in the range (1/7, 2/7] will return the
	Wednesday of that week, \code{offset} in the range (6/7, 1] will
	return the Monday that ends the week, and so on.  
  }
}
\details{
  \code{year}, \code{quarter}, \code{month} and \code{day} call
  \code{ymd}, and thus understand the same arguments as it does. The
  default implementation \code{ymd.default} passes it's arguments to a
  call to the function \code{jul}, so all of these functions work the
  same way that function does. 
}
\value{
  \code{ymd} and it's variants return numeric objects in yyyymmdd form.

  \code{year}, \code{quarter}, \code{month} and \code{day} return
  numeric objects.

  \code{ymd()} with no arguments returns today's yyyymmdd.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{jul}}, \code{\link{ti}}, \code{\link{as.Date}}}
\examples{
ymd()                            ## today's date and time
weekFromNow <- ymd(today() + 7)  ## today() returns a daily ti
year(jul(today()))
month(Sys.time())
## create a monthly tis (Time Indexed Series)
aTis <- tis(0, start = c(2000, 1), end = c(2004, 12), freq = 12)
ymd(ti(aTis))                    ## the yyyymmdd dates of the observations
}
\keyword{ chron }
