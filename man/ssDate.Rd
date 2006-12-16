\name{ssDate}
\alias{ssDate}
\alias{as.ssDate}
\alias{is.ssDate}
\title{ ssDate Objects }
\description{
  The function \code{ssDate} is used to create \code{ssDate}
  (spreadsheet date) objects, which are useful for reading and writing
  dates in spreadsheet form, i.e., as the number of days since December
  30, 1899.

  \code{as.ssDate} and \code{is.ssDate} coerce an object to a ssDate and
  test whether an object is a \code{ssDate}. 
}
\usage{
ssDate(x, ...)
as.ssDate(x)
is.ssDate(x)
}
\arguments{
    \item{x}{
	object to be tested (\code{is.ssDate}) or converted into a \code{ssDate}
	object. 
  }
  \item{\dots}{
	other args to be passed to \code{jul} function.
  }
}
\details{
  an \code{ssDate} is essentially a rebased Julian date that represents
  a date as the number of days since December 30, 1899.  The
  constructor function \code{ssDate} subtracts \code{jul(18991230)} from
  \code{jul(x, ...)} and coerces the result to class \code{ssDate}.
  Pretty much all of the stuff you can do with \code{jul} objects can
  also be done with \code{ssDate} objects.
  
}
\value{
  \code{is.ssDate} returns \code{TRUE} or \code{FALSE}.
  
  \code{as.ssDate} coerces its argument to have class \code{ssDate}, without
  making any attempt to discern whether or not this is a sensible thing
  to do.
  
  \code{ssDate} constructs a \code{ssDate} object like \code{x}.
  
  \code{ssDate} with no arguments returns the \code{ssDate} for the current day.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{jul}}}
\examples{
dec31 <- ssDate(20041231)
jan30 <- ssDate("2005-1-30")
jan30 - dec31              ## 30
feb28 <- jan30 + 29
ssDate()                      ## current date
}
\keyword{ chron }
