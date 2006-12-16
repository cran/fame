\name{ti}
\alias{ti}
\alias{ti.Date}
\alias{ti.default}
\alias{ti.jul}
\alias{ti.ssDate}
\alias{ti.ti}
\alias{ti.tis}
\alias{as.ti}
\alias{couldBeTi}
\alias{is.ti}
\title{ Time Index Objects }
\description{
  The function \code{ti} is used to create time index objects, which are
  useful for date calculations and as indexes for \code{tis} (time
  indexed series).

  \code{as.ti} and \code{is.ti} coerce an object to a time index and
  test whether an object is a time index.

  \code{couldBeTi} tests whether or not \code{x} is numeric and has all
  elements within the range expected for a \code{ti} time index with the
  given \code{tif}. If \code{tif} is \code{NULL} (the default), the test
  is whether or not \code{x} could be a \code{ti} of \emph{any}
  frequency. If so, it can be safely coerced to class \code{ti} by
  \code{as.ti}. 
}
\usage{
ti(x, \dots)
ti.Date(x, \dots)
ti.default(x, tif = NULL, freq = NULL, \dots)
ti.jul(x, tif = NULL, freq = NULL, hour = 0, minute = 0, second = 0, \dots)
ti.ssDate(x, \dots)
ti.ti(x, tif = NULL, freq = NULL, \dots)
ti.tis(x, \dots)
as.ti(x)
is.ti(x)
couldBeTi(x, tif = NULL)
}
\arguments{
  \item{x}{
	object to be tested (\code{is.ti}) or converted into a \code{ti}
	object.  As described in the details below, the constructor function
	\code{ti} can deal with	several different kinds of \code{x}. 
  }
  \item{hour}{used if and only if code{tif} is an intraday frequency }
  \item{minute}{used if and only if code{tif} is an intraday frequency }
  \item{second}{used if and only if code{tif} is an intraday frequency }
  \item{\dots}{
	other args to be passed to the method called by the generic function.
  }
  \item{tif}{
	a ti Frequency, given as either a numerical code or a string.
	\code{tif()} with no arguments returns a list of the allowable
	numerical codes and names.  Either \code{tif} or
	\code{freq} must be supplied for the variants of \code{ti()}.
  }
  \item{freq}{
	some \code{tif}'s can alternatively be specified by their frequency,
	such as 1 (annual), 2 (semiannual), 4 (quarterly), 6 (bimonthly),
	12 (monthly), 24 (semimonthly), 26 (biweekly), 36 (tenday), 52
	(weekly), 262 (business) and 365 (daily).  Either \code{tif} or
	\code{freq} must be supplied for the variants of \code{ti()}.
  }
}
\details{
  A \code{ti} has a \code{tif} (ti Frequency) and a period.  The period
  represents the number of periods elapsed since the base period for that
  frequency.  Adding or subtracting an integer to a \code{ti} gives
  another \code{ti}.  Provided their corresponding element have matching
  \code{tif}s, the comparison operators \code{<, >, <=, >=, ==} all
  work, and subtracting one \code{ti} from another gives the number of
  periods between them.  See the examples section below.

  The \code{ti} class implements methods for a number of generic
  functions, including \code{"["}, \code{as.Date}, \code{as.POSIXct},
  \code{as.POSIXlt}, \code{c}, \code{cycle}, \code{edit}, \code{format},
  \code{frequency}, \code{jul}, \code{max}, \code{min}, \code{print},
  \code{rep}, \code{seq}, \code{tif}, \code{tifName}, \code{time},
  \code{ymd}.

  \code{ti} is a generic function with specialized methods to handle
  \code{jul}, \code{Date}, \code{ti} and \code{tis} objects.   

  The default method (\code{ti.default}) deals with character \code{x} by
  calling \code{as.Date} on it.  Otherwise, it proceeds as follows:

  If \code{x} is numeric, a check is made to see if \code{x} could be
  a \code{ti} object that has somehow lost it's class attribute. Failing that,
  \code{isYmd} is used to see if it could be yyyymmdd date, then 
  \code{isTime} is called to see if \code{x} could be a decimal time (a
  number between 1799 and 2200). If \code{x} is of length 2, an attempt
  to interpret it as a \code{c(year, period)} pair  is made.  Finally,
  if all else fails, \code{as.Date(x)} is called to attempt to create a
  \code{Date} object that can then be used to construct a \code{ti}. 
}
\value{
  \code{is.ti} and \code{couldBeTi} return \code{TRUE} or \code{FALSE}.
  
  \code{as.ti} coerces its argument to have class \code{ti}, without
  making any attempt to discern whether or not this is a sensible thing
  to do.  \code{as.ti} should only be called on an object if
  \code{couldBeTi} on that object answers \code{TRUE}.
  
  \code{ti} constructs a \code{ti} object like \code{x}, except for two
  special cases:

  1.  If \code{x} is a \code{tis} series, the return value is a vector
      time index with elements corresponding to the observation periods of
      \code{x}. 

  2.  If \code{x} is a numeric object of length 2 interpretable as
      \code{c(year, period)}, the return value is a single \code{ti}. 
}
\note{
  The \code{as.Date(x)} call is not wrapped in a try-block, so it may be at
  the top of the stack when \code{ti} fails. 
}
\author{ Jeff Hallman }
\seealso{ \code{\link{jul}}, \code{\link{ymd}}, \code{\link{tif}},
  \code{\link{tifName}}, \code{\link{as.Date}}} 
\examples{
z <- ti(19971231, "monthly")   ##  monthly ti for Dec 97
is.ti(z)                       ##  TRUE
is.ti(unclass(z))              ##  FALSE
couldBeTi(unclass(z))          ##  TRUE
ymd(z + 4)                     ##  19980430
z - ti(c(1997,6), freq = 12)   ##  monthly ti for June 1997
ti(z, tif = "wmonday")         ##  week ending Monday June 30, 1997
}
\keyword{ chron }
\keyword{ ts }
