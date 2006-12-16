\name{dayOfPeriod}
\alias{dayOfPeriod}
\alias{dayOfWeek}
\alias{dayOfMonth}
\alias{dayOfYear}
\alias{firstDayOf}
\alias{lastDayOf}
\alias{firstBusinessDayOf}
\alias{lastBusinessDayOf}
\alias{firstBusinessDayOfMonth}
\alias{lastBusinessDayOfMonth}
\alias{currentMonthDay}
\alias{latestMonthDay}
\title{Day positions in Time Index Periods}
\description{Return position within a \code{ti} period, or a particular
  day within the period.}
\usage{
dayOfPeriod(xTi = today(), tif = NULL)
dayOfWeek(xTi = today())
dayOfMonth(xTi = today())
dayOfYear(xTi = today())
firstDayOf(xTi)
lastDayOf(xTi)
firstBusinessDayOf(xTi)
lastBusinessDayOf(xTi)
firstBusinessDayOfMonth(xTi)
lastBusinessDayOfMonth(xTi)
currentMonthDay(xTi, daynum)
latestMonthDay(xTi, daynum)
}
\arguments{
  \item{xTi}{a \code{ti} object or something that the \code{ti()}
  function can turn into a \code{ti} object }
  \item{tif}{a time index frequency code or name. See \code{\link{tif}}. }
  \item{daynum}{day number in month}
}
\details{
  The \code{dayOfXXXXX} functions all work the same way, returning the
  day number of the XXXXX that \code{jul(xTi)} falls on.  For example,
  if today is Thursday, January 5, 2006, then \code{dayOfWeek()},
  \code{dayOfMonth()} and \code{dayOfYear()} are all 5.  All of these
  are implemented via \code{dayOfPeriod}, which converts its first
  argument to a Julian date (via \code{jul(xTi)}) and finds the
  \code{ti} with frequency \code{tif} that day falls into. It returns
  the day number of the period represented by that time index that the
  Julian date falls on.  

  \code{firstDayOf} and \code{lastDayOf} return a daily \code{ti} for
  the first or last day of the period represented by \code{xTi}.
  \code{firstBusinessDayOf} and \code{lastBusinessDayOf} do the same but
  the returned \code{ti} has business daily frequency.

  \code{firstBusinessDayOfMonth} returns a business daily \code{ti} for
  the first business day of the month of \code{xTi}.
  \code{lastBusinessDayOfMonth} does the same but for the last business
  day of the month of \code{xTi}.

  \code{currentMonthDay} returns a daily \code{ti} for the next upcoming
  \code{daynum}'th of the month.  \code{latestMonthDay} does the same
  for the most recent \code{daynum}'th of the month.

  \code{currentMonday} returns the daily \code{ti} for the last day of
  the Monday-ending week that its argument falls into.  The other
  \code{current{Weekday}} functions work the same way.

}
\value{ All of the functions except the \code{dayOfXXXXX} return \code{ti}
  objects as described in the details section above.  The
  \code{dayOfXXXXX} functions return numbers.
}
\note{None of these business-day functions take account of holidays, so
  \code{firstBusinessDayOfMonth(20010101)}, for example, returns January
  1, 2001 which was actually a holiday.  To see how to handle holidays,
  look at the \code{\link{holidays}} and \code{\link{nextBusinessDay}} help pages.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{ti}}, \code{\link{tif}}, \code{\link{jul}},
  \code{\link{holidays}}, \code{\link{nextBusinessDay}},
  \code{\link{previousBusinessDay}} }
\keyword{ chron }
\keyword{ ts }
