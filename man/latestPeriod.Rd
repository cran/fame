\name{latestPeriod}
\alias{latestWeek}
\alias{latestMonth}
\alias{latestQuarter}
\alias{latestHalf}
\alias{latestYear}
\alias{latestQ4}
\alias{latestJanuary}
\alias{latestFebruary}
\alias{latestMarch}
\alias{latestApril}
\alias{latestMay}
\alias{latestJune}
\alias{latestJuly}
\alias{latestAugust}
\alias{latestSeptember}
\alias{latestOctober}
\alias{latestNovember}
\alias{latestDecember}
\title{ Most Recent Period Time Indexes }
\description{Return a \code{ti} for the most recent period of the
  desired frequency.}
\usage{
latestWeek(xTi = today())
latestMonth(xTi = today())
latestQuarter(xTi = today())
latestHalf(xTi = today())
latestYear(xTi = today())
latestQ4(xTi = today())
latestJanuary(xTi = today())
latestFebruary(xTi = today())
latestMarch(xTi = today())
latestApril(xTi = today())
latestMay(xTi = today())
latestJune(xTi = today())
latestJuly(xTi = today())
latestAugust(xTi = today())
latestSeptember(xTi = today())
latestOctober(xTi = today())
latestNovember(xTi = today())
latestDecember(xTi = today())
}
\arguments{
  \item{xTi}{a \code{ti} object or something that the \code{ti()}
  function can turn into a \code{ti} object }
}
\details{
  The \code{latest{whatever}} functions are the same as the
  corresponding \code{current{whatever}} functions, except that they
  return the most recent completed \code{ti} of the desired frequency.
  A period is considered to be completed on it last day.  For example,
  if today is Thursday, then \code{latestWedweek()} returns the week
  that ended yesterday.  Yesterday it would have returned the same week,
  but the day before that (Tuesday) it would have returned the
  "wwednesday" \code{ti} for the week that had ended six days before.

  \code{latestWeek} returns the weekly \code{ti} for the most recently
  completed week as of \code{xTi}.  If the \code{xTi} is itself a
  \code{ti}, the returned week is the most recently completed week as of
  the last day of \code{xTi}.  (Note that the default weekly frequency
  is "wmonday" (Monday-ending weeks), so \code{latestWeek} always
  returns "wmonday" \code{ti}'s.)  See
  \code{\link{setDefaultFrequencies}} to change this.

  All of the other \code{latest{SomeFreq}} functions work the same way,
  returning the \code{ti}'s for the most recently completed SomeFreq as
  of the last day of \code{xTi}. The \code{tif}'s (frequencies) for
  \code{latestHalf} and \code{latestQ4} are "semiannual" and
  "quarterly", respectively. 

  \code{latestJanuary} returns the monthly \code{ti} for January of the
  most recently completed January-ending year that the last day of its
  argument falls into.  \code{latestFebruary} returns the monthly
  \code{ti} for February of the most recently completed February-ending
  year that the last day of its argument falls into, and so on. 
}

\value{ All return return \code{ti} objects as described in the details.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{ti}}, \code{\link{tif}}, \code{\link{currentWeek}}
  \code{\link{setDefaultFrequencies}}}
\keyword{ chron }
\keyword{ ts }
