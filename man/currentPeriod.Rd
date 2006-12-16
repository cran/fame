\name{currentPeriod}
\alias{currentWeek}
\alias{currentMonth}
\alias{currentQuarter}
\alias{currentHalf}
\alias{currentYear}
\alias{currentQ4}
\alias{currentQMonth}
\alias{currentJanuary}
\alias{currentFebruary}
\alias{currentMarch}
\alias{currentApril}
\alias{currentMay}
\alias{currentJune}
\alias{currentJuly}
\alias{currentAugust}
\alias{currentSeptember}
\alias{currentOctober}
\alias{currentNovember}
\alias{currentDecember}
\title{ Current Period Time Indexes }
\description{Return a current \code{ti} of the desired frequency}
\usage{
currentWeek(xTi = today())
currentMonth(xTi = today())
currentQuarter(xTi = today())
currentHalf(xTi = today())
currentYear(xTi = today())
currentQ4(xTi = today())
currentQMonth(xTi = today())
currentJanuary(xTi = today())
currentFebruary(xTi = today())
currentMarch(xTi = today())
currentApril(xTi = today())
currentMay(xTi = today())
currentJune(xTi = today())
currentJuly(xTi = today())
currentAugust(xTi = today())
currentSeptember(xTi = today())
currentOctober(xTi = today())
currentNovember(xTi = today())
currentDecember(xTi = today())
}
\arguments{
  \item{xTi}{a \code{ti} object or something that the \code{ti()}
  function can turn into a \code{ti} object }
}
\details{
  \code{currentWeek} returns the weekly \code{ti} for the week that its
  argument falls into.  If the argument is itself a \code{ti}, the
  returned week contains the last day of the argument's period. The default
  weekly frequency is "wmonday" (Monday-ending weeks), so
  \code{currentWeek} always returns wmonday \code{ti}'s.  This can be
  changed via the \code{setDefaultFrequencies} function. 

  All of the other \code{current{SomeFreq}} functions work the same way,
  returning the \code{ti}'s of \code{tif} SomeFreq that the last day of
  their arguments period falls into.  The \code{tif}'s for
  \code{currentHalf} and \code{currentQ4} are "semiannual" and
  "quarterly", respectively.  Finally, \code{currentQMonth} returns the
  quarter-ending month of the \code{currentQuarter} of its argument.

  \code{currentJanuary} returns the monthly \code{ti} for January of the
  January-ending year that the last day of its argument falls into.
  \code{currentFebruary} returns the monthly \code{ti} for February of
  the February-ending year that the last day of its argument falls into,
  and so on.
}

\value{ All return return \code{ti} objects as described in the details.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{ti}}, \code{\link{tif}},
  \code{\link{latestWeek}} \code{\link{setDefaultFrequencies}}}
\keyword{ chron }
\keyword{ ts }
