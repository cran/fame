\name{currentMonday}
\alias{currentMonday}
\alias{currentTuesday}
\alias{currentWednesday}
\alias{currentThursday}
\alias{currentFriday}
\alias{currentSaturday}
\alias{currentSunday}
\alias{latestMonday}
\alias{latestTuesday}
\alias{latestWednesday}
\alias{latestThursday}
\alias{latestFriday}
\alias{latestSaturday}
\alias{latestSunday}
\title{Day of Week Time Indexes }
\description{Return daily \code{ti}'s for particular days of the week}
\usage{
currentMonday(xTi = today())
currentTuesday(xTi = today())
currentWednesday(xTi = today())
currentThursday(xTi = today())
currentFriday(xTi = today())
currentSaturday(xTi = today())
currentSunday(xTi = today())
latestMonday(xTi = today())
latestTuesday(xTi = today())
latestWednesday(xTi = today())
latestThursday(xTi = today())
latestFriday(xTi = today())
latestSaturday(xTi = today())
latestSunday(xTi = today())
}
\arguments{
  \item{xTi}{a \code{ti} object or something that the \code{ti()}
  function can turn into a \code{ti} object }
}
\value{
  \code{currentMonday} returns the daily \code{ti} for the last day of
  the Monday-ending week that its argument falls into.
  \code{currentTuesday} returns the daily \code{ti} for the last day of
  the Tuesday-ending week that its argument falls into, and so on for
  the other weekdays.

  \code{latestMonday} returns the daily \code{ti} for the last day of
  the most recent completed Monday-ending week that its argument falls
  into.  Ditto for the other days of the week.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{ti}} }
\keyword{ chron }
\keyword{ ts }
