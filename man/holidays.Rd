\name{holidays}
\alias{nextBusinessDay}
\alias{previousBusinessDay}
\alias{isHoliday}
\alias{isGoodFriday}
\alias{isEaster}
\alias{holidays}
\alias{federalHolidays}
\alias{goodFriday}
\alias{easter}
\alias{holidaysBetween}
\title{Holidays}
\description{Functions that know about Federal and FRB (Federal Reserve
  Board) holidays.
}
\usage{
nextBusinessDay(x, holidays = NULL, goodFriday = F, board = F)
previousBusinessDay(x, holidays = NULL, goodFriday = F, board = F)
isHoliday(x, goodFriday = F, board = F, businessOnly = T)
isGoodFriday(x)
isEaster(x)
holidays(years, goodFriday = F, board = F, businessOnly = T)
federalHolidays(years, board = F, businessOnly = T)
goodFriday(years)
easter(years)
holidaysBetween(startTi, endTi, goodFriday = F, board = F, businessOnly = T)
}
\arguments{
  \item{x}{a \code{ti} time index, or something that can be turned into
	one, such as a yyyymmdd number or a \code{Date} object.}
  \item{holidays}{a vector of holidays (in yyyymmdd form) to skip over, or
	\code{NULL}. In the latter case, the \code{holidays} function is used
	to determine days to skip over. }
  \item{goodFriday}{if \code{TRUE}, consider Good Friday as a holiday.
	Default is \code{FALSE} because Good Friday is not a federal holiday.}
  \item{board}{if \code{TRUE}, the Friday preceding a Saturday NewYears,
	Independence, Veterans or Christmas Day is considered a holiday.}
  \item{businessOnly}{if \code{TRUE} (the default), ignore Saturday NewYears,
	Independence, Veterans amd Christmas Day holidays.  Has no effect if
	\code{board} is \code{TRUE}, since that moves Saturday holidays to Friday.}
  \item{years}{numeric vector of 4 digit years}
  \item{startTi}{a daily \code{ti} time index, or something that can be
  turned into one}
  \item{endTi}{a daily \code{ti} time index, or something that can be
  turned into one}
}
\details{
  Federal law defines 10 holidays. Four of them, NewYears, Independence,
  Veterans and Christmas, fall on the same date every year. The other
  six fall on particular days of the week and months (MLK, Presidents,
  Memorial, Labor, Columbus, and Thanksgiving).

  If one of the four fixed-date holidays falls on a Sunday, the federal
  holiday is celebrated the next day (Monday).  If it falls on a
  Saturday, the preceding day (Friday) is a holiday for the Federal
  Reserve Board, but not for the Reserve Banks and the banking system as
  a whole. 
}
\value{
  \code{nextBusinessDay} and \code{previousBusinessDay} return
  "business" frequency \code{ti} objects.

  \code{isHoliday}, \code{isGoodFriday} and \code{isEaster} return
  Boolean vectors as long as \code{x}.

  \code{easter} and \code{goodFriday} return numeric vectors of yyyymmdd
  dates of the appropiate holidays for each year in the \code{years}
  argument.

  \code{federalHolidays} returns a numeric vector of yyyymmdd dates
  for the federal holidays for each year in \code{years}.  The
  \code{names} attribute of the returned vector contains the holiday
  names.

  \code{holidays} returns a vector like \code{federalHolidays} does.
  The only difference between the two functions is that \code{holidays}
  has the option of including Good Fridays.

  \code{holidaysBetween} returns a vector of yyyymmdd dates for holidays
  that fall within the time spanned by \code{[startTi, endTi]}.
}
\author{ Jeff Hallman }
\note{ The algorithm for finding Easter dates was found somewhere on the
  web (I don't remember where) and is unbelievably complex.  It would
  probably be simpler to just celebrate the home opener of the Cleveland
  Indians instead.
}
\keyword{ chron }
