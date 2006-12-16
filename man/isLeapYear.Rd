\name{isLeapYear}
\alias{isLeapYear}
\title{Check Leap Year}
\description{Checks whether or not the elements of its input are leap years.
}
\usage{
isLeapYear(y)
}
\arguments{
  \item{y}{ numeric vector of years }
}
\value{
  logical vector of same length as \code{y} indicating whether or not
  the given years are leap years.
}
\details{\code{y} is a leap year if it is evenly divisible by 4
  \emph{and} either it is not evenly divisible by 100 or it is evenly
  divisible by 400, i.e.,
  \code{y\%\%4 == 0 & (y\%\%100 != 0 | y\%\%400 == 0)}.
}
\author{ Jeff Hallman }
\examples{
isLeapYear(c(1899:2004))
}
\keyword{ chron }
