\name{Intraday}
\alias{hourly}
\alias{minutely}
\alias{secondly}
\title{ R support for FAME-style Intraday frequencies }
\description{
  create \code{tif} (TimeIndexFrequency) codes for hourly, minutely, and
  secondly \code{ti}'s.  
}
\usage{
  hourly(n = 0)
minutely(n = 0)
secondly(n = 0)
}
\arguments{
  \item{n}{number of base periods to skip.  That is, \code{hourly(2)}
  gives a \code{tif} code for a series observed every 2nd hour, while
  both \code{minutely()} and \code{minutely(1)} are for a series
  observed once per minute, \code{secondly(30)} means every 30 seconds,
  and so on.}
}
\details{
  The current implementation has hourly(n) --> 2000 + n,
  minutely(n) --> 3000 + n, and secondly(n) --> 4000 + n.  If \code{n}
  divides evenly into 3600 for \code{secondly(n)}, the return code will
  be the same as \code{hourly(n/3600)}.  For \code{secondly(n)} and
  \code{minutely(n)}, if \code{n} divides evenly into 60, the return
  code will be as if \code{minutely(n/60)} or \code{hourly(n/60)} had
  been called, respectively.

  For \code{hourly(n)}, \code{n} must evenly divide into 24 and be less
  than 24, i.e., \code{n} is one of 1, 2, 3, 4, 6, 8, 12.  For
  \code{minutely(n)}, \code{n} must be an even divisor of 1440, and less
  than 720.  For \code{secondly(n)}, \code{n} must divide evenly into
  86400, and be no larger than 960.
}
\value{ An integer \code{tif} code.}
\author{ Jeff Hallman }
\seealso{ \code{\link{tif}}}
\keyword{ chron }
