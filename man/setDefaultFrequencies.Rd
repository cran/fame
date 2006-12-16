\name{setDefaultFrequencies}
\alias{setDefaultFrequencies}
\alias{tifList}
\title{Return known Time Index Frequencies, Change Default Frequencies}
\description{A \code{tif} (Time Index Frequency) can usually be set
  either by code (a number) or by name.  \code{setDefaultFrequencies}
  sets particular frequencies for the \code{tif} names "weekly",
  "biweekly", "bimonthly" (also "bimonth"), "quarterly" (also "q"),
  "annual" (also "a"), and "semiannual" (also "sann").

  \code{tifList} returns the map of frequency names to frequency codes.
}
\usage{
setDefaultFrequencies(weekly     = "wmonday",
                      biweekly   = "bw2wednesday",
                      bimonthly  = "bimonthdecember",
                      quarterly  = "qdecember",
                      annual     = "anndecember",
                      semiannual = "sanndecember",
                      setup = FALSE)
tifList()
}
\arguments{
  \item{weekly}{A string giving the name of the particular frequency that frequency "weekly" will correspond to}
  \item{biweekly}{Ditto for "biweekly"}
  \item{bimonthly}{Ditto for "bimonth" and "bimonthly"}
  \item{quarterly}{Ditto for "q" and "quarterly"}
  \item{annual}{Ditto for "a" and "annual"}
  \item{semiannual}{Ditto for "sann" and "semiannual"}
  \item{setup}{If \code{TRUE}, set all of the defaults, otherwise set
  only the defaults for which arguments were given.  The default is
  \code{FALSE}, but see the details}
}
\details{The named vector \code{.tifList} (returned by the function of
  the same name) stored in the global enviroment contains the mapping of
  frequency names to frequency codes.  Running this function modifies
  the \code{tifList} vector and stores it back in the global environment.
  It gets run with \code{setup = TRUE} when the \code{fame} package is loaded.
  If you want different defaults, call the function sometime after that.
}
\value{A copy of the \code{.tifList} vector.}
\author{ Jeff Hallman }
\seealso{\code{\link{tifName}}}
\keyword{ chron }
