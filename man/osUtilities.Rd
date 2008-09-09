\name{osUtilities}
\alias{user}
\alias{runningLinux}
\alias{runningWindows}
\title{Operating System Utilities }
\description{
  \code{user} returns the user id of the current user.

  \code{runningLinux} is shorthand for \code{Sys.info()["sysname"] == "Linux"}.

  \code{runningWindows} is shorthand for \code{.Platform$OS.type == "windows"}.

}
\usage{
user()
runningLinux()
runningWindows()
}
\arguments{
}
\author{ Jeff Hallman }
\keyword{utilities}
