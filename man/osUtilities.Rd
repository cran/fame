\name{osUtilities}
\alias{user}
\alias{groups}
\alias{pid}
\alias{pgid}
\alias{ppid}
\alias{killProcess}
\alias{pwd}
\alias{runningLinux}
\alias{runningWindows}
\title{Operating System Utilities }
\description{
  \code{user} returns the user id of the current user.

  \code{groups} returns a character vector of the Linux security groups
  \code{user.} is a member of.

  \code{pid}, \code{pgid} and \code{ppid} return the process, group and
  parent process ID numbers, respectively, of the current \R process.

  \code{killProcess} kills a specified process.

  \code{pwd} returns the process working directory.

  \code{runningLinux} is shorthand for \code{Sys.info()["sysname"] == "Linux"}.

  \code{runningWindows} is shorthand for \code{.Platform$OS.type == "windows"}.

}
\usage{
user()
groups(user. = user())
pid()
pgid()
ppid()
killProcess(pid)
pwd()
runningLinux()
runningWindows()
}
\arguments{
  \item{user.}{a user id}
  \item{pid}{a process id number}
}
\author{ Jeff Hallman }
\keyword{utilities}
