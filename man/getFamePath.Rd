\name{getFamePath}
\alias{getFamePath}
\title{Path to a Fame Database}
\description{Attempts to resolve its argument to a database path.}
\usage{
getFamePath(dbString, stopOnFail = TRUE)
}
\arguments{
  \item{dbString}{A database name understood by the local customizing
  function \code{fameLocalPath} (if it exists), the name of a file in
  the current directory, or the full path name of a database.}
  \item{stopOnFail}{If \code{TRUE}, stop with an error message if a
  readable database cannot be found.}
}
\details{
  If a local function called \code{fameLocalPath} exists, it is called
  first on \code{dbString} to get a pathname. Otherwise, \code{dbString}
  is presumed to be the pathname.  The pathname is checked for existence
  and readability.  If there is a white space in the pathname, it is
  presumed valid, as that might specifying a Fame Server.  Otherwise, if
  there is a failure and \code{stopOnFail} is \code{TRUE}, the function
  fails with an error message.

  \code{putfame} calls this function with \code{stopOnFail = FALSE},
  because it normally creates a new database if there isn't one there already. 
}
\value{A path to a Fame database or Fame Server specification.}
\seealso{
\code{\link{fameCustomization}}
}
\keyword{ database }
