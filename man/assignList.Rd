\name{assignList}
\alias{assignList}
\title{Assign Values In a List to Names}
\description{
  Assigns the values in a list to variables in an environment.  The
  variable names are taken from the \code{names} of the list, so all of
  the elements of the list must have non-blank names.
}
\usage{
assignList(aList, pos = -1, envir = as.environment(pos), inherits = FALSE)
}
\arguments{
  \item{aList}{a list of values to be assigned to variables with names
	given by \code{names(aList)}.}
  \item{pos}{where to do the assignment.  By default, assigns into the
    current environment.}
  \item{envir}{the \code{\link{environment}} to use.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
}
\details{
  See \code{\link{assign}} for details on how \R assignment works.  This
  function simply uses the elements of \code{names(aList)} and
  \code{aList} itself to call the \code{.Internal} function used by
  \code{\link{assign}} once for each element of \code{aList}.
}
\value{
  This function is invoked for its side effect, which assigns values to
  the variables with names given by \code{names(aList)}.
}
\seealso{\code{\link{assign}}
}
\examples{
myList <- list(a = 1, b = 2, c = 3)
assignList(myList) ## equivalent to a <- 1; b <- 2; c <- 3
}
\keyword{data}
