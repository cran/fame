\name{cbind.tis}
\alias{cbind.tis}
\title{ Combine Series Into a Multivariate (Matrix) Time Indexed Series }
\description{
  This is \code{cbind} for \code{tis} objects.  It binds several
  \code{ts} and \code{tis} objects together into a single matrix time
  indexed series.  
}
\usage{
\method{cbind}{tis}(\dots, union = F)
}
\arguments{
  \item{\dots}{
	any number of univariate or multivariate \code{ts} or \code{tis}
	objects.  All will be converted to \code{tis} objects by
	\code{as.tis}, and the result series all must have the same
	\code{tif} (time index frequency). 
  }
  \item{union}{
	a logical.  If \code{union} = F, a matrix created by the
	intersection of the time windows for the arguments will be created.
	If \code{union} = T, the union of the time windows will be used to
	create the matrix. 
  }
}
\details{
  If \code{union} is \code{TRUE} and the series in \dots do not all
  start and end on the same time index, the missing observations are
  filled with \code{NA}.

  The column names of the returned series are determined as follows:

  If an argument was given a name in the call, the corresponding column
  has that name.  If the argument was itself a matrix with column names,
  those will be used, otherwise the argument's name is expanded with
  digits denoting its respective columns.
}
\value{
  a multivariate \code{tis} object.
}
\note{
  Class \code{"ts"} has it's own \code{cbind} method which knows nothing
  about \code{tis} objects.  R generic functions like \code{cbind}
  dispatch on the class of their first argument, so if you want to
  combine \code{tis} and \code{ts} objects by calling the generic
  \code{cbind}, be sure that the first argument is a \code{tis}, not a
  \code{ts}.  You can always ensure this is the case by wrapping the
  first argument in \dots in \code{as.tis()}.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{cbind}}}
\examples{
}
\keyword{ ts }
