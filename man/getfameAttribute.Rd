\name{getFameAttribute}
\alias{getFameAttribute}
\alias{getFameCreated}
\alias{getFameUpdated}
\title{Get Fame Object Attributes }
\description{
  \code{getFameAttribute} reads one attribute of one object in a Fame
  database and returns it as a string.
  \code{getFameCreated} and \code{getFameUpdated} return an object's
  \code{created} and \code{updated} attributes, respectively, as
  \code{POSIXlt} dates.
}
\usage{
getFameAttribute(attribute, fname, db = NULL, path = NULL)
getFameCreated(fname, db)
getFameUpdated(fname, db)
}
\arguments{
  \item{attribute}{
	string naming the attribute to be returned}
  \item{fname}{
    name of an object in a FAME database}
  \item{db}{
    string giving the relative or full path to the Fame database to read
    or write from. If the \code{fameLocalPath()} function is defined, it
    will be called on this argument to obtain a path.
  }
  \item{path}{
    string giving the relative or full path to the Fame database to read
    or write from. If this argument is not \code{NULL}, the \code{db} argument is
    ignored and \code{fameLocalPath()} is not called.
  }
}
\value{
  \code{getFameAttribute} returns a named string.

  \code{getFameCreated} and \code{getFameUpdated} each return a
  \code{POSIXlt} object.
}
\examples{
\dontrun{
getFameUpdated("gdp.q", db = "somedb")
}
}
\keyword{ database }
