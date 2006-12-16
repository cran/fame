\name{description}
\alias{description}
\alias{documentation}
\alias{description<-}
\alias{documentation<-}
\title{Description and Documentation Attributes}
\description{Get or set the \code{description} and \code{documentation}
  strings for an object.
}
\usage{
description(x)
description(x) <- value
documentation(x)
documentation(x) <- value
}
\arguments{
  \item{x}{object whose description or documentation attribute is to be
  set or retrieved}
  \item{value}{a string}
}
\value{
  The setters invisibly return \code{x}, the getters return the desired
  attribute or NULL.
}
\author{ Jeff Hallman }
\keyword{ attribute }
