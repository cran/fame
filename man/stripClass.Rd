\name{stripClass}
\alias{stripClass}
\alias{stripTis}
\title{Remove part of a class attribute }
\usage{
stripClass(x, classString)
stripTis(x)
}
\arguments{
  \item{x}{an object whose \code{class} character vector may or may not
	include \code{classString}}
  \item{classString}{name of class to remove from the inheritance chain }
}
\description{An R object may have a class attribute that is a character
  vector giving the names of classes it inherits from. \code{stripClass}
  strips the class \code{classString} from that character vector.
  \code{stripTis(x)} is shorthand for \code{stripClass(x, "tis")}.
}
\value{An object like \code{x}, but whose \code{class} attribute does
  not include \code{classString}.  If the \code{class} attribute less
  \code{classString} is empty, \code{unclass(x)} is returned.
}
\author{ Jeff Hallman }
\note{This function can be useful in functions that return a modified
  version of one their arguments.  For example, the \code{format.ti} method
  takes a \code{ti} (TimeIndex) as an argument and returns a character object
  object 'like' the original argument.  The first thing
  \code{format.ti(x)} does internally is \code{z <- stripClass(x, "ti")}. 
  This creates \code{z} as a copy of \code{x} but with the difference
  that \code{z} no longer inherits from class \code{ti}.  The function
  then fills in the data elements of \code{z} with the approriate
  strings and returns it.  The beauty of this approach is that the
  returned \code{z} already has all of the attributes \code{x} had,
  except that it no longer inherits from class \code{ti}.  In
  particular, if \code{x} was a matrix with dimnames, etc., \code{z}
  will also have those attributes.
}
\seealso{ \code{\link{class}}}
\keyword{classes}
