\name{pad.string}
\alias{pad.string}
\title{Pad strings to a particular length}
\description{Pads a collection of strings to a desired length to either
  the left or right with a specified character.
}
\usage{
pad.string(x, len = max(nchar(x)), padchar = " ", right = T)
}
\arguments{
  \item{x}{character object }
  \item{len}{desired length of the returned strings}
  \item{padchar}{a single-character string.}
  \item{right}{if \code{TRUE} (the default), the strings in \code{x} are
  padded to the right, else they are padded to the left.}
}
\value{an object like \code{x}, but with the strings padded out to the
  desired length. Strings in \code{x} that are already \code{len} or more
  characters long are unaffected by the function.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{format}}}
\examples{
pad.string(c("aaa", "bbbbbb", "ccccccccccc"), len = 7, padchar = "X")
}
\keyword{character}
